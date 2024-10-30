# Query Sql Statements -----------------------------
# codesetQuerySql <- function(codesetTable) {
#   sql <- glue::glue("CREATE TABLE {codesetTable} (
#   codeset_id int NOT NULL,
#   concept_id bigint NOT NULL
# )
# ;")
# return(sql)
# }

conceptSetQuerySql <- function(conceptIds) {
  conceptIds <- paste(conceptIds, collapse = ", ")
  sql <- glue::glue("select concept_id from @vocabularyDatabaseSchema.CONCEPT where concept_id in ({conceptIds})")
  return(sql)
}


conceptSetDescendantsSql <- function(conceptIds) {
  conceptIds <- paste(conceptIds, collapse = ", ")
  sql <- glue::glue(
    "select c.concept_id
  from @vocabularyDatabaseSchema.CONCEPT c
  join @vocabularyDatabaseSchema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
  and ca.ancestor_concept_id in ({conceptIds})
  and c.invalid_reason is null"
  )
  return(sql)
}

conceptSetIncludeSql <- function(includeQuery) {
  sql <- glue::glue("select distinct I.concept_id
  FROM
  (
  {includeQuery}
  ) I")
  return(sql)
}


conceptSetExcludeSql <- function(excludeQuery) {
  sql <- glue::glue("LEFT JOIN
  (
  {excludeQuery}
  ) E ON I.concept_id = E.concept_id
  WHERE E.concept_id is null"
  )
  return(sql)
}

conceptSetMappedSql <- function(conceptSetQuery) {
  sql <- glue::glue("select distinct cr.concept_id_1 as concept_id
  FROM
  (
  {conceptSetQuery}
  ) C
  join @vocabularyDatabaseSchema.concept_relationship cr
    on C.concept_id = cr.concept_id_2 and cr.relationship_id = 'Maps to' and cr.invalid_reason IS NULL")
  return(sql)
}

# convert concept set to table -----------------
exp_to_table <- function(cs) {
  tb <- tibble::tibble(
    concept_id = purrr::map_int(cs@Expression, ~.x@Concept@concept_id),
    is_excluded = purrr::map_lgl(cs@Expression, ~.x@isExcluded),
    include_descendants = purrr::map_lgl(cs@Expression, ~.x@includeDescendants),
    include_mapped = purrr::map_lgl(cs@Expression, ~.x@includeMapped)
  )
  return(tb)
}

# Build Queries ---------------------
include_concept_query <- function(concepts_to_include) {
  concept_set_query <- conceptSetQuerySql(conceptIds = concepts_to_include$concept_id)

  if (any(concepts_to_include$include_descendants)) {
    include_decendant_ids <- concepts_to_include |> dplyr::filter(include_descendants) |> dplyr::pull(concept_id)
    include_descendants_query <- conceptSetDescendantsSql(include_decendant_ids)
    concept_set_query <- glue::glue("{concept_set_query} \n UNION \n {include_descendants_query}")
  }

  final_query <- conceptSetIncludeSql(includeQuery = concept_set_query)

  return(final_query)
}

mapped_concept_query <- function(mapped_concepts) {

  concept_set_query <- conceptSetQuerySql(conceptIds = mapped_concepts$concept_id)

  if (any(mapped_concepts$include_descendants)) {
    include_decendant_ids <- mapped_concepts |> dplyr::filter(include_descendants) |> dplyr::pull(concept_id)
    include_descendants_query <- conceptSetDescendantsSql(include_decendant_ids)
    concept_set_query <- glue::glue("{concept_set_query} \n UNION \n {include_descendants_query}")
  }

  final_query <- conceptSetMappedSql(conceptSetQuery = concept_set_query)
  return(final_query)
}

exclude_concept_query <- function(concepts_to_exclude) {

  concept_set_query <- conceptSetQuerySql(conceptIds = concepts_to_exclude$concept_id)

  if (any(concepts_to_exclude$include_descendants)) {
    include_decendant_ids <- concepts_to_exclude |> dplyr::filter(include_descendants) |> dplyr::pull(concept_id)
    include_descendants_query <- conceptSetDescendantsSql(include_decendant_ids)
    concept_set_query <- glue::glue("{concept_set_query} \n UNION \n {include_descendants_query}")
  }

  final_query <- conceptSetExcludeSql(excludeQuery = concept_set_query)
  return(final_query)
}


# Put it all together ---------------------------
#TODO add mapped part to builders
build_codeset_query <- function(tb, id){

  # get concepts to exclude
  concepts_to_exclude <- tb |> dplyr::filter(is_excluded)
  # get concepts to include
  concepts_to_include <-  tb |> dplyr::filter(!is_excluded)

  concept_set_query <- include_concept_query(concepts_to_include)

  if (nrow(concepts_to_exclude) > 0) {
    exclude_query <- exclude_concept_query(concepts_to_exclude = concepts_to_exclude)
  } else(
    exclude_query = ""
  )

  codeset_query <- glue::glue("
  SELECT {id} as codeset_id, c.concept_id FROM
  (
  {concept_set_query}
  {exclude_query}
  ) C")
  return(codeset_query)
}

.bindCodesetQueries <- function(conceptSet, codesetTable) {

  #make ids for concept sets
  ids <- seq_along(conceptSet)
  #turn into list of tables
  ll <- purrr::map2(conceptSet, ids, ~exp_to_table(.x) |> build_codeset_query(id = .y))
  set <- paste(ll, collapse = "\nUNION ALL\n")
  final_query <- glue::glue(
    "-- Create Codesets
    CREATE TABLE {codesetTable} (
        codeset_id int NOT NULL,
        concept_id bigint NOT NULL
    )
    ;

    INSERT INTO {codesetTable} (codeset_id, concept_id)
    {set}
    ;"
    )

  return(final_query)
}

pluck_concept_sets <- function(clinChar) {
  es <- clinChar@extractSettings
  clin_class <- purrr::map_chr(es, ~class(.x))
  idx <- which(clin_class %in% c("presenceChar", "countChar", "costChar", "timeToChar"))
  dd <- es[idx]
  return(dd)
}

codeset_key <- function(clinChar) {
  dmChar <- pluck_concept_sets(clinChar)
  cs_tbl <- purrr::map(dmChar, ~.x@conceptSets) |>
    purrr::compact() |>
    purrr::flatten()

  cs_id <- !duplicated(purrr::map_chr(cs_tbl, ~.x@id))
  cs_tbl2 <- cs_tbl[cs_id]


  return(cs_tbl2)
}



get_hash_order <- function(clinChar) {

  cs_tbl <- codeset_key(clinChar)
  hash_order <- purrr::map_chr(cs_tbl, ~.x@id)
  return(hash_order)
}

hash_to_id <- function(x, clinChar) {
  # get hash
  hash_order <- get_hash_order(clinChar)

  current_hash_ids <- purrr::map_chr(x@conceptSets, ~.x@id)
  query_ids <- which(hash_order %in% current_hash_ids)
  x@tempTables$codeset <- query_ids
  return(x)
}

infuse_codset_id <- function(clinChar) {

  for (i in seq_along(clinChar@extractSettings)) {
    classChar <- class(clinChar@extractSettings[[i]])
    if (classChar %in% c("presenceChar", "timeToChar", "countChar", "costChar")) {
      clinChar@extractSettings[[i]] <-
        hash_to_id(clinChar@extractSettings[[i]], clinChar)
    } else{
      next
    }
  }
  return(clinChar)

}


