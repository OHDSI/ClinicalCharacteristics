targetCohorts <- list(
  id = 3,
  name = "ipf"
)

library(Capr)
cs1 <- list(
  't2d' = cs(descendants(201826), name = "t2d"),
  'ckd' = cs(descendants(46271022), name = "ckd")
)

cs2 <- list(
  'pirfenidone' = cs(descendants(45775206), name = "pirfenidone"),
  'nintedanib' = cs(descendants(45775396), name = "nintedanib")
)

# testing pattern

ll <- list(
  'criteria' = list(
    'orderId' = 1,
    'charType' = list(type = "presence", op = "at_least", occurrences = 1),
    'lineItems' = list( #ConceptSet
      'conceptSets' = cs1,
      'domain' = "condition_occurrence",
      'sourceConcepts' = NULL,
      'typeConcepts' = NULL,
      'visitOccurrenceConcepts' = NULL
    ),
    'timeWindows' = list(c(-365, -1))
  ),
  'criteria' = list(
    'orderId' = 2,
    'charType' = list(type = "presence", op = "at_least", occurrences = 1),
    'lineItems' = list( #ConceptSet
      'conceptSets' = cs2,
      'domain' = "drug_exposure",
      'sourceConcepts' = NULL,
      'typeConcepts' = NULL,
      'visitOccurrenceConcepts' = NULL
    ),
    'timeWindows' = list(c(0, 90), c(0, 365))
  ),
  'criteria' = list(
    'orderId' = 3,
    'charType' = list(type = "count"),
    'lineItems' = list( #ConceptSet
      'conceptSets' = cs1,
      'domain' = "condition_occurrence",
      'sourceConcepts' = NULL,
      'typeConcepts' = NULL,
      'visitOccurrenceConcepts' = NULL
    ),
    'timeWindows' = list(c(0, 90), c(0, 365))
  )
)



# Step 1 get all unique time window combinations

# function to extract time window bounds
.pluckTimeWindows <- function(tw) {
  tw_a <- purrr::map_int(tw, ~.x[1])
  tw_b <- purrr::map_int(tw, ~.x[2])
  tb <- tibble::tibble(
    min = tw_a,
    max = tw_b
  )
  return(tb)
}

# extract time windows and make a tibble
twRef <- purrr::map_dfr(ll, ~.pluckTimeWindows(.x$timeWindows)) |>
  dplyr::distinct() |>
  dplyr::mutate(
    id = dplyr::row_number(),
    .before = 1
  )

# Step 2 get all unique concept sets

.pluckConceptSets <- function(ll) {
  cs_tbl <- purrr::map(
    ll,
    ~.x$lineItems$conceptSets
  ) |>
    purrr::compact() |>
    purrr::flatten()

  cs_id <- !duplicated(purrr::map_chr(cs_tbl, ~.x@id))
  cs_tbl2 <- cs_tbl[cs_id]
  return(cs_tbl2)
}

# make list of concept sets to make query
csList <- .pluckConceptSets(ll)
cs_query <- bind_codeset_queries(csList, codesetTable = "#Codeset")

# function toget cs reference table
.csRefTable <- function(csList) {
  csHash <- csList |>
    purrr::map_chr(~.x@id)

  # # make key for cs use
  csTbl <- tibble::tibble(
    'id' = 1:length(csHash),
    'name' = names(csHash),
    'hash' = unname(csHash)
  )
  return(csTbl)
}

csRef <- .csRefTable(csList)

# domain reference table
.dmRefTable <- function(ll) {
  dm <- purrr::map_chr(ll, ~.x$lineItems$domain) |>
    unname() |>
    unique()

  tb <- tibble::tibble(
    'id' = 1:length(dm),
    'domain' = dm
  )
  return(tb)

}

dmRef <- .dmRefTable(ll)

# char reference table
.charRefTable <- function(ll) {
  char <- purrr::map_chr(ll, ~.x$charType$type) |>
    unname() |>
    unique()

  tb <- tibble::tibble(
    'id' = 1:length(char),
    'charType' = char
  )
  return(tb)
}
charRef <- .charRefTable(ll)
# Step 3: Make meta table

.csMeta <- function(ii, twRef, csRef, dmRef, charRef) {

  # get order id
  idx <- ii$orderId

  # get time windows ids
  ii_tw <- .pluckTimeWindows(ii$timeWindows) |>
    dplyr::inner_join(
      twRef, by = c("min", "max")
    ) |>
      dplyr::select(id, min, max)

  # get cs ids
  csHash <- purrr::map_chr(ii$lineItems$conceptSets, ~.x@id) |>
    unname()
  ii_cs <- csRef |>
    dplyr::filter(hash %in% csHash)

  # get domain ids
  ii_dm <- dmRef |>
    dplyr::filter(domain %in% ii$lineItems$domain)

  # get charTypes
  ii_char <- charRef |>
    dplyr::filter(charType %in% ii$charType$type)

  # make row of meta data
  row <- tibble::tibble(
    'orderId' = idx,
    'twId' = ii_tw$id |> glue::glue_collapse(sep = ", "),
    'csId' = ii_cs$id |> glue::glue_collapse(sep = ", "),
    'domain' = ii_dm$domain,
    'char' = ii_char$charType
  )

  return(row)

}

metaTb <- purrr::map_dfr(ll, ~.csMeta(.x, twRef = twRef, csRef = csRef, dmRef = dmRef, charRef = charRef))

# Step 4: merge domain, concept set, tw combos
tst <- metaTb |>
  dplyr::select(csId, domain) |>
  dplyr::distinct()

new_twId <- character(length = nrow(tst))
for (i in 1:nrow(tst)) {

  tmp <- tst[i, ]
  new_twId[i] <- metaTb |>
    dplyr::select(-c(orderId, char)) |>
    dplyr::inner_join(
      tmp, by = c("csId", "domain")
    ) |>
    dplyr::pull(twId) |>
    glue::glue_collapse(sep = ", ")

}


cs_query_ref_table <- tibble::tibble(
  'twId' = new_twId,
  'csId' = tst$csId,
  'domain' = tst$domain
) |>
  dplyr::group_by(
    domain
  ) |>
  dplyr::mutate(
    'tempTableName' = glue::glue("#{domain}_{dplyr::row_number()}")
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    idx = dplyr::row_number(),
    .before = 1
  )
cs_query_ref_table


codesetIds <- cs_query_ref_table$csId[1]
timeIds <- cs_query_ref_table$twId[1]
csTempTableName <- cs_query_ref_table$tempTableName[1]
domain <- cs_query_ref_table$domain[1]
domain_trans <- domain_translate(domain)
sql_tst <- readr::read_file(
  file = fs::path(here::here(), "inst/sql/new2/conceptSetQuery.sql")
) |> glue::glue()
cat(sql_tst)

