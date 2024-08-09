# function to pluck out the conceptSet Line Items
.pluckConceptSetLineItems <- function(tableShell) {
  lineClasses <- purrr::map_chr(tableShell, ~class(.x)[1])
  csLineItems <- tableShell[which(lineClasses == "ConceptSetDefinition")]
  return(csLineItems)
}

.buildCodesetQueries <- function(tableShell) {

  #temporary change with class
  codesetTable <- "#Codeset"

  # get concept set line items
  csLineItems <- .pluckConceptSetLineItems(tableShell)
  # retrieve each concept set from the line items and flatten
  csCapr <- purrr::map(
    csLineItems,
    ~.x$grabConceptSets()
  ) |>
    purrr::compact() |>
    purrr::flatten()
  # remove duplicated ids
  cs_id <- !duplicated(purrr::map_chr(cs_tbl, ~.x@id))
  cs_tbl2 <- cs_tbl[cs_id]

  # change function name to .camel
  cs_query <- bind_codeset_queries(cs_tbl2, codesetTable = codesetTable)
  return(cs_query)

}


# function to build Concept Set Meta table
.conceptSetMeta <- function(tableShell) {

  # get concept set line items
  csLineItems <- .pluckConceptSetLineItems(tableShell)

  # get time windows
  tw <- purrr::map_dfr(csLineItems, ~.x$getTimeWindows()) |>
    dplyr::distinct() |>
    dplyr::mutate(
      id = dplyr::row_number(), .before = 1
    )

  # get concept sets
  cs <- purrr::map_dfr(csLineItems, ~.x$getConceptSets()) |>
    dplyr::distinct() |>
    dplyr::mutate(
      id = dplyr::row_number(), .before = 1
    )

  # get domains
  dm_ii <- purrr::map_chr(csLineItems, ~.x$getDomain()) |>
    unique()
  dm <- tibble::tibble(
    'id' = 1:length(dm_ii),
    'domain' = dm_ii
  )



}
