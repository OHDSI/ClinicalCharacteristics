#' Function to make a time table for the time windows slot
#' @param time_a the time bound on the left side
#' @param time_b the time bound on the right side
#' @export
makeTimeTable <- function(time_a, time_b) {
  if (length(time_a) != length(time_b)) {
    stop("time_a and time_b need to be of same length")
  }
  #time_id <- seq_along(time_a)
  tb <- tibble::tibble(
    #time_id,
    time_a,
    time_b
  )
  return(tb)
}


time_key <- function(clinChar) {
  dmChar <- pluck_domain_char(clinChar)
  time_tbl <- purrr::map_dfr(dmChar, ~.x@time) |>
    dplyr::distinct() |>
    dplyr::mutate(
      time_id = dplyr::row_number(), .before = 1
    )
  return(time_tbl)
}

insert_time_table <- function(connection, clinChar) {

  time_tbl <- time_key(clinChar)

  cli::cat_bullet(
    glue::glue("Insert time window tables for characterization"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  if(connection@dbms == "snowflake") {
    DatabaseConnector::insertTable(
      connection = connection,
      tableName = clinChar@executionSettings@timeWindowTable,
      tempEmulationSchema = clinChar@executionSettings@tempEmulationSchema,
      data = time_tbl,
      tempTable = TRUE
    )
  } else{
    DatabaseConnector::insertTable(
      connection = connection,
      tableName = clinChar@executionSettings@timeWindowTable,
      data = time_tbl,
      tempTable = TRUE
    )
  }

  invisible(time_tbl)
}
