stowTable <- function(connection, clinChar) {

  dbms <- connection@dbms
  tempTable <- clinChar@stowSettings@dataTable
  # create sql
  sql <- glue::glue(
    "SELECT * FROM {tempTable} order by cohort_id, subject_id, category_id, time_id, value_id;"
  ) |>
    SqlRender::translate(
      targetDialect = dbms
    )
  tmpFile <- clinChar@stowSettings@dataLoc
  # write table to parquet
  arrow::write_dataset(
    dataset = DatabaseConnector::querySql(connection = connection, sql = sql),
    path = tmpFile,
    format = clinChar@stowSettings@format
  )

  cli::cat_bullet(
    glue::glue("Stowed {crayon::green(tempTable)}: {crayon::cyan(tmpFile)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  invisible(clinChar)
}

stowCount <- function(connection, clinChar) {

  dbms <- connection@dbms
  cohortIds <- paste(clinChar@targetCohort@id, collapse = ",")
  workDatabaseSchema <- clinChar@executionSettings@workDatabaseSchema
  cohortTable <- clinChar@executionSettings@cohortTable
  # create sql
  sql <- glue::glue(
    "WITH T1 AS (
     SELECT *
     FROM {workDatabaseSchema}.{cohortTable}
     WHERE cohort_definition_id IN ({cohortIds})
    )
    SELECT COHORT_DEFINITION_ID AS COHORT_ID, COUNT(COHORT_DEFINITION_ID) AS total
    FROM T1 GROUP BY COHORT_DEFINITION_ID;"
  ) |>
    SqlRender::translate(
      targetDialect = dbms
    )
  tmpFile <- clinChar@stowSettings@countLoc
  # write table to parquet
  arrow::write_dataset(
    dataset = DatabaseConnector::querySql(connection = connection, sql = sql),
    path = tmpFile,
    format = clinChar@stowSettings@format
  )

  cli::cat_bullet(
    glue::glue("Stowed {crayon::green('Total Counts')}: {crayon::cyan(tmpFile)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  invisible(clinChar)
}

#' Function that retrieves the data table generated from the clinChar
#' @param clinChar the clinChar object describing the characterization
#' @param category_id the slot number of the char object in the extractSettings
#' @return A subset of the data table as an arrow object. To get the table you must
#' pipe `dplyr::collect`
#' @export
retrieveTable <- function(clinChar, category_id = NULL) {
  loc <- clinChar@stowSettings@dataLoc
  ff <- clinChar@stowSettings@format
  dd <- arrow::open_dataset(loc, format = ff) |>
    dplyr::rename_with(tolower)

  if (!is.null(category_id)) {
    dd <- dd |>
      dplyr::filter(
        category_id %in% !!category_id
      )
  }

  return(dd)
}

#' Function that retrieves the count table generated from the clinChar
#' @param clinChar the clinChar object describing the characterization
#' @return A table providing the person count of the cohorts as an arrow object. To get the table you must
#' pipe `dplyr::collect`
#' @export
retrieveCount <- function(clinChar) {
  loc <- clinChar@stowSettings@countLoc
  ff <- clinChar@stowSettings@format
  dd <- arrow::open_dataset(loc, format = ff) |>
    dplyr::rename_with(tolower)
  return(dd)
}
