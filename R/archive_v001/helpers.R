trunc_drop <- function(connection, table) {
  sql <- glue::glue("TRUNCATE TABLE {table}; DROP TABLE {table};") |>
    SqlRender::translate(targetDialect = connection@dbms)
  verboseExecute(
    connection = connection,
    sql = sql,
    task = glue::glue("Truncate and Drop {table}"))
}

getCohorts <- function(connection,
                            workDatabaseSchema,
                            cohortTable,
                            targetIds,
                            stow = FALSE) {

  dbms <- connection@dbms

  #temp table for snowflake
  if (dbms == "snowflake") {
    tempTable <- glue::glue("{workDatabaseSchema}.target_tmp")
  } else {
    tempTable <- "#target"
  }

  sql <- glue::glue(
    "SELECT * INTO {tempTable} FROM {workDatabaseSchema}.{cohortTable} WHERE cohort_definition_id IN ({targetIds});"
  ) |>
    SqlRender::translate(
      targetDialect = dbms
    )

  verboseExecute(connection = connection, sql = sql, task = "Get Target Cohort")

  if (stow) {
    tbl <- stowTable(connection = connection, tempTable = tempTable)
    trunc_drop(connection = connection, table = tempTable)
    return(tbl)
  }

}
