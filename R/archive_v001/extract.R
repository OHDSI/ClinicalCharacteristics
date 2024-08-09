verboseExecute <- function(connection, sql, task, printSql = FALSE) {

  cli::cat_bullet(
    glue::glue("Executing Task: {crayon::magenta(task)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  DatabaseConnector::executeSql(connection = connection, sql = sql)

  if (printSql) {
    cli::cat_bullet(
      glue::glue("Task {crayon::magenta(task)} done using:"),
      bullet = "pointer",
      bullet_col = "yellow"
    )
    cli::cat_line(sql, col = "#FFC000")
  }

  invisible(sql)
}

trunc_drop <- function(connection, table) {
  sql <- glue::glue("TRUNCATE TABLE {table}; DROP TABLE {table};") |>
    SqlRender::translate(targetDialect = connection@dbms)
  verboseExecute(
    connection = connection,
    sql = sql,
    task = glue::glue("Truncate and Drop {table}"))
}

stowTable <- function(connection, tempTable) {

  dbms <- connection@dbms
  # create sql
  sql <- glue::glue(
    "SELECT * FROM {tempTable};"
  ) |>
    SqlRender::translate(
      targetDialect = dbms
    )
  # make file
  tmpFile <- fs::file_temp(ext = "parquet")

  # write table to parquet
  arrow::write_dataset(
    dataset = DatabaseConnector::querySql(connection = connection, sql = sql),
    path = tmpFile,
    format = "parquet"
  )

  tbl <- list(
    'item' = tempTable,
    'loc' = tmpFile
  )
  class(tbl) <- "stowedTable"

  cli::cat_bullet(
    glue::glue("Stowed {crayon::green(tempTable)}: {crayon::cyan(tmpFile)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  return(tbl)
}




getTargetCohort <- function(connection,
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
