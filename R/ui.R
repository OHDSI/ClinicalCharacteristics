# function that builds and aggregates ts
generateTableShell <- function(tableShell, executionSettings, buildOptions = NULL) {

  if (is.null(buildOptions)){
    buildOptions <- defaultTableShellBuildOptions()
  }

  # Step 0: user print specifying job info
  tableShell$printJobDetails()

  # Step 1: insert time windows
  tableShell$insertTimeWindows(executionSettings, buildOptions)

  # Step 2: make sql file for table shell run
  sql <- tableShell$buildTableShellSql(executionSettings, buildOptions)

  # Step 3: Execute them on dbms
  cli::cat_bullet(
    glue::glue_col("{yellow Executing shell sql}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  DatabaseConnector::executeSql(connection = executionSettings$getConnection(), sql = sql)

  # Step 4: Transform results (continuous => categorical; categorical => continuous)
  # tableShell$categorizeItems(executionSettings)
  # tableShell$scoreItems(executionSettings)

  # Step 5: Aggregate Results Table
  # step 5a: aggregate categorical results first
  cli::cat_bullet(
    glue::glue_col("{yellow Aggregating and formatting results}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  categoricalResults <- tableShell$aggregateTableShell(executionSettings, buildOptions, type = "categorical")
  #categoricalResultsFormatted <- tableShell$.labelResults(categoricalResultsRaw, type = "categorical")

  #step 5b: aggregate continuous results second
  continuousResults <- tableShell$aggregateTableShell(executionSettings, buildOptions, type = "continuous")
  #continuousResultsFormatted <- tableShell$.labelResults(continuousResultsRaw, type = "continuous")

  # keep dat Temp table for future use
  if (buildOptions$keepResultsTable) {
    # TODO create ctas to save temp table
  } else{
    #close on exit
    executionSettings$disconnect()
  }

  tableShellResults <- list(
    'categorical' = categoricalResults,
    'continuous' = continuousResults
  )

  return(tableShellResults)
}




# function that shows sql used to make table shell
reviewTableShellSql <- function(tableShell, executionSettings, buildOptions = NULL){

  if (is.null(buildOptions)){
    buildOptions <- defaultTableShellBuildOptions()
  }

  # make sql file for table shell run
  sql <- tableShell$buildTableShellSql(executionSettings, buildOptions)

  cli::cat_bullet(
    glue::glue("Opening Table Shell Query Sql in Monaco widget"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  mnc <- monaco::monaco(
    contents = sql,
    language = "sql",
    theme = "vs"
  )

  return(mnc)
}


# function that creates text summary of how table shells were created
buildTableShellReport <- function(tableShell) {

}

# function to save table shell results
saveTableShell <- function(result, outputPath) {

}

# creates reactable output of the tables shells
previewTableShell <- function(results, type) {

  if (type == "categorical") {
    cat_dat <- results$categorical

    res_tb <- reactable::reactable(
      data = cat_dat,
      columns = list(
        'cohortId' = reactable::colDef(name = "Cohort Id"),
        'cohortName' = reactable::colDef(name = "Cohort Name"),
        'categoryId' = reactable::colDef(name = "Category Id"),
        'categoryLabel' = reactable::colDef(name = "Category Name"),
        'timeId' = reactable::colDef(name = "Time Id"),
        'twLabel' = reactable::colDef(name = "Time Name"),
        'valueId' = reactable::colDef(name = "Value Id"),
        'name' = reactable::colDef(name = "Value Name"),
        'n' = reactable::colDef(
          name = "n", format = reactable::colFormat(separators = TRUE)
        ),
        'pct' = reactable::colDef(
          name = "pct", format = reactable::colFormat(percent = TRUE, digits = 2)
        )
      ),
      highlight = TRUE,
      bordered = TRUE,
      outlined = TRUE,
      resizable = TRUE,
      filterable = TRUE,
      searchable = TRUE
    )
  }

  if (type == "continuous") {

    cts_dat <- results$continuous

    res_tb <- reactable::reactable(
      data = cts_dat,
      columns = list(
        'cohortId' = reactable::colDef(name = "Cohort Id"),
        'cohortName' = reactable::colDef(name = "Cohort Name"),
        'categoryId' = reactable::colDef(name = "Category Id"),
        'categoryLabel' = reactable::colDef(name = "Category Name"),
        'timeId' = reactable::colDef(name = "Time Id"),
        'twLabel' = reactable::colDef(name = "Time Name"),
        'valueId' = reactable::colDef(name = "Value Id"),
        'n' = reactable::colDef(
          name = "n", format = reactable::colFormat(separators = TRUE)
        ),
        'mean' = reactable::colDef(
          name = "mean", format = reactable::colFormat(separators = TRUE, digits = 2)
        ),
        'sd' = reactable::colDef(
          name = "sd", format = reactable::colFormat(separators = TRUE, digits = 2)
        ),
        'min' = reactable::colDef(
          name = "min", format = reactable::colFormat(separators = TRUE)
        ),
        'p25' = reactable::colDef(
          name = "25th", format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        'median' = reactable::colDef(
          name = "median", format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        'p75' = reactable::colDef(
          name = "75th", format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        'max' = reactable::colDef(
          name = "max", format = reactable::colFormat(separators = TRUE)
        )
      ),
      highlight = TRUE,
      bordered = TRUE,
      outlined = TRUE,
      resizable = TRUE,
      filterable = TRUE,
      searchable = TRUE
    )
  }

  return(res_tb)
}
