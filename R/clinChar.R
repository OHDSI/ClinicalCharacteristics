# ClinChar --------------------------

setClass("ClinChar",
         slots = c(targetCohort = "targetCohort",
                   executionSettings = "executionSettings",
                   #stowSettings = "stowSettings",
                   extractSettings = "list"),
         prototype = list(
           targetCohort = new("targetCohort"),
           executionSettings = new("executionSettings"),
           #stowSettings = new("stowSettings"),
           extractSettings = list()
         )
)

setGeneric("check_dbms", function(x)  standardGeneric("check_dbms"))
setMethod("check_dbms", "ClinChar", function(x){
  x@executionSettings@dbms
})


#' Create a clinChar object
#' @description
#' This function creates a clinChar object used to run the characterization
#' @param targetCohortIds the cohort ids from the cohort table needed for description
#' @param targetCohortNames the names of the cohorts corresponding to these ids
#' @param dbms the dbms where the OMOP data sits
#' @param cdmDatabaseSchema the database schema string specifying where the cdm sits
#' @param vocabularyDatabaseSchema the database schema string specifying where the vocabulary sits
#' @param workDatabaseSchema the database schema string specifying where the cohor table sits
#' @param tempEmulationSchema schema to handle temp tables for oracle and snowflake
#' @param cohortTable the table where the cohorts are
#' @return makes a clinChar object
#' @export
makeClinChar <- function(targetCohortIds,
                         targetCohortNames = NULL,
                         dbms,
                         database,
                         cdmDatabaseSchema,
                         vocabularyDatabaseSchema = cdmDatabaseSchema,
                         workDatabaseSchema,
                         tempEmulationSchema = workDatabaseSchema,
                         cohortTable,
                         datTableName = "dat") {
  # make new clin char object
  clinChar <- new("ClinChar")
  # add target ids
  clinChar@targetCohort@id <- as.integer(targetCohortIds)

  # add target cohort names
  if (is.null(targetCohortNames)) {
    clinChar@targetCohort@name <- glue::glue("cohort_{targetCohortIds}")
  } else {
    clinChar@targetCohort@name <- targetCohortNames
  }

  if (dbms == "snowflake") {
    clinChar@executionSettings@tempEmulationSchema <- tempEmulationSchema
    clinChar@executionSettings@dataTable <- glue::glue("{workDatabaseSchema}.{datTableName}_tmp")
    # clinChar@targetCohort@tempTable <- glue::glue("{workDatabaseSchema}.target_tmp")
    # clinChar@executionSettings@timeWindowTable <- glue::glue("{workDatabaseSchema}.tw_tmp")
    # clinChar@executionSettings@codesetTable <- glue::glue("{workDatabaseSchema}.codeset_tmp")
  } else{
    clinChar@executionSettings@tempEmulationSchema <- NA_character_
    clinChar@executionSettings@dataTable <- glue::glue("#{datTableName}")
  }

  # add execution settings
  clinChar@executionSettings@dbms <- dbms
  clinChar@executionSettings@database <- database
  clinChar@executionSettings@cdmDatabaseSchema <- cdmDatabaseSchema
  clinChar@executionSettings@vocabularyDatabaseSchema <- vocabularyDatabaseSchema
  clinChar@executionSettings@workDatabaseSchema <- workDatabaseSchema
  clinChar@executionSettings@cohortTable <- cohortTable


  return(clinChar)
}

make_dat_table <- function() {
  sql <- glue::glue("
  -- Init Data table
  CREATE TABLE {{dataTable}} (
  cohort_id int NOT NULL,
  subject_id bigint NOT NULL,
  category_id int NOT NULL,
  time_id int NOT NULL,
  value_id bigint NOT NULL,
  value int NOT NULL
)
;")
return(sql)
}

# Build Query -----------------------
setGeneric("build_query", function(x)  standardGeneric("build_query"))
setMethod("build_query", "ClinChar", function(x){

  # make params to paste into sql
  cdmDatabaseSchema <- x@executionSettings@cdmDatabaseSchema
  workDatabaseSchema <- x@executionSettings@workDatabaseSchema
  vocabularyDatabaseSchema <- x@executionSettings@vocabularyDatabaseSchema
  cohortTable <- x@executionSettings@cohortTable
  dataTable <- x@executionSettings@dataTable
  targetTable <- x@targetCohort@tempTable
  timeWindowTable <- x@executionSettings@timeWindowTable
  codesetTable <- x@executionSettings@codesetTable

  cs_tbl <- codeset_key(x)
  if (length(cs_tbl) > 0) {
    cs_query <- bind_codeset_queries(cs_tbl, codesetTable = codesetTable)
    drop_temp_tb_tw_cs <- drop_temp_tables(x@executionSettings)
  } else {
    cs_query <- ""
    drop_temp_tb_tw_cs <- trunc_drop(timeWindowTable)
  }

  # collect all sql for char run
  collect_sql <- glue::glue(
    make_dat_table(), # make the dat table
    as_sql(x@targetCohort), # create target cohort
    cs_query,
    paste(purrr::map_chr(x@extractSettings, ~as_sql(.x)), collapse = "\n\n"), # run covars
    "\n-- Drop Temp Tables\n",
    drop_temp_tables(x@targetCohort),
    drop_temp_tb_tw_cs,
    drop_domain_temp(x), # drop tables
    .sep = "\n\n"
  )

  # translate if snowflake
  dbms <- x@executionSettings@dbms
  if (dbms == "snowflake") {
    collect_sql <- SqlRender::translate(
      sql = collect_sql,
      targetDialect = dbms,
      tempEmulationSchema = x@executionSettings@tempEmulationSchema
      )
  } else {
    collect_sql <- SqlRender::translate(
      sql = collect_sql,
      targetDialect = dbms
    )
  }

  return(collect_sql)

})

clinCharJobDetails <- function(clinChar) {
  charType <- purrr::map_chr(clinChar@extractSettings, ~class(.x))
  domains <- purrr::map_chr(clinChar@extractSettings, ~.x@domain)
  orderIds <- as.character(purrr::map_int(clinChar@extractSettings, ~.x@orderId))
  database <- clinChar@executionSettings@database
  cli::cat_bullet(
    crayon::yellow("Job Details"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  cli::cat_line(
    glue::glue("  ClinChar run on {crayon::green(database)}")
  )
  cli::cat_line(
    glue::glue("  ClinChar tasks to do:")
  )
  cli::cat_line(
    glue::glue("\t({crayon::yellow(orderIds)}) {charType}/{domains}")
  )

  invisible(charType)
}

# obj checks ----------------------

check_score <- function(x) {
  score_slot <- "score" %in% methods::slotNames(x)
  if (score_slot) {
    check <- !is.null(x@score)
  } else{
    check <- FALSE
  }
  return(check)
}


is_vd_cts <- function(clinChar) {
  es <- clinChar@extractSettings
  cln <- purrr::map_chr(es, ~methods::is(.x))
  vd_char <- c("visitDetailChar")
  ids <- which(cln %in% vd_char)
  if (length(ids) == 0) {
    check <- FALSE
  } else {
    check <- es[[ids]]@count
  }
  return(check)
}

get_cts_ids <- function(clinChar) {
  es <- clinChar@extractSettings
  cln <- purrr::map_chr(es, ~methods::is(.x))
  cts_char <- c("ageChar", "countChar", "costChar", "timeToChar",
                "timeInChar", "labChar")
  if (is_vd_cts(clinChar)) {
    cts_char <- c(cts_char, "visitDetailChar")
  }
  ids <- which(cln %in% cts_char)

  #add cat that are scored
  to_cts <- purrr::map_lgl(es, ~check_score(.x)) |> which()
  if (length(to_cts) > 0) {
    to_cts <- (to_cts * 1000) + 1
    ids <- c(ids, to_cts)
  }

  return(ids)
}

check_categorize <- function(x) {
  score_slot <- "categorize" %in% methods::slotNames(x)
  if (score_slot) {
    check <- !is.null(x@categorize)
  } else{
    check <- FALSE
  }
  return(check)
}


get_cat_ids <- function(clinChar) {
  es <- clinChar@extractSettings
  cln <- purrr::map_chr(es, ~methods::is(.x))
  cat_char <- c("demoConceptChar", "presenceChar", "locationChar", "yearChar")
  if (!is_vd_cts(clinChar)) {
    cat_char <- c(cat_char, "visitDetailChar")
  }

  ids <- which(cln %in% cat_char)

  #add cts that are categorized
  to_cat <- purrr::map_lgl(es, ~check_categorize(.x)) |> which()
  if (length(to_cat) > 0) {
    to_cat <- (to_cat * 1000) + 1
    ids <- c(ids, to_cat)
  }

  return(ids)
}

# Summarize ----------------

summarize_continuous <- function(connection, dataTable, cts_ids) {

  cts_ids <- cts_ids |>
    paste(collapse = ", ")

  sql <- glue::glue(
    "WITH T1 AS (
    SELECT * FROM {dataTable} WHERE category_id IN ({cts_ids})
  )
  SELECT
  cohort_id, category_id, time_id, value_id,
    COUNT(subject_id) AS N,
    SUM(value) As occ_cnt,
    STDDEV(value) AS sd,
    min(value) AS min,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY value) as p25,
    PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY value) as median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY value) as p75,
    max(value) AS max
  FROM T1
  GROUP BY cohort_id, category_id, time_id, value_id
  ;") |>
    SqlRender::translate(targetDialect = connection@dbms)

  cts_sum <- DatabaseConnector::querySql(connection, sql = sql) |>
    tibble::as_tibble() |>
    dplyr::rename_with(tolower) |>
    dplyr::arrange(cohort_id, category_id, time_id, value_id)

  return(cts_sum)

}

summarize_categorical<- function(connection, dataTable, workDatabaseSchema, cohortTable, cat_ids) {

  cat_ids <- cat_ids |>
    paste(collapse = ", ")

  sql <- glue::glue(
    "WITH T1 AS (
    SELECT * FROM {dataTable} WHERE category_id IN ({cat_ids})
  ),
  T2 AS (
    SELECT cohort_definition_id AS cohort_id, COUNT(subject_id) AS tot
    FROM {workDatabaseSchema}.{cohortTable}
    GROUP BY cohort_definition_id
  ),
  T3 AS (
    SELECT
      cohort_id, category_id, time_id, value_id,
      COUNT(subject_id) AS n
    FROM T1
    GROUP BY cohort_id, category_id, time_id, value_id
  ),
  T4 AS (
    SELECT
      a.cohort_id, a.category_id, a.time_id, a.value_id, a.n,
      ((1.0 * a.n) / (1.0 * b.tot))  AS pct
    FROM T3 a
    JOIN T2 b ON a.cohort_id = b.cohort_id
  )
  SELECT
  cohort_id, category_id, time_id, value_id, n, pct
  FROM T4
  ;") |>
    SqlRender::translate(targetDialect = connection@dbms)

  cat_sum <- DatabaseConnector::querySql(connection, sql = sql) |>
    tibble::as_tibble() |>
    dplyr::rename_with(tolower) |>
    dplyr::arrange(cohort_id, category_id, time_id, value_id)

  return(cat_sum)

}

# Labels ------

cohort_key <- function(clinChar) {
  cohortKey <- tibble::tibble(
    cohort_id = as.numeric(clinChar@targetCohort@id),
    cohort_name = clinChar@targetCohort@name
  )
  return(cohortKey)
}

set_time_labels <- function(clinChar) {

  timeKey <- time_key(clinChar)

  if (nrow(timeKey) == 0) {
    timeKey <- tibble::tibble(
      time_id = -999,
      time_name = "Static from Index"
    )
  } else {
    timeKey <- timeKey |>
      dplyr::mutate(
        time_name = glue::glue("{time_a}d:{time_b}d")
      ) |>
      dplyr::select(
        -c(time_a, time_b)
      ) |>
      tibble::add_row(
        time_id = -999,
        time_name = "Static from Index"
      )
  }
  return(timeKey)
}

set_labels <- function(clinChar) {

  char_lbl <- purrr::map_dfr(clinChar@extractSettings, ~get_labels(.x)) |>
    dplyr::left_join(
      set_time_labels(clinChar), by = c("time_name")
    ) |>
    tidyr::expand_grid(cohort_key(clinChar)) |>
    dplyr::select(
      cohort_id, cohort_name, order_id, category_id, category_name,
      time_id, time_name, value_id, value_name
    )
  return(char_lbl)
}


check_and_drop_dat <- function(connection, clinChar) {

  datTable <- clinChar@executionSettings@dataTable
  sql <- glue::glue("DROP TABLE IF EXISTS {datTable}")

  cli::cat_bullet(
    glue::glue("Drop {crayon::green(datTable)} from db if it already exists"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  DatabaseConnector::executeSql(
    connection = connection,
    sql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  invisible(datTable)

}

# Summarize steps -----------------

score_them <- function(clinChar, connection) {

  es <- clinChar@extractSettings
  to_cts <- purrr::map_lgl(es, ~check_score(.x)) |> which()

  ## Do score First
  if (length(to_cts) > 0) {
    cli::cat_bullet(
      glue::glue("Score covariates {crayon::yellow('(categorical => continuous)')}"),
      bullet = "pointer",
      bullet_col = "yellow"
    )
    for (i in to_cts) {
      ## extract score obj
      scoreObj <- es[[i]]@score
      cli::cat_bullet(
        glue::glue("Build {crayon::magenta(scoreObj@name)} Score"),
        bullet = "pointer",
        bullet_col = "yellow"
      )

      # Step 1: insert weights temp table

      ## deal with temp tables if in snowflake
      if(connection@dbms == "snowflake") {
        scratchSchema <- clinChar@executionSettings@workDatabaseSchema
        scoreTbl <- glue::glue("{scratchSchema}.{scoreObj@name}")
        tempTabToggle <- FALSE
      } else{
        scoreTbl <- glue::glue("#{scoreObj@name}")
        tempTabToggle <- TRUE
      }
      cli::cat_line(glue::glue("\t - Insert weights table to dbms as {crayon::green(scoreTbl)}"))
      ## insert temp weights table
      DatabaseConnector::insertTable(
        connection = connection,
        tableName = scoreTbl,
        data = scoreObj@weights,
        tempTable = tempTabToggle
      )

      # Step 2: Make score cov and add back to dataTable
      cli::cat_line(glue::glue("\t - Build score and add to {crayon::green(clinChar@executionSettings@dataTable)}"))
      score_value(
        connection = connection,
        dataTable = clinChar@executionSettings@dataTable,
        scoreTable = scoreTbl,
        workDatabaseSchema = clinChar@executionSettings@workDatabaseSchema,
        scoreId = i,
        scoreSql = scoreObj@sql
      )
      cli::cat_line(glue::glue("\t - New category_id for score: {crayon::green((i * 1000) + 1)}"))
      # go to next score
    }
    #end all scores
  }
  invisible(to_cts)
}

categorize_them <- function(clinChar, connection) {

  es <- clinChar@extractSettings
  to_cat <- purrr::map_lgl(es, ~check_categorize(.x)) |> which()

  ## Do Categories Second
  if (length(to_cat) > 0) {
    cli::cat_bullet(
      glue::glue("Categorize covariates {crayon::yellow('(continuous => categorical)')}"),
      bullet = "pointer",
      bullet_col = "yellow"
    )
    for (i in to_cat) {
      ## extract breaks obj
      breaksObj <- es[[i]]@categorize
      cli::cat_bullet(
        glue::glue("Categorize using {crayon::magenta(breaksObj@name)} breaks"),
        bullet = "pointer",
        bullet_col = "yellow"
      )
      breaksKey <- breaksObj@breaks |>
        dplyr::select(-c(grp))

      # Step 1: insert breaks temp table

      ## deal with temp tables if in snowflake
      if(connection@dbms == "snowflake") {
        scratchSchema <- clinChar@executionSettings@workDatabaseSchema
        breaksTbl <- glue::glue("{scratchSchema}.{breaksObj@name}")
        tempTabToggle <- FALSE
      } else{
        breaksTbl <- glue::glue("#{breaksObj@name}")
        tempTabToggle <- TRUE
      }
      cli::cat_line(glue::glue("\t - Insert breaksKey to dbms as {crayon::green(breaksTbl)}"))
      ## insert temp weights table
      DatabaseConnector::insertTable(
        connection = connection,
        tableName = breaksTbl,
        data = breaksKey,
        tempTable = tempTabToggle
      )

      # Step 2: Make score cov and add back to dataTable
      cli::cat_line(glue::glue("\t - Categorize and add to {crayon::green(clinChar@executionSettings@dataTable)}"))
      year <- grepl("year", breaksObj@name)
      categorize_value(
        connection = connection,
        dataTable = clinChar@executionSettings@dataTable,
        breaksTable = breaksTbl,
        workDatabaseSchema = clinChar@executionSettings@workDatabaseSchema,
        catId = i,
        year = year
      )
      cli::cat_line(glue::glue("\t - New category_id for categorized variable: {crayon::green((i * 1000) + 1)}"))
      # go to next categorize
    }
    #end all categorize
  }
  invisible(to_cat)

}


summarize_them_cts <- function(clinChar, connection) {

  cts_ids <- get_cts_ids(clinChar)
  if (length(cts_ids) > 0) {
    cli::cat_bullet("Summarize Continuous Variables",
                    bullet = "pointer",
                    bullet_col = "yellow")
    # summarize continuous covars
    cts_sum <- summarize_continuous(
      connection = connection,
      dataTable = clinChar@executionSettings@dataTable,
      cts_ids = cts_ids
    ) |>
      dplyr::mutate(
        mean = occ_cnt / n # do outside ow it rounds
      ) |>
      dplyr::left_join(
        set_labels(clinChar),
        by = c("cohort_id" = "cohort_id",
               "category_id" = "order_id",
               "time_id" = "time_id",
               "value_id" = "value_id")
      ) |>
      dplyr::rename(
        order_id = category_id,
        category_id = category_id.y
      ) |>
      dplyr::mutate(
        database_id = clinChar@executionSettings@database
      ) |>
      dplyr::select(
        database_id,
        cohort_id, cohort_name, order_id, category_id, category_name,
        time_id, time_name, value_id, value_name,
        n, mean, sd, min, p25, median, p75, max
      )
  } else {
    cts_sum <- tibble::tibble()
  }

  return(cts_sum)
}


summarize_them_cat <- function(clinChar, connection) {

  # Cat Summary
  cat_ids <- get_cat_ids(clinChar)
  if (length(cat_ids) > 0) {

    cli::cat_bullet("Summarize Categorical Variables",
                    bullet = "pointer",
                    bullet_col = "yellow")

    cat_sum <- summarize_categorical(
      connection = connection,
      dataTable = clinChar@executionSettings@dataTable,
      workDatabaseSchema = clinChar@executionSettings@workDatabaseSchema,
      cohortTable = clinChar@executionSettings@cohortTable,
      cat_ids = cat_ids
    ) |>
      dplyr::left_join(
        set_labels(clinChar),
        by = c("cohort_id" = "cohort_id",
               "category_id" = "order_id",
               "time_id" = "time_id",
               "value_id" = "value_id")
      ) |>
      dplyr::rename(
        order_id = category_id,
        category_id = category_id.y
      ) |>
      dplyr::mutate(
        database_id = clinChar@executionSettings@database
      ) |>
      dplyr::select(
        database_id,
        cohort_id, cohort_name, order_id, category_id, category_name,
        time_id, time_name, value_id, value_name,
        n, pct
      )
  } else {
    cat_sum <- tibble::tibble()
  }
  return(cat_sum)
}

save_summaries <- function(dat,
                           type = c("categorical", "continuous"),
                           savePath,
                           saveName) {

  type_pr <- snakecase::to_title_case(type)

  cli::cat_bullet(
    glue::glue("Saving {type_pr} Characteristics as csv"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  saveName <- glue::glue("{saveName}_{type}")
  filePath <- fs::path(savePath, saveName, ext = "csv")
  readr::write_csv(dat, file = filePath)
  cli::cat_line(
    glue::glue("   Saving to {crayon::cyan(filePath)}")
  )
  invisible(dat)
}

# UI ---------------------------

#' Runs the characterization and extracts data into an arrow object
#' @description
#' This runs the characterization specified by the clinChar object
#' @param connection the DatabaseConnector connection linking to the dbms with OMOP data
#' @param clinChar the clinChar object describing the study
#' @param dropDat toggle option to drop temporary data table with clinChar results in the dbms
#' @param saveName a labelling name to distinguish the characterization
#' @param savePath the folder path to save the csv, defaults to current directory
#' @return runs database query described in extractSettings and uploads them to the stow object
#' @export
runClinicalCharacteristics <- function(connection,
                                       clinChar,
                                       dropDat = TRUE,
                                       saveName = NULL,
                                       savePath = here::here()) {

  cli::cat_boxx(
    "Run Clinical Characteristics Job"
  )
  clinCharJobDetails(clinChar)
  cli::cat_line()

  # build sql
  sql <- build_query(clinChar)

  # check and drop data
  check_and_drop_dat(connection, clinChar)

  # add time table
  insert_time_table(connection = connection, clinChar = clinChar)

  # Run queries

  cli::cat_bullet(
    "Run ClinChar Queries....",
    bullet = "pointer",
    bullet_col = "yellow"
  )
  ## execute on db
  DatabaseConnector::executeSql(connection = connection, sql = sql)

  # Next look for scoring and categories
  ## Do score First
  score_them(clinChar, connection)
  ## Do categoies second
  categorize_them(clinChar, connection)

  # Now summarize
  ## Continuous summary first
  cts_sum <- summarize_them_cts(clinChar, connection)
  ## Categorical summary second
  cat_sum <- summarize_them_cat(clinChar, connection)

  clin_char_res <- list(
    'continuous' = cts_sum,
    'categorical' = cat_sum
  )

  # save as csv
  if(is.null(saveName)) {
    saveName <- "clin_char_res"
  } else {
    saveName <- snakecase::to_snake_case(saveName)
  }

  if (nrow(clin_char_res$categorical) > 0) {
    save_summaries(dat = clin_char_res$categorical,
                   type = "categorical",
                   saveName = saveName,
                   savePath = savePath)
  }

  if (nrow(clin_char_res$continuous) > 0 ) {
    save_summaries(dat = clin_char_res$continuous,
                   type = "continuous",
                   saveName = saveName,
                   savePath = savePath)
  }


  #drop #dat option
  if (dropDat) {
    cli::cat_bullet(
      glue::glue("Drop {crayon::green(clinChar@executionSettings@dataTable)} from db"),
      bullet = "pointer",
      bullet_col = "yellow"
    )
    DatabaseConnector::executeSql(
      connection = connection,
      trunc_drop(clinChar@executionSettings@dataTable),
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
  }

  invisible(clin_char_res)
}


#' Preview the characterization
#' @description
#' This shows the characterization tables using reactable
#' @param tb the table created from the tabulateClincalCharacteristics
#' @param type which table to preview
#' @return presents the reactable summarizing the characterization
#' @export
previewClincalCharacteristics <- function(tb, type = c("categorical", "continuous")) {


  if (type == "categorical") {
    cat_dat <- tb$categorical |>
      dplyr::mutate(
        cohort_name = snakecase::to_title_case(cohort_name),
        category_name = snakecase::to_title_case(category_name)
      ) |>
      dplyr::arrange(
        cohort_id, order_id, category_id, time_id, value_id
      )

    res_tb <- reactable::reactable(
      data = cat_dat,
      columns = list(
        'cohort_id' = reactable::colDef(name = "Cohort Id"),
        'cohort_name' = reactable::colDef(name = "Cohort Name"),
        'order_id' = reactable::colDef(name = "Order Id"),
        'category_id' = reactable::colDef(name = "Category Id"),
        'category_name' = reactable::colDef(name = "Category Name"),
        'time_id' = reactable::colDef(name = "Time Id"),
        'time_name' = reactable::colDef(name = "Time Name"),
        'value_id' = reactable::colDef(name = "Value Id"),
        'value_name' = reactable::colDef(name = "Value Name"),
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

    cts_dat <- tb$continuous |>
      dplyr::mutate(
        cohort_name = snakecase::to_title_case(cohort_name),
        category_name = snakecase::to_title_case(category_name)
      ) |>
      dplyr::arrange(
        cohort_id, order_id, category_id, time_id, value_id
      )

    res_tb <- reactable::reactable(
      data = cts_dat,
      columns = list(
        'cohort_id' = reactable::colDef(name = "Cohort Id"),
        'cohort_name' = reactable::colDef(name = "Cohort Name"),
        'order_id' = reactable::colDef(name = "Order Id"),
        'category_id' = reactable::colDef(name = "Category Id"),
        'category_name' = reactable::colDef(name = "Category Name"),
        'time_id' = reactable::colDef(name = "Time Id"),
        'time_name' = reactable::colDef(name = "Time Name"),
        'value_id' = reactable::colDef(name = "Value Id"),
        'value_name' = reactable::colDef(name = "Value Name"),
        'n' = reactable::colDef(
          name = "n", format = reactable::colFormat(separators = TRUE)
        ),
        'mean' = reactable::colDef(
          name = "nean", format = reactable::colFormat(separators = TRUE, digits = 2)
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

#' Function to review the query built by clinical characteristics
#' @param clinChar the clinChar object
#' @param savePath the location to save the query to review. By default
#' this saves the query to a file named clinCharQuery.sql in your active
#' directory
#' @return a monaco html widget to review the query. You may edit the query in the
#' viewer and save to the file created by this function
#' @export
reviewQuery <- function(clinChar, savePath = here::here("clinCharQuery.sql")) {
  sql <- build_query(clinChar)
  cli::cat_bullet(
    glue::glue("Writing ClinChar Query to: {crayon::cyan(savePath)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  readr::write_file(sql, savePath)
  cli::cat_bullet(
    glue::glue("Opening {crayon::magenta('Monaco')} widget"),
    bullet = "pointer",
    bullet_col = "yellow"
  )
  mnc <- monaco::monaco(
    contents = savePath,
    language = "sql",
    theme = "vs"
  )
  return(mnc)
}
