# Breaks -------------------------
# Continuous => categorical
## Age breaks ---------------------------------

#' Make 5 yr age group
#' @description
#' Helper function for age characteristic to make 5 year age breaks
#'
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
age5yrGrp <- function() {

  x <- seq(0,130, by = 5)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  lab <- c(lab, paste0(dplyr::last(x), "+"))
  br <- new("breaksStrategy", name = "age5yrGrp",
            breaks = x,
            labels = lab
  )
  return(br)

}

#' Make 10 yr age group
#' @description
#' Helper function for age characteristic to make 10 year age breaks
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
age10yrGrp <- function() {

  x <- seq(0,130, by = 10)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  lab <- c(lab, paste0(dplyr::last(x), "+"))
  br <- new("breaksStrategy", name = "age10yrGrp",
            breaks = x,
            labels = lab
  )
  return(br)
}


# ageAcs <- function() {
#   x <- c(0,5,10,15,18,20,21,22,25,30,35,40,45,50,55,
#          60,62,65,67,70, 75, 80, 85, 130)
#   a <- dplyr::lead(x) - 1
#   lab <- glue::glue("{x}-{a}")[-length(x)]
#   ll <- tibble::tibble(
#     value = as.numeric(0:129),
#     grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
#   ) |>
#     dplyr::mutate(
#       grp_id = as.numeric(grp)
#     )
#   br <- new("breaksStrategy", name = "ageAcs", breaks = ll)
#   return(br)
# }
#
# ageChildWorkRetire <- function() {
#   x <- c(0,15,65, 130)
#   a <- dplyr::lead(x) - 1
#   lab <- glue::glue("{x}-{a}")[-length(x)]
#   ll <- tibble::tibble(
#     value = as.numeric(0:129),
#     grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
#   ) |>
#     dplyr::mutate(
#       grp_id = as.numeric(grp)
#     )
#   br <- new("breaksStrategy", name = "ageChildWorkRetire", breaks = ll)
#   return(br)
# }
#
#
# age19Grps <- function() {
#   x <- c(seq(0,85, by = 5), 130)
#   x[1] <- 1
#   a <- dplyr::lead(x) - 1
#   lab <- c("0", glue::glue("{x}-{a}"))[-20]
#   x <- c(0,x)
#
#   ll <- tibble::tibble(
#     value = as.numeric(0:129),
#     grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
#   ) |>
#     dplyr::mutate(
#       grp_id = as.numeric(grp)
#     )
#   br <- new("breaksStrategy", name = "age19Grps", breaks = ll)
#   return(br)
# }

#' Make age group using 65 threshold
#' @description
#' Helper function for age characteristic to make age breaks categorizing persons
#' below age 65 and at or above age 65
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
age65 <- function() {
  x <- c(0,65)
  lab <- c(">65", "<=65")
  br <- new("breaksStrategy", name = "age65Grp",
            breaks = x,
            labels = lab
  )
  return(br)
}

#' Make age group using 18 threshold
#' @description
#' Helper function for age characteristic to make age breaks categorizing persons
#' below age 18 and at or above age 18
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
age18 <- function() {
  x <- c(0,18)
  lab <- c(">18", "<=18")
  br <- new("breaksStrategy", name = "age18Grp",
            breaks = x,
            labels = lab
  )
  return(br)
}

## year breaks--------------------

#' Make year group using 5 yr threshold
#' @description
#' Helper function for year characteristic to make year breaks categorizing persons
#' per 5 year groups
#' @param startYear the start year of the sequence, defaults to 2000
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
year5yrGrp <- function(startYear = 2000) {
  this_year <- as.integer(lubridate::year(lubridate::today()))
  x <- seq(startYear, (this_year + 5), by = 5)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  x2 <- x[-length(x)]
  br <- new("breaksStrategy", name = "year5yrGrp",
            breaks = x2,
            labels = lab
  )
  return(br)
}


#' Make year group using 10 yr threshold
#' @description
#' Helper function for year characteristic to make year breaks categorizing persons
#' per 10 year groups
#' @param startYear the start year of the sequence, defaults to 2000
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
year10yrGrp <- function(startYear = 2000) {

  this_year <- as.integer(lubridate::year(lubridate::today()))
  x <- seq(startYear, (this_year + 10), by = 10)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  x2 <- x[-length(x)]
  br <- new("breaksStrategy", name = "year10yrGrp",
            breaks = x2,
            labels = lab
  )
  return(br)

}

#' Make year group by covid time
#' @description
#' Helper function for year characteristic to make year breaks categorizing persons
#' by covid time where 1987-2019 is pre-covid, 2020-2022 is covid and 2023+ is post-covid
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
yearCovid <- function(startYear = 2000) {
  x <- c(startYear, 2020, 2023)
  lab <- c("pre-covid", "covid", "post-covid")
  br <- new("breaksStrategy", name = "yearCovid",
            breaks = x,
            labels = lab
  )
  return(br)

}


## Measurement breaks --------------------

bmiGroups <- function() {
  br <- new("breaksStrategy", name = "bmiBreaks",
            breaks = c(0, 18.5, 25, 30),
            labels = c("underweight", "normal", "overweight", "obese")
  )
  return(br)
}


## Count breaks --------------------------

#' Function to make custom categorical breaks
#' @param breaks a sequence of values to use as break points for categorization.
#' The sequence corresponds to the left side of the bound. For example
#' 0, 2 would represent counts cuts of 1 and 2+.
#' @param labels a sequence of character strings labelling the break points for categorization.
#' For example 1 and 2+ would be labels for 0, 2.
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
countBreaks <- function(breaks, labels) {
  br <- new("breaksStrategy",
            name = "countBreaks",
            breaks = breaks,
            labels = labels
  )
  return(br)
}

## Custom breaks --------------------------

#' Function to make custom categorical breaks
#' @param name the name of the categorization, as single character string
#' @param breaks a sequence of values to use as break points for categorization.
#' The sequence corresponds to the left side of the bound. For example
#' 0, 2, 6, 12, 18 would represent age cuts of 0-1, 2-5, 6-11, 12-17 and 18+.
#' @param labels a sequence of character strings labelling the break points for categorization.
#' For example 0-1, 2-5, 6-11, 12-17 and 18+ would be labels to sequence 0, 2, 6, 12, 18.
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
customBreaks <- function(name, breaks, labels) {
  br <- new("breaksStrategy",
            name = name,
            breaks = breaks,
            labels = labels
  )
  return(br)
}

## sql helper ---------------
.catagorizeType <- function(name) {
  if (grepl("year", name)) {
    type <- "year"
  } else if (name == "countBreaks") {
    type <- "count"
  } else {
    type <- "default"
  }
  return(type)
}

make_case_when_sql <- function(breaksStrategy, type = c("default","year", "count")) {
  x <- breaksStrategy@breaks

  if (type == "default") {
    sql_when <- tibble::tibble(
      lhs = x,
      rhs = dplyr::lead(x) - 0.01
    ) |>
      dplyr::mutate(
        ord = dplyr::row_number(),
        expr_left = glue::glue("{lhs} <= a.value"),
        expr_right = dplyr::if_else(!is.na(rhs), glue::glue("a.value <= {rhs}"), ""),
        expr_both = glue::glue("WHEN ({expr_left} AND {expr_right}) THEN {ord}"),
        expr_both = dplyr::if_else(is.na(rhs), gsub(" AND ", "", expr_both), expr_both)
      ) |>
      dplyr::pull(expr_both) |>
      glue::glue_collapse(sep = "\n")

    case_when_sql <- c(
      "CASE ",
      glue::glue_collapse(sql_when, sep = "\n\t"),
      "\nELSE -999 END AS value_id"
    ) |>
      glue::glue_collapse()

  }
  if (type == "year"){
    sql_when <- tibble::tibble(
      lhs = x,
      rhs = dplyr::lead(x) - 0.01
    ) |>
      dplyr::mutate(
        ord = dplyr::row_number(),
        expr_left = glue::glue("{lhs} <= a.value_id"),
        expr_right = dplyr::if_else(!is.na(rhs), glue::glue("a.value_id <= {rhs}"), ""),
        expr_both = glue::glue("WHEN ({expr_left} AND {expr_right}) THEN {ord}"),
        expr_both = dplyr::if_else(is.na(rhs), gsub(" AND ", "", expr_both), expr_both)
      ) |>
      dplyr::pull(expr_both) |>
      glue::glue_collapse(sep = "\n")

    case_when_sql <- c(
      "CASE ",
      glue::glue_collapse(sql_when, sep = "\n\t"),
      "\nELSE -999 END AS value_id"
    ) |>
      glue::glue_collapse()
  }


  if (type == "count"){
    sql_when <- tibble::tibble(
      lhs = x,
      rhs = dplyr::lead(x) - 0.01
    ) |>
      dplyr::mutate(
        ord = dplyr::row_number(),
        expr_left = glue::glue("{lhs} <= a.value"),
        expr_right = dplyr::if_else(!is.na(rhs), glue::glue("a.value <= {rhs}"), ""),
        expr_both = glue::glue("WHEN ({expr_left} AND {expr_right}) THEN CAST(((CAST(a.value_id AS INT) * 1000) + {ord}) AS INT)"),
        expr_both = dplyr::if_else(is.na(rhs), gsub(" AND ", "", expr_both), expr_both)
      ) |>
      dplyr::pull(expr_both) |>
      glue::glue_collapse(sep = "\n")

    case_when_sql <- c(
      "CASE ",
      glue::glue_collapse(sql_when, sep = "\n\t"),
      "\nELSE -999 END AS value_id"
    ) |>
      glue::glue_collapse()
  }


  return(case_when_sql)
}


## sql runner -----------

# Old version with merge key
# categorize_sql <- function(catId) {
#   newId <- (catId * 1000) + 1
#   sql <- glue::glue("
#   WITH T1 AS (
#       -- Get covariate to categorize
#       SELECT * FROM {{dataTable}} WHERE category_id = {catId}
#     ),
#     T2 AS (
#     SELECT a.cohort_id, a.subject_id, a.category_id, a.time_id, b.grp_id AS value_id,
#       1 AS value
#     FROM T1 a
#     LEFT JOIN {{breaksTable}} b ON a.value = b.value
#     )
#     SELECT dd.cohort_id, dd.subject_id,
#       {newId} AS category_id,
#       dd.time_id,
#       value_id, value
#     INTO {{breaks_dat_tmp}}
#     FROM T2 dd
#     ;")
#
#   return(sql)
#
# }

categorize_sql <- function(catId, breaksStrategy, type) {
  newId <- (catId * 1000) + 1
  case_when_sql <- make_case_when_sql(breaksStrategy, type)
    sql <- glue::glue("
    SELECT a.cohort_id, a.subject_id,
      {newId} AS category_id, a.time_id,
      {case_when_sql},
      1 AS value
    FROM (
      SELECT * FROM {{dataTable}} WHERE category_id = {catId}
      ) a")

  return(sql)
}

# year_sql <- function(catId) {
#   newId <- (catId * 1000) + 1
#   case_when_sql <- make_case_when_sql(breaksStrategy)
#   sql <- glue::glue("
#   WITH T1 AS (
#       -- Get covariate to categorize
#       SELECT * FROM {{dataTable}} WHERE category_id = {catId}
#     ),
#     T2 AS (
#     SELECT a.cohort_id, a.subject_id, a.category_id, a.time_id, b.grp_id AS value_id,
#       1 AS value
#     FROM T1 a
#     LEFT JOIN {{breaksTable}} b ON a.value_id = b.value
#     )
#     SELECT dd.cohort_id, dd.subject_id,
#       {newId} AS category_id,
#       dd.time_id,
#       value_id, value
#     INTO {{breaks_dat_tmp}}
#     FROM T2 dd
#     ;")
#
#   return(sql)
#
# }

categorize_value <- function(connection,
                             dataTable,
                             breaksStrategy,
                             workDatabaseSchema,
                             catId,
                             type) {

  dbms <- connection@dbms

  breaks_dat_tmp <- "#breaks"

  cat_sql <- categorize_sql(catId, breaksStrategy, type) |> glue::glue()

  sql <- glue::glue("
    /* Insert categories into data table */
    INSERT INTO {dataTable} (cohort_id, subject_id, category_id, time_id, value_id, value)
    {cat_sql};") |>
    SqlRender::translate(targetDialect = dbms,
                         tempEmulationSchema = workDatabaseSchema)

  DatabaseConnector::executeSql(connection, sql)
  invisible(sql)

}

# Scores -------------------------------
# Categorical => continuous

## sql runner -----------
score_value <- function(connection, dataTable, scoreTable, workDatabaseSchema, scoreId, scoreSql) {

  dbms <- connection@dbms

  score_dat_tmp <- "#score"

  newId <- (scoreId * 1000) + 1
  scoreSql2 <- glue::glue(scoreSql)
  sql <- glue::glue("
    /* Step 1: Make Score Values */
    {scoreSql2}

    /* Step 2: Insert into data table */
    INSERT INTO {dataTable} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT * FROM {score_dat_tmp};

    /* Step 3: Drop temp score tables */
    TRUNCATE TABLE {score_dat_tmp}; DROP TABLE {score_dat_tmp};
    TRUNCATE TABLE {scoreTable}; DROP TABLE {scoreTable};") |>
    SqlRender::translate(targetDialect = dbms,
                         tempEmulationSchema = workDatabaseSchema)

  DatabaseConnector::executeSql(connection, sql)
  invisible(sql)
}


## Charlson -----------------------------

#' Function to make charlson score
#' @param ageId the id of the age slot in the clinChar object
#' @return Creates a scoreStrategy object holding weights for score
#' @export
charlsonIndexScore <- function(ageId) {

  # deal with concept scores first
  idx <- seq_along(charlsonConcepts())

  weights <- c(
    # Score for:Acute MI,congestive heart failure, peripheral vascular disease,
    # cerebrovascular disease, dementia, chronic pulmonary disease,
    # rheumatologic disease, peptic ulcer, mild liver disease, controlled diabetes
    rep(1, times = 10),
    # Score for: hemiplegia or paraplegia, renal disease, malignancy localizedm leukemia, lymphoma
    rep(2, times = 5),
    # score for: AIDS and metastatic tumor
    rep(6, times = 2)
  )

  charlsonSql <- fs::path_package("ClinicalCharacteristics", "sql/CharlsonIndex.sql") |>
    readr::read_file() |>
    glue::glue()



  charlsonIndex <- new("scoreStrategy",
                       name = "CharlsonIndex",
                       domain = c("Condition", "Age"),
                       sql = charlsonSql,
                       weights = tibble::tibble(
                         id = idx,
                         w = weights
                       )
                       )

  ageTbl <- tibble::tibble(
    value = 0:130
  )
  # next add age scores
  ageScore <- ageTbl |>
    dplyr::filter(
      value >= 50
    ) |>
    dplyr::mutate(
      w = dplyr::case_when(
       dplyr::between(value, 50, 59) ~ 1,
       dplyr::between(value, 60, 69) ~ 2,
       dplyr::between(value, 70, 79) ~ 3,
       value >= 80 ~ 4
      ),
      id = value
    ) |>
    dplyr::select(
      id, w
    )

  charlsonIndex@weights <- dplyr::bind_rows(charlsonIndex@weights, ageScore)
  cli::cat_bullet(
    glue::glue("{crayon::red('Note')}: User needs to add an ageChar to clinChar object when using charlsonIndexScore()"),
    bullet = "info",
    bullet_col = "blue"
  )

  return(charlsonIndex)
}

# insert_score_weights <- function(connection, clinChar, scoreObj) {
#
#   cli::cat_bullet(
#     glue::glue("Insert weights to create score for {scoreObj@name}"),
#     bullet = "pointer",
#     bullet_col = "yellow"
#   )
#
#   if(connection@dbms == "snowflake") {
#     scratchSchema <- clinChar@executionSettings@workDatabaseSchema
#     scoreTbl <- glue::glue("{scratchSchema}.score_{scoreObj@name}")
#     tempTabToggle <- TRUE
#   } else{
#     scoreTbl <- glue::glue("#score_{scoreObj@name}")
#     tempTabToggle <- FALSE
#   }
#
#   DatabaseConnector::insertTable(
#     connection = connection,
#     tableName = scoreTbl,
#     data = scoreObj@weights,
#     tempTable = tempTabToggle
#   )
#
#
#   invisible(scoreObj@weights)
# }

charlsonSql <- function(scoreObj, ageId) {

  cat_ids <- paste(ageId, conditionId, sep = ", ")
  new_id <- (conditionId * 1000) + 1
  # make charlson score sql
  sql <- glue::glue("
  -- Charlson Score Sql
  WITH T1 AS (
      -- Get category ids corresponding to charlson age and condition presence
      SELECT cohort_id, subject_id, category_id, time_id,
      CAST(CASE WHEN category_id = {ageId} THEN value ELSE value_id END AS int) AS value_id,
      value
      FROM {{dataTable}} WHERE category_id IN ({cat_ids})
    ),
    T2 AS (
    SELECT a.cohort_id, a.subject_id, a.category_id, a.time_id, a.value_id,
      CASE WHEN b.w IS NULL THEN 0 ELSE b.w END AS value
    FROM T1 a
    LEFT JOIN {{scoreTable}} b ON CAST(a.value_id AS INT) = CAST(b.id AS INT)
    ),
    age AS (
      SELECT cohort_id, subject_id, SUM(value) AS age_score
      FROM T2
      WHERE category_id = {ageId}
      GROUP BY cohort_id, subject_id
    ),
    cond AS (
      SELECT cohort_id, subject_id, time_id, SUM(value) AS cond_score
      FROM T2
      WHERE category_id = {conditionId}
      GROUP BY cohort_id, subject_id, time_id
    )
    SELECT dd.cohort_id, dd.subject_id,
      {new_id} AS category_id,
      dd.time_id,
      -999 AS value_id,
      dd.cond_score + ag.age_score AS value
    INTO {{score_dat_tmp}}
    FROM cond dd
    JOIN age ag ON ag.cohort_id = dd.cohort_id AND ag.subject_id = dd.subject_id
    ;")

  return(sql)
}



