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
  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  ) |>
    dplyr::mutate(
      grp_id = as.numeric(grp)
    )
  br <- new("breaksStrategy", name = "age5yrGrp", breaks = ll)
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
  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  ) |>
    dplyr::mutate(
      grp_id = as.numeric(grp)
    )
  br <- new("breaksStrategy", name = "age10yrGrp", breaks = ll)
  return(br)
}

#' Make age group using American Community Survey style
#' @description
#' Helper function for age characteristic to make age breaks following the break
#' style of the American Community Survey (acs)
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
ageAcs <- function() {
  x <- c(0,5,10,15,18,20,21,22,25,30,35,40,45,50,55,
         60,62,65,67,70, 75, 80, 85, 130)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  ) |>
    dplyr::mutate(
      grp_id = as.numeric(grp)
    )
  br <- new("breaksStrategy", name = "ageAcs", breaks = ll)
  return(br)
}

#' Make age groups for common workforce lifespan
#' @description
#' Helper function for age characteristic to make age breaks following lifespan
#' of a working human. 0-14 is child, 15-64 is working age, 65+ is retirement
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
ageChildWorkRetire <- function() {
  x <- c(0,15,65, 130)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  ) |>
    dplyr::mutate(
      grp_id = as.numeric(grp)
    )
  br <- new("breaksStrategy", name = "ageChildWorkRetire", breaks = ll)
  return(br)
}

#' Make 19 age groups
#' @description
#' Helper function for age characteristic to make 19 age groups where
#' every 5 years are categorized up to age 85. 85+ is a single group
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
age19Grps <- function() {
  x <- c(seq(0,85, by = 5), 130)
  x[1] <- 1
  a <- dplyr::lead(x) - 1
  lab <- c("0", glue::glue("{x}-{a}"))[-20]
  x <- c(0,x)

  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  ) |>
    dplyr::mutate(
      grp_id = as.numeric(grp)
    )
  br <- new("breaksStrategy", name = "age19Grps", breaks = ll)
  return(br)
}

#' Make age group using 65 threshold
#' @description
#' Helper function for age characteristic to make age breaks categorizing persons
#' below age 65 and at or above age 65
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
age65 <- function() {
  x <- c(0,65, 130)
  a <- dplyr::lead(x) - 1
  lab <- c(">65", "<=65")
  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  ) |>
    dplyr::mutate(
      grp_id = as.numeric(grp)
    )
  br <- new("breaksStrategy", name = "age65", breaks = ll)
  return(br)
}

#' Make age group using 18 threshold
#' @description
#' Helper function for age characteristic to make age breaks categorizing persons
#' below age 18 and at or above age 18
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
age18 <- function() {
  x <- c(0,18, 130)
  a <- dplyr::lead(x) - 1
  lab <- c(">18", "<=18")
  ll <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = x, labels = lab, right = FALSE)
  ) |>
    dplyr::mutate(
      grp_id = as.numeric(grp)
    )
  br <- new("breaksStrategy", name = "age18", breaks = ll)
  return(br)
}

## year breaks--------------------

#' Make year group using 5 yr threshold
#' @description
#' Helper function for year characteristic to make year breaks categorizing persons
#' per 5 year groups
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
year5yrGrp <- function() {
  this_year <- as.integer(lubridate::year(lubridate::today()))
  x <- seq(1987, (this_year + 5), by = 5)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  ll <- tibble::tibble(
    value = as.numeric(1987:(this_year + 1)),
    grp = cut(as.numeric(1987:(this_year + 1)), breaks = x, labels = lab, right = FALSE)
  ) |>
    dplyr::mutate(
      grp_id = as.numeric(grp)
    )
  br <- new("breaksStrategy", name = "year5yrGrp", breaks = ll)
  return(br)
}


#' Make year group using 10 yr threshold
#' @description
#' Helper function for year characteristic to make year breaks categorizing persons
#' per 10 year groups
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
year10yrGrp <- function() {
  this_year <- as.integer(lubridate::year(lubridate::today()))
  x <- seq(1987, (this_year + 10), by = 10)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]
  ll <- tibble::tibble(
    value = as.numeric(1987:(this_year + 1)),
    grp = cut(as.numeric(1987:(this_year + 1)), breaks = x, labels = lab, right = FALSE)
  ) |>
    dplyr::mutate(
      grp_id = as.numeric(grp)
    )
  br <- new("breaksStrategy", name = "year10yrGrp", breaks = ll)
  return(br)
}

#' Make year group by covid time
#' @description
#' Helper function for year characteristic to make year breaks categorizing persons
#' by covid time where 1987-2019 is pre-covid, 2020-2022 is covid and 2023+ is post-covid
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
yearCovid <- function() {
  this_year <- as.integer(lubridate::year(lubridate::today()))
  x <- c(1987,2020, 2023, (this_year + 1))
  a <- dplyr::lead(x) - 1
  lab <- c("pre-covid", "covid", "post-covid")
  ll <- tibble::tibble(
    value = as.numeric(1987:(this_year + 1)),
    grp = cut(as.numeric(1987:(this_year + 1)), breaks = x, labels = lab, right = FALSE)
  ) |>
    dplyr::mutate(
      grp_id = as.numeric(grp)
    )
  br <- new("breaksStrategy", name = "yearCovid", breaks = ll)
  return(br)
}


## Custom breaks --------------------------

#' Function to make custom categorical breaks
#' @param x a sequence of values to categorize
#' @param breaks the breaks to the x sequenece..see `cut`
#' @param labels the labels of the sequence
#' @return Creates a breaksStrategy object holding the labels for categorization
#' @export
customBreaks <- function(x, breaks, labels) {
  ll <- tibble::tibble(
    value = x,
    grp = cut(x, breaks = breaks, labels = labels, right = FALSE)
  ) |>
    dplyr::mutate(
      grp_id = as.numeric(grp)
    )
  br <- new("breaksStrategy", name = "customBreaks", breaks = ll)
  return(br)
}

## sql runner -----------

categorize_sql <- function(catId) {
  newId <- (catId * 1000) + 1
  sql <- glue::glue("
  WITH T1 AS (
      -- Get covariate to categorize
      SELECT * FROM {{dataTable}} WHERE category_id = {catId}
    ),
    T2 AS (
    SELECT a.cohort_id, a.subject_id, a.category_id, a.time_id, b.grp_id AS value_id,
      1 AS value
    FROM T1 a
    LEFT JOIN {{breaksTable}} b ON a.value = b.value
    )
    SELECT dd.cohort_id, dd.subject_id,
      {newId} AS category_id,
      dd.time_id,
      value_id, value
    INTO {{breaks_dat_tmp}}
    FROM T2 dd
    ;")

  return(sql)

}

year_sql <- function(catId) {
  newId <- (catId * 1000) + 1
  sql <- glue::glue("
  WITH T1 AS (
      -- Get covariate to categorize
      SELECT * FROM {{dataTable}} WHERE category_id = {catId}
    ),
    T2 AS (
    SELECT a.cohort_id, a.subject_id, a.category_id, a.time_id, b.grp_id AS value_id,
      1 AS value
    FROM T1 a
    LEFT JOIN {{breaksTable}} b ON a.value_id = b.value
    )
    SELECT dd.cohort_id, dd.subject_id,
      {newId} AS category_id,
      dd.time_id,
      value_id, value
    INTO {{breaks_dat_tmp}}
    FROM T2 dd
    ;")

  return(sql)

}

categorize_value <- function(connection, dataTable, breaksTable,
                             workDatabaseSchema, catId, year) {

  dbms <- connection@dbms

  breaks_dat_tmp <- "#breaks"

  if (year) {
    cat_sql <- year_sql(catId) |> glue::glue()
  } else {
    cat_sql <- categorize_sql(catId) |> glue::glue()
  }

  sql <- glue::glue("
    /* Step 1: Make Score Values */
    {cat_sql}

    /* Step 2: Insert into data table */
    INSERT INTO {dataTable} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT * FROM {breaks_dat_tmp};

    /* Step 3: Drop temp score tables */
    TRUNCATE TABLE {breaks_dat_tmp}; DROP TABLE {breaks_dat_tmp};
    TRUNCATE TABLE {breaksTable}; DROP TABLE {breaksTable};") |>
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


  # next add age scores
  ageScore <- age10yrGrp()@breaks |>
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



