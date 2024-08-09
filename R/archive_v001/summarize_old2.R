# Merge Keys ----------------------------
cohort_key <- function(clinChar) {
  cohortKey <- tibble::tibble(
    cohort_id = as.numeric(clinChar@targetCohort@id),
    cohort_name = clinChar@targetCohort@name
  )
  return(cohortKey)
}

cat_name <- function(slot) {

  cl <- class(slot)
  dm <- slot@domain
  if (cl %in% c("presenceChar", "labChar", "countChar", "costChar", "timeToChar")) {
    catName <- glue::glue("{gsub('Char', '', cl)}_{dm}")
  } else{
    catName <- dm
  }

  return(catName)
}

category_key <- function(clinChar){

  catName <- purrr::map_chr(clinChar@extractSettings, ~cat_name(.x))
  catId <- purrr::map_int(clinChar@extractSettings, ~.x@orderId)

  categoryKey <- tibble::tibble(
    category_id = catId,
    category_name = catName
  )
  return(categoryKey)
}

# Categorize Value ----------------------

categorize_value <- function(tbl, breaksKey) {
  tb <- tbl |>
    dplyr::left_join(breaksKey, by = "value") |>
    dplyr::mutate(
     value = 1,
     value_id = grp
    ) |>
    dplyr::select(-c(grp))
  return(tb)
}


categorize_year <- function(tbl, breaksKey) {
  tb <- tbl |>
    dplyr::left_join(breaksKey, by = c("value_id" = "value")) |>
    dplyr::mutate(
      value = 1,
      value_id = grp
    ) |>
    dplyr::select(-c(grp))
  return(tb)
}

# Score Value --------------------



score_value <- function(connection, clinChar) {

  datTbl <- clinChar@executionSettings@dataTable
  dbms <- clinChar@executionSettings@dbms
  workDatabaseSchema <- clinChar@executionSettings@workDatabaseSchema


  if (dbms == "snowflake") {
    score_tmp <- glue::glue("{workDatabaseSchema}.score_tmp")
  } else{
    score_tmp <- "#score"
  }

    sql <- glue::glue("
    WITH T1 AS (
      SELECT cohort_id, subject_id, category_id, time_id,
      CAST(CASE WHEN value_id = -999 THEN value ELSE value_id END AS int) AS value_id,
      value
      FROM #dat WHERE category_id IN (1,2)
    ),
    T2 AS (
    SELECT a.cohort_id, a.subject_id, a.category_id, a.time_id, a.value_id,
      CASE WHEN b.w IS NULL THEN 0 ELSE b.w END AS value
    FROM T1 a
    LEFT JOIN #score_CharlsonIndex b ON CAST(a.value_id AS INT) = CAST(b.id AS INT)
    ),
    demo AS (
      SELECT cohort_id, subject_id, SUM(value) AS dem_score
      FROM T2
      WHERE time_id = -999
      GROUP BY cohort_id, subject_id
    ),
    dom AS (
      SELECT cohort_id, subject_id, time_id, SUM(value) AS dom_score
      FROM T2
      WHERE time_id <> -999
      GROUP BY cohort_id, subject_id, time_id
    )
    SELECT dd.cohort_id, dd.subject_id,
      2001 AS category_id,
      dd.time_id,
      -999 AS value_id,
      dd.dom_score + de.dem_score AS value
    INTO {score_tmp}
    FROM dom dd
    JOIN demo de ON de.cohort_id = dd.cohort_id AND de.subject_id = dd.subject_id
    ;

    -- Insert into data table
    INSERT INTO {datTbl} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT * FROM {score_tmp}
    ;

    TRUNCATE TABLE {score_tmp}; DROP TABLE {score_tmp};")


  DatabaseConnector::executeSql(connection, sql)
}



# Summary --------------------

check_score <- function(x) {
  score_slot <- "score" %in% methods::slotNames(x)
  if (score_slot) {
    check <- !is.null(x@score)
  } else{
    check <- FALSE
  }
  return(check)
}

get_cts_ids <- function(clinChar) {
  es <- clinChar@extractSettings
  cln <- purrr::map_chr(es, ~methods::is(.x))
  cts_char <- c("ageChar", "countChar", "costChar", "timeToChar", "timeInChar", "labChar")
  ids <- which(cln %in% cts_char)

  #add cat that are scored
  to_cts <- purrr::map_lgl(es, ~check_score(.x)) |> which()
  if (length(to_cts) > 0) {
    to_cts <- (to_cts * 1000) + 1
    ids <- c(ids, to_cts)
  }

  return(ids)
}


summarize_continuous <- function(connection, clinChar) {

  datTbl <- clinChar@executionSettings@dataTable
  cts_id <- get_cts_ids(clinChar) |>
    paste(collapse = ", ")

  sql <- glue::glue(
  "WITH T1 AS (
    SELECT * FROM {datTbl} WHERE category_id IN ({cts_id})
  )
  SELECT
  cohort_id, category_id, time_id, value_id,
    COUNT(subject_id) AS N,
    AVG(value) As mean,
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

  cli::cat_bullet("Summarize Continuous Variables",
                  bullet = "pointer",
                  bullet_col = "yellow")

  cts_sum <- DatabaseConnector::querySql(connection, sql = sql) |>
    tibble::as_tibble() |>
    dplyr::rename_with(tolower)

  return(cts_sum)

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
  cat_char <- c("demoConceptChar", "presenceChar", "visitDetailChar", "locationChar")
  ids <- which(cln %in% cat_char)

  #TODO add cts that are categorized
  #to_cat <- c()
  #ids <- c(ids, to_cat)

  return(ids)
}

summarize_categorical<- function(connection, clinChar) {

  datTbl <- clinChar@stowSettings@dataTable
  cat_id <- get_cat_ids(clinChar) |>
    paste(collapse = ", ")
  workDatabaseSchema <- clinChar@executionSettings@workDatabaseSchema
  cohortTable <- clinChar@executionSettings@cohortTable

  sql <- glue::glue(
    "WITH T1 AS (
    SELECT * FROM {datTbl} WHERE category_id IN ({cat_id})
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

  cli::cat_bullet("Summarize Categorical Variables",
                  bullet = "pointer",
                  bullet_col = "yellow")

  cat_sum <- DatabaseConnector::querySql(connection, sql = sql) |>
    tibble::as_tibble() |>
    dplyr::rename_with(tolower)

  return(cat_sum)

}

# arrow version deprecate
# summarize_continuous <- function(tbl) {
#
#   tb <- tbl |>
#     dplyr::group_by(cohort_id, category_id, time_id, value_id) |>
#     dplyr::summarize(
#       n = dplyr::n_distinct(subject_id),
#       mean = mean(value),
#       sd = sd(value),
#       min_value = min(value),
#       p25_value = quantile(value, probs = c(0.25)),
#       median_value = median(value),
#       p75_value = quantile(value, probs = c(0.75)),
#       max_value = max(value)
#     ) |>
#     dplyr::ungroup() |>
#     dplyr::collect()
#
#   return(tb)
# }
#
# summarize_categorical <- function(tbl, clinChar) {
#
#   tb <- tbl |>
#     dplyr::group_by(cohort_id, category_id, time_id, value_id) |>
#     dplyr::summarize(
#       sum_value = sum(value)
#     ) |>
#     dplyr::ungroup() |>
#     dplyr::left_join(retrieveCount(clinChar), by = "cohort_id") |>
#     dplyr::mutate(
#       sum_value = as.numeric(sum_value),
#       average_value = sum_value / total
#     ) |>
#     dplyr::select(-c(total)) |>
#     dplyr::collect()
#
#   return(tb)
# }

# Labels ------------------------

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


label_table <- function(tbl, clinChar) {

  timeKey <- set_time_labels(clinChar)

  lbl_tbl <- tbl |>
    dplyr::left_join(
      cohort_key(clinChar), by = "cohort_id"
    ) |>
    dplyr::left_join(
      category_key(clinChar), by = "category_id"
    ) |>
    dplyr::left_join(
      timeKey, by = "time_id"
    ) |>
    dplyr::relocate(
      cohort_name, .after = cohort_id
    ) |>
    dplyr::relocate(
      category_name, .after = category_id
    ) |>
    dplyr::relocate(
      time_name, .after = time_id
    )

  if (is.factor(lbl_tbl$value_id)) {
    lbl_tbl <- lbl_tbl |>
      dplyr::mutate(
        value_name = as.character(value_id),
        value_id = as.numeric(value_id)
      ) |>
      dplyr::relocate(
        value_name, .after = value_id
      )
  }


  return(lbl_tbl)
}

# Sum Char -------------------------

setGeneric("sum_char", function(x, clinChar)  standardGeneric("sum_char"))

## Demo Concept Char ----------------
setMethod("sum_char", "demoConceptChar", function(x, clinChar){

  orderId <- x@orderId
  domain <- x@domain

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  demo_labels <- rlang::call2(glue::glue("{domain}_categories")) |>
    eval()

  tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_categorical(clinChar) |>
    label_table(clinChar) |>
    dplyr::left_join(
      demo_labels, by = "value_id"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )
  return(tb)

})

## Age Char ----------------
setMethod("sum_char", "ageChar", function(x, clinChar){

  orderId <- x@orderId

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_continuous() |>
    label_table(clinChar) |>
    dplyr::mutate(
      value_name = "age"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )

  if (!is.null(x@categorize)) {
    tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
      categorize_value(breaksKey = x@categorize@breaks) |>
      summarize_categorical(clinChar) |>
      label_table(clinChar)

  }

  return(tb)

})

## Year Char ----------------
setMethod("sum_char", "yearChar", function(x, clinChar){

  orderId <- x@orderId

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  if (!is.null(x@categorize)) {
    #TODO change this to categorize year
    tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
      categorize_year(breaksKey = x@categorize@breaks) |>
      summarize_categorical(clinChar) |>
      label_table(clinChar)
  } else {
    tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
      summarize_categorical(clinChar) |>
      label_table(clinChar)
  }

  return(tb)

})

## Location Char ----------------
setMethod("sum_char", "locationChar", function(x, clinChar){

  orderId <- x@orderId
  locKey <- x@locationTable@key

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_categorical(clinChar) |>
    dplyr::left_join(locKey, by = "value_id") |>
    label_table(clinChar)
  return(tb)

})

## visit detail Char ----------------
setMethod("sum_char", "visitDetailChar", function(x, clinChar){

  orderId <- x@orderId
  detKey <- x@visitDetailTable@key

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )
  #summarize continuous
  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    dplyr::group_by(cohort_id, subject_id, category_id, time_id, value_id) |>
    dplyr::summarize(
      value = sum(value)
    ) |>
    dplyr::ungroup() |>
    summarize_continuous() |>
    dplyr::left_join(detKey, by = c("value_id" = "concept_id")) |>
    dplyr::rename(
      value_name = concept_name
    ) |>
    dplyr::relocate(
      value_name, .after = "value_id"
    ) |>
    label_table(clinChar)

  #summarize categorical
  tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    dplyr::distinct() |>
    summarize_categorical(clinChar) |>
    dplyr::left_join(detKey, by = c("value_id" = "concept_id")) |>
    dplyr::rename(
      value_name = concept_name
    ) |>
    dplyr::relocate(
      value_name, .after = "value_id"
    ) |>
    label_table(clinChar)

  return(tb)

})


## Lab Char ----------------
setMethod("sum_char", "labChar", function(x, clinChar){

  orderId <- x@orderId
  labKey <- x@labUnitTable@key |>
    dplyr::mutate(
      lab_unit_code = as.numeric(
        (measurement_concept_id * 1000000) + (unit_concept_id - (floor(unit_concept_id/1000) * 1000))
        )
    ) |>
    dplyr::mutate(
      unit_name2 = dplyr::if_else(unit_concept_id == 0, "no units", unit_name),
      value_name = glue::glue("{measurement_name} ({unit_name2})"),
      value_id = lab_unit_code
    ) |>
    dplyr::select(value_id, value_name)

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_continuous() |>
    dplyr::inner_join(
      labKey, by = "value_id"
    ) |>
    label_table(clinChar) |>
    dplyr::relocate(
      value_name, .after = value_id
    )

  return(tb)

})

## Presence Char ----------------
setMethod("sum_char", "presenceChar", function(x, clinChar){

  orderId <- x@orderId

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  codesetKey <- tibble::tibble(
    value_id = x@tempTables$codeset,
    value_name = purrr::map_chr(x@conceptSets, ~.x@Name)
  )

  tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_categorical(clinChar) |>
    label_table(clinChar) |>
    dplyr::left_join(
      codesetKey, by = "value_id"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )


  if (!is.null(x@score)) {
    cat_to_get <- x@score@domain[-1]
    ii <- c(orderId, find_char(clinChar, type = cat_to_get))
    timeKey <- set_time_labels(clinChar)

    tb$continuous <- retrieveTable(clinChar = clinChar, category_id = ii) |>
      score_value(scoreKey = x@score@weights) |>
      dplyr::mutate(
        category_name = x@score@name,
        value_name = x@score@name
      ) |>
      dplyr::left_join(
        cohort_key(clinChar), by = "cohort_id"
      ) |>
      dplyr::left_join(
        timeKey, by = "time_id"
      ) |>
      dplyr::select(
        cohort_id, cohort_name, category_id, category_name,
        time_id, time_name, value_id, value_name,
        n:max_value
      )

  }


  return(tb)

})



## Cost Char ----------------
setMethod("sum_char", "costChar", function(x, clinChar){

  orderId <- x@orderId

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )


  if (!is.null(x@conceptSets)) {
    codesetKey <- tibble::tibble(
      value_id = x@tempTables$codeset,
      value_name = purrr::map_chr(x@conceptSets, ~.x@Name)
    )
  } else {
    codesetKey <- cost_categories()
  }

  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_continuous() |>
    label_table(clinChar) |>
    dplyr::left_join(
      codesetKey, by = "value_id"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )

  if (!is.null(x@categorize)) {
    tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
      categorize_value(breaksKey = x@categorize@breaks) |>
      summarize_categorical(clinChar) |>
      label_table(clinChar)

  }

  return(tb)

})

## Count Char ----------------
setMethod("sum_char", "countChar", function(x, clinChar){

  orderId <- x@orderId
  domain <- x@domain

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  if (!is.null(x@conceptSets)) {
    codesetKey <- tibble::tibble(
      value_id = x@tempTables$codeset,
      value_name = purrr::map_chr(x@conceptSets, ~.x@Name)
    )
  } else {
    codesetKey <- tibble::tibble(
      value_id = -999,
      value_name = count_label(domain)
    )
  }

  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_continuous() |>
    label_table(clinChar) |>
    dplyr::left_join(
      codesetKey, by = "value_id"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )

  if (!is.null(x@categorize)) {
    tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
      categorize_value(breaksKey = x@categorize@breaks) |>
      summarize_categorical(clinChar) |>
      label_table(clinChar)

  }

  return(tb)

})

## TimeIn Char ----------------
setMethod("sum_char", "timeInChar", function(x, clinChar){

  orderId <- x@orderId
  domain <- x@domain


  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_continuous() |>
    label_table(clinChar) |>
    dplyr::left_join(
      time_in_label(), by = "value_id"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )

  if (!is.null(x@categorize)) {
    tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
      categorize_value(breaksKey = x@categorize@breaks) |>
      summarize_categorical(clinChar) |>
      label_table(clinChar)

  }

  return(tb)

})

## TimeTo Char ----------------
setMethod("sum_char", "timeToChar", function(x, clinChar){

  orderId <- x@orderId
  domain <- x@domain

  codesetKey <- tibble::tibble(
    value_id = x@tempTables$codeset,
    value_name = purrr::map_chr(x@conceptSets, ~.x@Name)
  )

  tb <- list(
    'continuous' = NULL,
    'categorical' = NULL
  )

  tb$continuous <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
    summarize_continuous() |>
    label_table(clinChar) |>
    dplyr::left_join(
      codesetKey, by = "value_id"
    ) |>
    dplyr::relocate(
      value_name, .after = value_id
    )

  if (!is.null(x@categorize)) {
    tb$categorical <- retrieveTable(clinChar = clinChar, category_id = orderId) |>
      categorize_value(breaksKey = x@categorize@breaks) |>
      summarize_categorical(clinChar) |>
      label_table(clinChar)

  }

  return(tb)

})

# UI -------------------------------

#' Summarizes the characterization and pulls into R
#' @description
#' This summarizes the characterization specified by the clinChar object
#' @param clinChar the clinChar object describing the study
#' @return pulls the aggregated data into R
#' @export
tabulateClinicalCharacteristics <- function(clinChar) {

  es <- clinChar@extractSettings
  tb <- purrr::map(es, ~sum_char(.x, clinChar = clinChar)) |>
    purrr::flatten() |>
    purrr::compact()

  # get indicies for continuous and categorical
  cts_idx <- which(names(tb) == "continuous")
  cat_idx <- which(names(tb) == "categorical")

  cts_tb <- tb[cts_idx] |> purrr::list_rbind()
  cat_tb <- tb[cat_idx] |>
    purrr::list_rbind()

  tb <- list(
    'continuous' = cts_tb,
    'categorical' = cat_tb
  )

  return(tb)
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
        cohort_id, category_id, time_id, value_id
      )

    res_tb <- reactable::reactable(
      data = cat_dat,
      columns = list(
        'cohort_id' = reactable::colDef(name = "Cohort Id"),
        'cohort_name' = reactable::colDef(name = "Cohort Name"),
        'category_id' = reactable::colDef(name = "Category Id"),
        'category_name' = reactable::colDef(name = "Category Name"),
        'time_id' = reactable::colDef(name = "Time Id"),
        'time_name' = reactable::colDef(name = "Time Name"),
        'value_id' = reactable::colDef(name = "Value Id"),
        'value_name' = reactable::colDef(name = "Value Name"),
        'sum_value' = reactable::colDef(
          name = "N", format = reactable::colFormat(separators = TRUE)
        ),
        'average_value' = reactable::colDef(
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
        cohort_id, category_id, time_id, value_id
      )

    res_tb <- reactable::reactable(
      data = cts_dat,
      columns = list(
        'cohort_id' = reactable::colDef(name = "Cohort Id"),
        'cohort_name' = reactable::colDef(name = "Cohort Name"),
        'category_id' = reactable::colDef(name = "Category Id"),
        'category_name' = reactable::colDef(name = "Category Name"),
        'time_id' = reactable::colDef(name = "Time Id"),
        'time_name' = reactable::colDef(name = "Time Name"),
        'value_id' = reactable::colDef(name = "Value Id"),
        'value_name' = reactable::colDef(name = "Value Name"),
        'n' = reactable::colDef(
          name = "N", format = reactable::colFormat(separators = TRUE)
        ),
        'mean' = reactable::colDef(
          name = "Mean", format = reactable::colFormat(separators = TRUE, digits = 2)
        ),
        'sd' = reactable::colDef(
          name = "SD", format = reactable::colFormat(separators = TRUE, digits = 2)
        ),
        'min_value' = reactable::colDef(
          name = "Min", format = reactable::colFormat(separators = TRUE)
        ),
        'p25_value' = reactable::colDef(
          name = "25th", format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        'median_value' = reactable::colDef(
          name = "Median", format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        'p75_value' = reactable::colDef(
          name = "75th", format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        'max_value' = reactable::colDef(
          name = "Max", format = reactable::colFormat(separators = TRUE)
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

#' Export the characterization
#' @description
#' Save the characterization result as a csv in a folder
#' @param tb the table created from the tabulateClincalCharacteristics
#' @param saveName a labelling name to distinguish the characterization
#' @param savePath the folder path to save the csv, defaults to current directory
#' @return saves a continuous and categorical csv output from the characterization
#' @export
exportClinicalCharacteristics <- function(tb, saveName, savePath = here::here()) {

  saveName <- snakecase::to_snake_case(saveName)

  if (nrow(tb$categorical) > 0) {
    cli::cat_bullet(
      glue::glue("Saving Categorical Characteristics as csv"),
      bullet = "pointer",
      bullet_col = "yellow"
    )
    catSaveName <- glue::glue("{saveName}_categorical")
    catFilePath <- fs::path(savePath, catSaveName, ext = "csv")
    readr::write_csv(tb$categorical, file = catFilePath)
    cli::cat_line(
      glue::glue("   Saving to {crayon::cyan(catFilePath)}")
    )
  }

  if (nrow(tb$continuous) > 0 ) {
    cli::cat_bullet(
      glue::glue("Saving Continuous Characteristics as csv"),
      bullet = "pointer",
      bullet_col = "yellow"
    )
    ctsSaveName <- glue::glue("{saveName}_continuous")
    ctsFilePath <- fs::path(savePath, ctsSaveName, ext = "csv")
    readr::write_csv(tb$continuous, file = ctsFilePath)
    cli::cat_line(
      glue::glue("   Saving to {crayon::cyan(ctsFilePath)}")
    )
  }


  invisible(saveName)

}

