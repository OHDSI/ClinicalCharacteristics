
# Age Continuous -------------------

summarizeAgeContinuous <- function(clinChar) {

  cohortKey <- tibble::tibble(
    cohort_id = as.numeric(clinChar@targetCohort@id),
    cohort_name = clinChar@targetCohort@name
  )

  categoryKey <- tibble::tibble(
    category_id = 101,
    category_name = "age"
  )


  dd <- retrieveTable(clinChar) |>
    dplyr::filter(
      category_id == 101
    ) |>
    dplyr::group_by(cohort_id, category_id) |>
    continuous_summary() |>
    dplyr::ungroup() |>
    dplyr::left_join(cohortKey, by = "cohort_id") |>
    dplyr::left_join(categoryKey, by = "category_id") |>
    dplyr::collect()|>
    dplyr::select(
      cohort_id, cohort_name,
      category_id, category_name,
      n, mean, sd, min_value, p25_value, median_value, p75_value, max_value
    )
  return(dd)
}


# Age Categorical ------------------------
summarizeAgeCategorical <- function(clinChar, ageBreaks) {

  cohortKey <- tibble::tibble(
    cohort_id = as.numeric(clinChar@targetCohort@id),
    cohort_name = clinChar@targetCohort@name
  )

  categoryKey <- tibble::tibble(
    category_id = 101,
    category_name = "age"
  )

  breaksKey <- tibble::tibble(
    value = as.numeric(0:129),
    grp = cut(0:129, breaks = ageBreaks$breaks, labels = ageBreaks$labels, right = FALSE)
  )


  dd <- retrieveTable(clinChar) |>
    dplyr::filter(
      category_id == 101
    ) |>
    dplyr::left_join(breaksKey, by = "value") |>
    dplyr::group_by(cohort_id, category_id) |>
    dplyr::count(grp) |>
    dplyr::ungroup() |>
    dplyr::left_join(cohortKey, by = "cohort_id") |>
    dplyr::left_join(categoryKey, by = "category_id") |>
    dplyr::left_join(
      retrieveCount(clinChar), by = "cohort_id"
    ) |>
    dplyr::mutate(
      sum_value = as.numeric(n),
      average_value = sum_value / total
    ) |>
    dplyr::collect() |>
    dplyr::select(
      cohort_id, cohort_name,
      category_id, category_name, grp, sum_value, average_value
    ) |>
    dplyr::arrange(grp)

  return(dd)

}

# Demographics -------------------

race_categories <- function() {
  tibble::tribble(
    ~value_id, ~value_name,
    8527, "White",
    38003599, "African American",
    8516, "Black or African American",
    8515, "Asian",
    0, "Not Identified"
  )
}

gender_categories <- function() {
  tibble::tribble(
    ~value_id, ~value_name,
    8532, "Female",
    8507, "Male",
    0, "Not Identified"
  )
}

ethnicity_categories <- function() {
  tibble::tribble(
    ~value_id, ~value_name,
    38003563, "Hispanic or Latino",
    38003564, "Not Hispanic or Latino",
    0, "Not Identified"
  )
}

summarizeDemographics <- function(clinChar, type) {

  cohortKey <- tibble::tibble(
    cohort_id = as.numeric(clinChar@targetCohort@id),
    cohort_name = clinChar@targetCohort@name
  )

  demo_type <- demo_translate(type)

  categoryKey <- tibble::tibble(
    category_id = demo_type$category_id,
    category_name = type
  )

  demo_labels <- rlang::call2(glue::glue("{type}_categories")) |>
    eval()


  dd <- retrieveTable(clinChar) |>
    dplyr::filter(
      category_id == demo_type$category_id
    ) |>
    dplyr::group_by(cohort_id, category_id) |>
    dplyr::count(value_id) |>
    dplyr::ungroup() |>
    dplyr::left_join(cohortKey, by = "cohort_id") |>
    dplyr::left_join(categoryKey, by = "category_id") |>
    dplyr::left_join(demo_labels, by = "value_id") |>
    dplyr::left_join(
      retrieveCount(clinChar), by = "cohort_id"
    ) |>
    dplyr::mutate(
      sum_value = as.numeric(n),
      average_value = sum_value / total
    ) |>
    dplyr::collect() |>
    dplyr::select(
      cohort_id, cohort_name,
      category_id, category_name, value_id, value_name, sum_value, average_value
    )
  return(dd)
}

# Location -------------------------

#TODO make this better and work for different columns
grab_locations <- function(clinChar, connection) {

  cdmDatabaseSchema <- clinChar@executionSettings@cdmDatabaseSchema
  sql <- glue::glue("SELECT l.location_id, l.location_source_value
  FROM {cdmDatabaseSchema}.location l") |>
    SqlRender::translate(targetDialect = clinChar@executionSettings@dbms)

  cli::cat_bullet(
    glue::glue("Database Query: {crayon::green('Grab unique locations from CDM')}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  loc_tbl <- DatabaseConnector::querySql(connection = connection, sql = sql)
  names(loc_tbl) <- c("value_id", "value_name")

  return(loc_tbl)
}





summarizeLocation <- function(clinChar, connection) {

  cohortKey <- tibble::tibble(
    cohort_id = as.numeric(clinChar@targetCohort@id),
    cohort_name = clinChar@targetCohort@name
  )

  categoryKey <- tibble::tibble(
    category_id = 105,
    category_name = "location"
  )
  loc_tbl <- grab_locations(clinChar, connection)

  dd <- retrieveTable(clinChar) |>
    dplyr::filter(
      category_id == 105
    ) |>
    dplyr::left_join(loc_tbl, by = "value_id") |>
    dplyr::group_by(cohort_id, category_id, value_id, value_name) |>
    dplyr::summarize(
      sum_value = dplyr::n()
    )|>
    dplyr::ungroup() |>
    dplyr::left_join(cohortKey, by = "cohort_id") |>
    dplyr::left_join(categoryKey, by = "category_id") |>
    dplyr::left_join(
      retrieveCount(clinChar), by = "cohort_id"
    ) |>
    dplyr::mutate(
      sum_value = as.numeric(sum_value),
      average_value = sum_value / total
    ) |>
    dplyr::collect() |>
    dplyr::select(
      cohort_id, cohort_name,
      category_id, category_name, value_id, value_name, sum_value, average_value
    )

  return(dd)

}


# codeset -----------------------

find_domain <- function(es, domain) {
  clin_class <- purrr::map_chr(es, ~class(.x))
  idx <- which(clin_class %in% c("domainChar", "labChar"))
  dd <- es[idx]

  clin_domain <- purrr::map_chr(dd, ~.x@domain)
  dm <- dd[which(clin_domain %in% domain)]
  return(dm)
}

summarizeDomain <- function(clinChar, domain) {

  #get the analysis id
  domain_trans <- domain_translate(domain)

  #merge key for cohort names
  cohortKey <- tibble::tibble(
    cohort_id = as.numeric(clinChar@targetCohort@id),
    cohort_name = clinChar@targetCohort@name
  )

  #merge key for category
  categoryKey <- tibble::tibble(
    category_id = domain_trans$analysis_id,
    category_name = domain
  )

  # find the slot with the domain
  dm <- find_domain(clinChar@extractSettings, domain = "condition_occurrence")

  # merge key for the codeset ids
  codesetKey <- tibble::tibble(
    value_id = as.numeric(seq_along(dm[[1]]@conceptSets)),
    value_name = purrr::map_chr(dm[[1]]@conceptSets, ~.x@Name)
  )

  #merge key for time ids
  timeKey <- tibble::tibble(
    time_id = as.numeric(dm[[1]]@time$time_id),
    time_name = glue::glue("{dm[[1]]@time$time_a}d:{dm[[1]]@time$time_b}d")
  )

  dd <- retrieveTable(clinChar) |>
    dplyr::rename_with(tolower) |>
    dplyr::filter(
      category_id == domain_trans$analysis_id
    ) |>
    dplyr::group_by(cohort_id, category_id, time_id, value_id) |>
    dplyr::summarize(
      sum_value = sum(value)
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(cohortKey, by = "cohort_id") |>
    dplyr::left_join(categoryKey, by = "category_id") |>
    dplyr::left_join(timeKey, by = "time_id") |>
    dplyr::left_join(codesetKey, by = "value_id") |>
    dplyr::left_join(retrieveCount(clinChar), by = "cohort_id") |>
    dplyr::mutate(
      sum_value = as.numeric(sum_value),
      average_value = sum_value / total
    ) |>
    dplyr::collect() |>
    dplyr::collect() |>
    dplyr::select(
      cohort_id, cohort_name,
      category_id, category_name,
      time_id, time_name,
      value_id, value_name,
      sum_value, average_value
    )

  return(dd)
}


grab_concept <- function(clinChar, connection, ids) {

  ids <- paste(ids, collapse = ", ")
  cdmDatabaseSchema <- clinChar@executionSettings@cdmDatabaseSchema
  sql <- glue::glue("SELECT c.concept_id, c.concept_name
  FROM {cdmDatabaseSchema}.concept c WHERE c.concept_id IN ({ids})") |>
    SqlRender::translate(targetDialect = clinChar@executionSettings@dbms)

  cli::cat_bullet(
    glue::glue("Database Query: {crayon::green('Grab concept names from CDM')}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  concept_tbl <- DatabaseConnector::querySql(connection = connection, sql = sql)
  names(concept_tbl) <- c("concept_id", "concept_name")

  return(concept_tbl)
}

decodeLab <- function(x) {

  tb <- tibble::tibble(
    measurement_id = x@labIds,
    unit_id = x@unitIds
  ) |>
    dplyr::mutate(
      value_id = as.numeric((measurement_id * 1000000) + (unit_id - (floor(unit_id/1000) * 1000)))
    )

  return(tb)
}

summarizeLab <- function(clinChar, connection) {


  dm <- find_domain(clinChar@extractSettings, domain = "labs")

  #merge key for cohort names
  cohortKey <- tibble::tibble(
    cohort_id = as.numeric(clinChar@targetCohort@id),
    cohort_name = clinChar@targetCohort@name
  )

  #merge key for category
  categoryKey <- tibble::tibble(
    category_id = 600,
    category_name = "lab"
  )

  #merge key for time ids
  timeKey <- tibble::tibble(
    time_id = as.numeric(dm[[1]]@time$time_id),
    time_name = glue::glue("{dm[[1]]@time$time_a}d:{dm[[1]]@time$time_b}d")
  )

  # merge key for concepts
  ids <- c(dm[[1]]@labIds, dm[[1]]@unitIds)
  concept_tb <- grab_concept(clinChar, connection = connection, ids = ids)
  lb_tb <- decodeLab(dm[[1]]) |>
    dplyr::left_join(
      concept_tb, by = c("measurement_id" = "concept_id")
    ) |>
    dplyr::rename(
      measurement_name = concept_name
    ) |>
    dplyr::left_join(
      concept_tb, by = c("unit_id" = "concept_id")
    ) |>
    dplyr::rename(
      unit_name = concept_name
    )

  dd <- retrieveTable(clinChar) |>
    dplyr::filter(
      category_id == 600
    ) |>
    dplyr::group_by(cohort_id, category_id, time_id, value_id) |>
    continuous_summary() |>
    dplyr::ungroup() |>
    dplyr::left_join(cohortKey, by = "cohort_id") |>
    dplyr::left_join(categoryKey, by = "category_id") |>
    dplyr::left_join(timeKey, by = "time_id") |>
    dplyr::left_join(lb_tb, by = "value_id") |>
    dplyr::collect() |>
    dplyr::select(
      cohort_id, cohort_name,
      category_id, category_name,
      time_id, time_name,
      measurement_id, measurement_name,
      unit_id, unit_name,
      n, mean, sd, min_value, p25_value, median_value,
      p75_value, max_value
    )
  return(dd)
}

# score value
score_value <- function(tbl, scoreKey){
  # join arrow with score key
  tb <- tbl |>
    dplyr::mutate(
      value_id = dplyr::case_when(
        value_id == -999 ~ value,
        TRUE ~ value_id
      )
    ) |>
    dplyr::left_join(scoreKey, by = c("value_id" = "id")) |>
    dplyr::mutate(
      value = dplyr::case_when(
        is.na(w) ~ 0,
        TRUE ~ w
      )
    )|>
    dplyr::select(-w)

  # get score from demographics time invariant
  demoScore <- tb |>
    dplyr::filter(time_id == -999) |>
    dplyr::group_by(cohort_id, subject_id) |>
    dplyr::summarize(
      value = sum(value)
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(
      demo_value = value
    )
  # get score for domain, time varying
  domainScore <- tb |>
    dplyr::filter(time_id != -999) |>
    dplyr::group_by(cohort_id, subject_id, time_id) |>
    dplyr::summarize(
      value = sum(value)
    ) |>
    dplyr::ungroup()

  #determine score for people
  fullScore <- domainScore |>
    dplyr::left_join(demoScore,
                     by = c("cohort_id", "subject_id")) |>
    dplyr::mutate(
      value = value + demo_value
    ) |>
    dplyr::group_by(cohort_id, time_id) |>
    dplyr::summarize(
      n = dplyr::n_distinct(subject_id),
      mean = mean(value),
      sd = sd(value),
      min_value = min(value),
      p25_value = quantile(value, probs = c(0.25)),
      median_value = median(value),
      p75_value = quantile(value, probs = c(0.75)),
      max_value = max(value)
    ) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    dplyr::mutate(
      value_id = -999,
      category_id = -999
    )

  return(fullScore)

}
