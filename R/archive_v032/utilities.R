# verbose version of execute
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

pluck_domain_char <- function(clinChar) {
  es <- clinChar@extractSettings
  clin_class <- purrr::map_chr(es, ~class(.x))
  idx <- which(clin_class %in% c("presenceChar", "labChar", "countChar", "costChar",
                                 "timeToChar", "visitDetailChar"))
  dd <- es[idx]
  return(dd)
}


domain_translate <- function(domain) {
  tt <- switch(domain,
               "condition_occurrence" = list(
                 'record_id' = "condition_occurrence_id",
                 'concept_id' ="condition_concept_id",
                 'concept_type_id' = "condition_type_concept_id",
                 'event_date' = "condition_start_date",
                 'source_concept_id' = "condition_source_concept_id"
               ),
               "drug_exposure" = list(
                 'record_id' = "drug_exposure_id",
                 'concept_id' = "drug_concept_id",
                 'concept_type_id' = "drug_type_concept_id",
                 'event_date' = "drug_exposure_start_date",
                 'source_concept_id' = "drug_source_concept_id"
               ),
               "procedure_occurrence" = list(
                 'record_id' = "procedure_occurrence_id",
                 'concept_id' = "procedure_concept_id",
                 'concept_type_id' = "procedure_type_concept_id",
                 'event_date' = "procedure_date",
                 'source_concept_id' = "procedure_source_concept_id"
               ),
               "observation" = list(
                 'record_id' = "observation_id",
                 'concept_id' = "observation_concept_id",
                 'concept_type_id' = "observation_type_concept_id",
                 'event_date' = "observation_date",
                 'source_concept_id' = "observation_source_concept_id"
               ),
               "device_exposure" = list(
                 'record_id' = "device_exposure_id",
                 'concept_id' = "device_concept_id",
                 'concept_type_id' = "device_type_concept_id",
                 'event_date' = "device_exposure_start_date",
                 'source_concept_id' = "device_source_concept_id"
               ),
               "measurement" = list(
                 'record_id' = "measurement_id",
                 'concept_id' = "measurement_concept_id",
                 'concept_type_id' = "measurement_type_concept_id",
                 'event_date' = "measurement_date",
                 'source_concept_id' = "measurement_source_concept_id"
               ),
               "visit_occurrence" = list(
                 'record_id' = "visit_occurrence_id",
                 'concept_id' = "visit_concept_id",
                 'concept_type_id' = "visit_type_concept_id",
                 'event_date' = "visit_start_date",
                 'source_concept_id' = "visit_source_concept_id"
               ),
               "provider" = list(
                 'concept_id' = "specialty_concept_id",
                 'merge_key' = "provider_id"
               ),
               "care_site" = list(
                 'concept_id' = "place_of_service_concept_id",
                 'merge_key' = "care_site_id"
               ),
               "gender" = list('concept_id' ="gender_concept_id"),
               "race" = list('concept_id' = "race_concept_id"),
               "ethnicity" = list('concept_id' = "ethnicity_concept_id")
  )
  return(tt)
}


find_char <- function(clinChar, type = c("Age")) {
  type <- match.arg(type)
  type <- tolower(type)
  cls <- purrr::map_chr(clinChar@extractSettings, ~.x@domain)
  idx <- which(type %in% cls)
  return(idx)
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


cost_categories <- function() {
  tibble::tribble(
    ~value_id, ~value_name,
    44818668, "USD",
    44818568, "EUR",
    44818571, "GBP"
  )
}

time_in_label <- function() {
  tibble::tribble(
    ~value_id, ~value_name,
    1001, "Time In Cohort",
    9201000262, "Length of Inpatient Stay"
  )
}

count_label <- function(domain) {
  lb <- switch(domain,
               'drug_exposure' = "medication count",
               'procedure_occurrence' = "procedure count",
               'measurement' = "measurement count",
               'condition_occurrence' = "condition count",
               'visit_occurrence' = "visit count")
  return(lb)
}

#' SQL for source concept WHERE clause
source_concept_sql <- function(domain, sourceConcepts) {

  domain_trans <- domain_translate(domain)
  if (!all(is.na(sourceConcepts))) {
    sourceConcepts <- paste(sourceConcepts, collapse = ", ")
    sourceConceptSql <- glue::glue(
      "AND {domain_trans$source_concept_id} IN ({sourceConcepts})"
    )
  } else {
    sourceConceptSql <- ""
  }

  return(sourceConceptSql)
}


#
# domain_lbl <- function() {
#   tibble::tribble(
#     ~domain_id, ~domain_name,
#     1001, "age",
#     1002, "gender",
#     1003, "race",
#     1004, "ethnicity",
#     1005, "year",
#     1006, "location",
#     1007, "time_in_cohort",
#     2001, "condition_presence",
#     2002, "condition_count",
#     2004, "condition_timeTo",
#     3001, "drug_presence",
#     3002, "drug_count",
#     3003, "drug_cost",
#     3004, "drug_timeTo",
#     4001, "procedure_presence",
#     4002, "procedure_count",
#     4003, "procedure_cost",
#     4004, "procedure_timeTo",
#     5001, "measurement_presence",
#     5002, "measurement_count",
#     5004, "measurement_timeTo",
#     5005, "labs",
#     6001, "observation_presence",
#     6002, "observation_count",
#     6004, "observation_timeTo",
#     7001, "device_presence",
#     8001, "visit_presence",
#     8002, "visit_count",
#     8003, "visit_cost",
#     8004, "visit_timeTo",
#     8005, "time_in_inpatient",
#     9001, "provider_specialty"
#   )
# }
