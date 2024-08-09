# UI functions -----------------
addAgeChar <- function(clinChar) {
  clinChar@extractSettings <- append(clinChar@extractSettings, new("ageChar"))
  return(clinChar)
}


addDemoConceptChar <- function(clinChar, demoTypes) {
  
  demoChar <- purrr::map(demoTypes, ~new("demoConceptChar", type = .x))
  clinChar@extractSettings <- append(clinChar@extractSettings, demoChar)
  return(clinChar)
}


demo_translate <- function(demographic) {
  tt <- switch(demographic,
               "gender" = list('concept_id' ="gender_concept_id",
                               'category_id' = "gender"),
               "race" = list('concept_id' = "race_concept_id",
                             'category_id' = "race"),
               "ethnicity" = list('concept_id' = "ethnicity_concept_id",
                                  'category_id' = "ethnicity")
  )
  return(tt)
}

get_person_age <- function(cdmDatabaseSchema,
                           targetTable,
                           dataTable,
                           dbms) {
  
  sql <- glue::glue(
    "INSERT INTO {dataTable} (cohort_id, subject_id, category_id, time_id, value_id, value)
     SELECT t.cohort_definition_id AS cohort_id, t.subject_id,
     'age' AS category_id,
     -999 AS time_id,
     -999 AS value_id,
     YEAR(t.cohort_start_date) - d.year_of_birth AS value
     FROM {targetTable} t
     JOIN {cdmDatabaseSchema}.person d
     ON t.subject_id = d.person_id;"
  ) |>
    SqlRender::translate(
      targetDialect = dbms
    )
  return(sql)
}


get_person_concept <- function(cdmDatabaseSchema,
                               demographic,
                               dataTable,
                               dbms) {
  
  demo_trans <- demo_translate(demographic)
  
  sql <- glue::glue(
    "INSERT INTO {dataTable} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT t.cohort_definition_id AS cohort_id, t.subject_id,
     '{demo_trans$category_id}' AS category_id,
     -999 AS time_id,
     d.{demo_trans$concept_id} AS value_id,
     1 AS value
     FROM {targetTable} t
     JOIN {cdmDatabaseSchema}.person d
     ON t.subject_id = d.person_id;"
  ) |>
    SqlRender::translate(
      targetDialect = dbms
    )
  return(sql)
}

get_location <- function(cdmDatabaseSchema,
                         targetTable,
                         location_column,
                         dataTable,
                         dbms) {
  sql <- glue::glue("
  WITH location AS (
    SELECT t.*, d.location_id
    FROM {targetTable} t
    JOIN {cdmDatabaseSchema}.person d
    ON t.subject_id = d.person_id
  )
  SELECT l.cohort_definition_id AS cohort_id,
  l.subject_id,
  'location' AS category_id,
  -999 AS time_id,
  b.{location_column} AS value_id,
  1 AS value
  INTO {dataTable}
  FROM location l
  JOIN {cdmDatabaseSchema}.location b
  ON l.location_id = b.location_id
  ;") |>
    SqlRender::translate(
      targetDialect = dbms
    )
  return(sql)
}