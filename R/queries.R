# Helpers -------------------------------

# insert data to dat table
insert_into_dat <- function(insert_query) {
  sql <- glue::glue(
    "/*Insert into data table */
    INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
    {insert_query}
    ;
    ")
  return(sql)
}

# query to select time windows
select_time_window <- function(char) {

  time_a <- paste(char@time$time_a, collapse = ", ")
  time_b <- paste(char@time$time_b, collapse = ", ")

  sql <- glue::glue(
    "time_windows AS (
      -- Get appropriate time windows
      SELECT * FROM {{timeWindowTable}} tw
      WHERE time_a IN ({time_a}) AND time_b IN ({time_b})
    )"
  )

  return(sql)
}

# query to select codesets
select_codesets <- function(char) {

  codesetIds <- paste(char@tempTables$codeset, collapse = ", ")

  sql <- glue::glue(
    "code_sets AS(
       -- Get appropriate codeset ids
      SELECT * FROM {{codesetTable}} cs
      WHERE codeset_id IN ({codesetIds})
    )"
  )
  return(sql)
}

# concept type query
concept_type_sql <- function(char) {

  # pull components from char
  domain <- char@domain
  conceptType <- char@conceptType

  # translate to appropriate domain
  domain_trans <- domain_translate(domain)

  # if conceptType present than add a filter
  if (!all(is.na(conceptType))) {
    conceptType <- paste(conceptType, collapse = ", ")
    conceptTypeSql <- glue::glue(
      "AND {domain_trans$concept_type_id} IN ({conceptType})"
    )

  } else{
    conceptTypeSql <- ""
  }

  return(conceptTypeSql)

}

# source concept query
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
  #TODO what if this is the first WHERE statement

  return(sourceConceptSql)
}

# add join on codeset if needed
codeset_sql <- function(domain, conceptSets) {
  concept_id_col <- domain_translate(domain)$concept_id
  if (!is.null(conceptSets)) {
    codesetJoinSql <- glue::glue(
      "JOIN code_sets cs on (d.{concept_id_col} = cs.concept_id)"
    )
  } else {
    codesetJoinSql <- ""
  }
  return(codesetJoinSql)
}


# Char specific queries -----------------------------

## Demographics ------------------------------

age_sql <- function(orderId) {
  sql <- glue::glue(
    "-- Query Age per patient
    SELECT t.cohort_definition_id AS cohort_id, t.subject_id,
      {orderId} AS category_id,
      -999 AS time_id,
      -999 AS value_id,
      YEAR(t.cohort_start_date) - d.year_of_birth AS value
    FROM {{targetTable}} t
    JOIN {{cdmDatabaseSchema}}.person d
     ON t.subject_id = d.person_id
    "
  )
  return(sql)
}

year_sql <- function(orderId) {

  sql <- glue::glue(
    "-- Query index year
    SELECT t.cohort_definition_id AS cohort_id, t.subject_id,
     {orderId} AS category_id,
     -999 AS time_id,
     YEAR(t.cohort_start_date) AS value_id,
     1 AS value
     FROM {{targetTable}} t
    "
  )
  return(sql)

}

demo_concept_sql <- function(domain, orderId) {
  demo_trans <- domain_translate(domain)
  sql <- glue::glue(
    "-- Query {domain} demographic
    SELECT t.cohort_definition_id AS cohort_id, t.subject_id,
      {orderId} AS category_id,
      -999 AS time_id,
      d.{demo_trans$concept_id} AS value_id,
      1 AS value
     FROM {{targetTable}} t
     JOIN {{cdmDatabaseSchema}}.person d
      ON t.subject_id = d.person_id
    "
  )
  return(sql)
}


location_sql <- function(orderId) {
  sql <- glue::glue(
    "-- Query Location
    SELECT t.cohort_definition_id AS cohort_id, t.subject_id,
      {orderId} AS category_id,
      -999 AS time_id,
      d.location_id AS value_id,
      1 AS value
     FROM {{targetTable}} t
     JOIN {{cdmDatabaseSchema}}.person d
      ON t.subject_id = d.person_id
     WHERE d.location_id IS NOT NULL
    "
  )
  return(sql)
}


domain_sql <- function(char) {

  # get options
  domain <- char@domain
  domain_tbl <- char@tempTables[1]
  event_date_col <- domain_translate(domain)$event_date
  codesetJoinSql <- codeset_sql(domain, char@conceptSets)

  sql <- glue::glue(
    "-- {domain} query
    SELECT
      t.cohort_definition_id AS cohort_id,
      t.subject_id, t.cohort_start_date,
      tw.time_id,
      cs.codeset_id AS value_id,
      d.{event_date_col}
     INTO {domain_tbl}
     FROM {{targetTable}} t
     JOIN {{cdmDatabaseSchema}}.{domain} d ON t.subject_id = d.person_id
    {codesetJoinSql}
     INNER JOIN T1 tw
          ON DATEADD(day, tw.time_a, t.cohort_start_date) <= d.{event_date_col}
          AND DATEADD(day, tw.time_b, t.cohort_start_date) >= d.{event_date_col}
    "
  )
  return(sql)

}
