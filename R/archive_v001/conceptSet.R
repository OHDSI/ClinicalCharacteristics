
domain_translate <- function(domain) {
  tt <- switch(domain,
               "condition_occurrence" = list('concept_id' ="condition_concept_id",
                                             'event_date' = "condition_start_date",
                                             'analysis_id' = 200),
               "drug_exposure" = list('concept_id' = "drug_concept_id",
                                      'event_date' = "drug_start_date",
                                      'analysis_id' = 300),
               "procedure_occurrence" = list('concept_id' = "procedure_concept_id",
                                             'event_date' = "procedure_date",
                                             'analysis_id' = 400),
               "measurement" = list('concept_id' = "measurement_concept_id",
                                    'event_date' = "measurement_date",
                                    'analysis_id' = 500)
  )
  return(tt)
}

get_codeset_occurrences <- function(cdmDatabaseSchema,
                                    domainTable,
                                    codesetTable,
                                    targetTable,
                                    timeTable,
                                    domain,
                                    dbms) {
  
  domain_trans <- domain_translate(domain)
  sql <- glue::glue(
    "SELECT
      t.cohort_definition_id AS cohort_id,
      t.subject_id, t.cohort_start_date,
      tp.time_id,
      cs.codeset_id AS value_id, d.{domain_trans$event_date}
     INTO {domainTable}
     FROM {targetTable} t
     JOIN {cdmDatabaseSchema}.{domain} d ON t.subject_id = d.person_id
     JOIN {codesetTable} cs on (d.{domain_trans$concept_id} = cs.concept_id)
     INNER JOIN {timeTable} tp
          ON DATEADD(day, tp.time_a, t.cohort_start_date) <= d.{domain_trans$event_date}
          AND DATEADD(day, tp.time_b, t.cohort_start_date) >= d.{domain_trans$event_date}
     ;"
  ) |>
    SqlRender::translate(
      targetDialect = dbms
    )
  return(sql)
}

get_domain <- function(dataTable, domain, domainTable, dbms) {
  
  
  domain_trans <- domain_translate(domain)
  
  sql <- glue::glue(
    "
    -- Find matching covariates
    SELECT
      t.cohort_definition_id AS cohort_id,
      t.subject_id, t.cohort_start_date,
      tp.time_id,
      cs.codeset_id AS value_id, d.{domain_trans$event_date}
     INTO {{domainTable}}
     FROM {{targetTable}} t
     JOIN {{cdmDatabaseSchema}}.{domain} d ON t.subject_id = d.person_id
     JOIN {{codesetTable}} cs on (d.{domain_trans$concept_id} = cs.concept_id)
     INNER JOIN {timeTable} tp
          ON DATEADD(day, tp.time_a, t.cohort_start_date) <= d.{domain_trans$event_date}
          AND DATEADD(day, tp.time_b, t.cohort_start_date) >= d.{domain_trans$event_date}
     ;
    
    -- Make {domain} query
    INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT cohort_id, subject_id,
    {domain_trans$analysis_id} AS category_id,
    time_id,
    value_id,
    1 AS value
    FROM (
      SELECT
        d.*,
        ROW_NUMBER() OVER (PARTITION BY d.subject_id, d.time_id, d.value_id order by d.{domain_trans$event_date}) as ordinal
      FROM {domainTable} d
    )
    WHERE ordinal = 1
    ;") 
  
  return(sql)
}
