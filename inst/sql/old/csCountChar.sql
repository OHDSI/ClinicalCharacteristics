WITH T0 AS (
  /* Get time windows */
  SELECT * FROM @timeWindowTable tw
  WHERE time_a IN ({time_a}) AND time_b IN ({time_b})
),
T1 AS (
  /* Get codesets */
  SELECT * FROM @codesetTable cs
  WHERE codeset_id IN ({codesetIds})
),
T2 AS (
  /* Find matching {domain} covariates*/
  SELECT
    t.cohort_definition_id AS cohort_id,
    t.subject_id,
    t.cohort_start_date,
    tw.time_id,
    cs.codeset_id AS value_id,
    d.{domain_trans$record_id},
    d.{domain_trans$concept_id},
    d.{domain_trans$event_date}
  FROM @targetTable t
  JOIN @cdmDatabaseSchema.{domain} d ON t.subject_id = d.person_id
  JOIN T2 cs on (d.{domain_trans$concept_id} = cs.concept_id)
  INNER JOIN T1 tw
        ON DATEADD(day, tw.time_a, t.cohort_start_date) <= d.{domain_trans$event_date}
          AND DATEADD(day, tw.time_b, t.cohort_start_date) >= d.{domain_trans$event_date}
  {conceptTypeSql}
  {sourceConceptSql}
)
/* Count number of occurrences*/
SELECT
  d.cohort_definition_id,
  d.subject_id,
  d.time_id,
  d.value_id,
  COUNT(d.{domain_trans$record_id}) AS value
INTO {x@tempTables$count}
FROM T2 d
GROUP BY d.cohort_definition_id, d.subject_id, d.time_id, d.value_id
;

/* Insert count into data table*/
INSERT INTO @dataTable (cohort_id, subject_id, category_id, time_id, value_id, value)
SELECT
  cohort_definition_id AS cohort_id,
  subject_id,
  {x@orderId} AS category_id,
  time_id,
  value_id,
  value
FROM {x@tempTables$count}
;
