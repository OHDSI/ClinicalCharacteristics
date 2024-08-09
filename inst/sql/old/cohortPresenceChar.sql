-- WORK IN PROGRESS

WITH T1 AS (
  /* Get time windows */
  SELECT *
  FROM @timeWindowTable tw
  WHERE time_a IN ({time_a}) AND time_b IN ({time_b})
),
T2 AS (
  /* Get cohorts */
  SELECT *
  FROM @workDatabaseSchema.@cohortTable ch
  WHERE cohort_definition_id IN ({cohortIds})
)
/* Find presence of covariate in {domain}*/
SELECT
  t.cohort_definition_id AS cohort_id,
  t.subject_id,
  t.cohort_start_date,
  tw.time_id,
  ch.cohort_definition_id AS value_id
INTO {x@tempTables$cohort}
FROM @targetTable t
INNER JOIN T2 ch ON t.subject_id = ch.person_id
INNER JOIN T1 tw
      ON ch.cohort_start_date <= DATEADD(day, tw.time_b, t.cohort_start_date)
WHERE ch.cohort_end_date >= DATEADD(day, tw.time_a, t.cohort_start_date)
;

/* Insert count into data table*/
INSERT INTO @dataTable (cohort_id, subject_id, category_id, time_id, value_id, value)
SELECT
  i.cohort_id,
  i.subject_id,
  {x@orderId} AS category_id,
  i.time_id,
  i.value_id,
  1 AS value
FROM (
  {limit_sql(x)}
) i
;
