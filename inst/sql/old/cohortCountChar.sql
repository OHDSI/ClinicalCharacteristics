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
),
T3 AS (
/* Find presence of covariate in {domain}*/
  SELECT
    t.cohort_definition_id AS cohort_id,
    t.subject_id,
    t.cohort_start_date,
    tw.time_id,
    ch.cohort_definition_id AS value_id,
    ch.cohort_start_date AS record_id
  FROM @targetTable t
  INNER JOIN T2 ch ON t.subject_id = ch.person_id
  INNER JOIN T1 tw
      ON ch.cohort_start_date <= DATEADD(day, tw.time_b, t.cohort_start_date)
  WHERE ch.cohort_end_date >= DATEADD(day, tw.time_a, t.cohort_start_date)
)
SELECT
  d.cohort_id,
  d.subject_id,
  d.time_id,
  d.value_id,
  COUNT(DISTINCT d.record_id) AS value
INTO {x@tempTables$count}
FROM T3 d
GROUP BY d.cohort_id, d.subject_id, d.time_id, d.value_id
;

/* Insert count into data table*/
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
