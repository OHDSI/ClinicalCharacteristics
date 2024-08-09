WITH T1 AS (
  /* Get time windows */
  SELECT * FROM @timeWindowTable tw
  WHERE time_a IN ({time_a}) AND time_b IN ({time_b})
),

labs AS (
  /* Find matching lab values */
  SELECT
    M.person_id,
    M.measurement_concept_id,
    M.measurement_date,
    M.value_as_number,
    M.unit_concept_id,
    CAST((CAST(M.measurement_concept_id AS BIGINT) * 1000000) + (M.unit_concept_id - (FLOOR(M.unit_concept_id / 1000) * 1000)) AS BIGINT) AS value_id
  FROM @cdmDatabaseSchema.measurement M
  WHERE M.measurement_concept_id IN ({labIds})
  AND M.value_as_number is not null
  AND M.unit_concept_id IN ({unitIds})
)
SELECT
  t.cohort_definition_id AS cohort_id,
  t.subject_id,
  t.cohort_start_date,
  tw.time_id,
  l.value_id,
  l.value_as_number AS value,
  l.measurement_date
INTO {x@tempTables$lab}
FROM @targetTable t
JOIN labs l ON t.subject_id = l.person_id
INNER JOIN T1 tw
    ON DATEADD(day, tw.time_a, t.cohort_start_date) <= l.measurement_date
    AND DATEADD(day, tw.time_b, t.cohort_start_date) >= l.measurement_date
;

/* Insert count into data table*/
INSERT INTO @dataTable (cohort_id, subject_id, category_id, time_id, value_id, value)
SELECT
  i.cohort_id,
  i.subject_id,
  {x@orderId} AS category_id,
  i.time_id,
  i.value_id,
  i.value
FROM (
  {limit_sql(x)}
) i
;
