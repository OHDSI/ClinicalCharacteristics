WITH T1 AS (
  /* Get time windows */
  SELECT * FROM @timeWindowTable tw
  WHERE time_a IN ({time_a}) AND time_b IN ({time_b})
),
T2 AS (
  /* Get codesets */
  SELECT * FROM @codesetTable cs
  WHERE codeset_id IN ({codesetIds})
)
/* Find time to covariate in {domain}*/
SELECT
  t.cohort_definition_id AS cohort_id,
  t.subject_id,
  t.cohort_start_date,
  tw.time_id,
  cs.codeset_id AS value_id,
  d.{domain_trans$event_date},
  DATEDIFF(day, t.cohort_start_date, d.{domain_trans$event_date}) AS value /*time difference*/
INTO {x@tempTables$duration}
FROM @targetTable t
JOIN @cdmDatabaseSchema.{domain} d ON t.subject_id = d.person_id
JOIN T2 cs on (d.{domain_trans$concept_id} = cs.concept_id)
INNER JOIN T1 tw
      ON DATEADD(day, tw.time_a, t.cohort_start_date) <= d.{domain_trans$event_date}
      AND DATEADD(day, tw.time_b, t.cohort_start_date) >= d.{domain_trans$event_date}
{conceptTypeSql}
{sourceConceptSql}
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
