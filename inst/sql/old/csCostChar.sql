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
-- Find matching {domain} covariates
  SELECT
    t.cohort_definition_id AS cohort_id,
    t.subject_id,
    t.cohort_start_date,
    tw.time_id,
    cs.codeset_id AS value_id,
    d.{domain_trans$record_id},
    d.{domain_trans$concept_id},
    d.{domain_trans$event_date},
    cc.{x@costType} /* get cost column*/
  FROM {{targetTable}} a
  JOIN {{cdmDatabaseSchema}}.{domain} d ON a.subject_id = d.person_id
  JOIN T1 cs on (d.{domain_trans$concept_id} = cs.concept_id)
  JOIN {{cdmDatabaseSchema}}.cost cc ON d.{domain_trans$record_id} = cc.cost_event_id
  INNER JOIN T0 tw
        ON DATEADD(day, tw.time_a, a.cohort_start_date) <= d.{domain_trans$event_date}
        AND DATEADD(day, tw.time_b, a.cohort_start_date) >= d.{domain_trans$event_date}
  WHERE d.{domain_trans$concept_id} <> 0
  {conceptTypeSql}
  AND {x@costType} >= 0
)
/* Sum cost from occurrences*/
SELECT
  d.cohort_definition_id,
  d.subject_id,
  d.time_id,
  d.value_id,
  FLOOR(SUM(d.{x@costType})) AS value
INTO {x@tempTables$cost}
FROM T2 d
GROUP BY d.cohort_definition_id, d.subject_id, d.time_id, d.value_id
;

/* Insert cost into data table*/
INSERT INTO @dataTable (cohort_id, subject_id, category_id, time_id, value_id, value)
SELECT
  cohort_definition_id AS cohort_id,
  subject_id,
  {x@orderId} AS category_id,
  time_id,
  value_id,
  value
FROM {x@tempTables$cost}
;
