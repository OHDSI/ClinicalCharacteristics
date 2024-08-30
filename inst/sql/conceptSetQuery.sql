/* Query for a Concept Set */

WITH T1 AS (
  /* Get time windows */
  SELECT * FROM @timeWindowTable tw
  WHERE time_id IN ({timeIds})
),
T2 AS (
  /* Get codesets */
  SELECT * FROM @codesetTable cs
  WHERE codeset_id IN ({codesetIds})
)
/* Find presence of covariate in {domain}*/
SELECT
  t.cohort_definition_id AS cohort_id,
  t.subject_id,
  t.cohort_start_date,
  tw.time_id,
  cs.codeset_id AS value_id,
  d.{domain_trans$event_date} AS record_date
INTO {csTempTableName}
FROM @targetTable t
JOIN @cdmDatabaseSchema.{domain} d ON t.subject_id = d.person_id
JOIN T2 cs on (d.{domain_trans$concept_id} = cs.concept_id)
INNER JOIN T1 tw
      ON DATEADD(day, tw.time_a, t.cohort_start_date) <= d.{domain_trans$event_date}
      AND DATEADD(day, tw.time_b, t.cohort_start_date) >= d.{domain_trans$event_date}
;
