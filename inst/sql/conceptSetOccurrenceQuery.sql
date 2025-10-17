/* Find presence of concept set covariate in {domain}*/

SELECT
  t.cohort_definition_id AS target_cohort_id,
  t.subject_id,
  t.cohort_start_date,
  t.cohort_end_date,
  '{domain}' AS domain_table,
  tw.time_label,
  cs.codeset_id AS raw_occurrence_id,
  'codeset_id' AS raw_occurrence_description,
  {domainTranslation$event_date} AS event_date
FROM @target_cohort_table t
JOIN @cdm_database_schema.{domain} d ON t.subject_id = d.person_id
JOIN (
  /* Get codesets */
  SELECT *
  FROM @codeset_table cc
  WHERE codeset_id IN ({codeset_ids})
) cs ON (d.{domainTranslation$concept_id} = cs.concept_id)
INNER JOIN (
  /* Get time windows */
  SELECT *
  FROM @time_window_table tt
  WHERE time_label IN ('{time_labels}')
) tw ON DATEADD(day, tw.time_a, t.cohort_start_date) <= d.{domainTranslation$event_date}
  AND DATEADD(day, tw.time_b, t.cohort_start_date) >= d.{domainTranslation$event_date}

