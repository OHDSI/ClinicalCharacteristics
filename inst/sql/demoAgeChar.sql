-- Make query for age
INSERT INTO @dataTable (cohort_id, subject_id, category_id, time_id, value_id, value)
SELECT
  t.cohort_definition_id AS cohort_id,
  t.subject_id,
  {ordinal} AS category_id,
  -999 AS time_id,
  -999 AS value_id,
  YEAR(t.cohort_start_date) - d.year_of_birth AS value
FROM @targetTable t
JOIN @cdmDatabaseSchema.person d
ON t.subject_id = d.person_id
;
