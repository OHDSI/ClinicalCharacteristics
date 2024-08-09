-- Make query for index year
INSERT INTO @dataTable (cohort_id, subject_id, category_id, time_id, value_id, value)
SELECT
  t.cohort_definition_id AS cohort_id,
  t.subject_id,
  {ordinal} AS category_id,
  -999 AS time_id,
  YEAR(t.cohort_start_date) AS value_id,
  1 AS value
FROM @targetTable t
;
