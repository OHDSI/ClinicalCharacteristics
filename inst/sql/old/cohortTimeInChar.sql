-- Make time in {domain} query
INSERT INTO @dataTable (cohort_id, subject_id, category_id, time_id, value_id, value)
SELECT
  t.cohort_definition_id AS cohort_id,
  t.subject_id,
  {x@orderId} AS category_id,
  -999 AS time_id,
  1001 AS value_id,
  DATEDIFF(day, t.cohort_start_date, t.cohort_end_date) AS value
FROM @targetTable t
;
