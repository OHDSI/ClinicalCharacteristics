/* Presence Stat transformation */
INSERT INTO @dataTable (cohort_id, subject_id, category_id, time_id, value_id, value)
SELECT
  a.cohort_id,
  a.subject_id,
  CAST('{{catId}}' AS INT) AS category_id,
  --ordinal_id,
  --aggregation_type,
  a.time_id,
  a.value_id,
  1 AS value
FROM (
  SELECT
    d.cohort_id,
    d.subject_id,
    d.time_id,
    d.value_id,
    COUNT(DISTINCT d.record_date) AS num
  FROM {{csTempTableName}} d
  GROUP BY d.cohort_id, d.subject_id, d.time_id, d.value_id
) a
WHERE a.NUM {op} {occurrences}
;