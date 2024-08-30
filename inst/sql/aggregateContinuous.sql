WITH T1 AS (
  SELECT * FROM @dataTable WHERE category_id IN ({continuousIds})
)
SELECT
cohort_id, category_id, time_id, value_id,
  COUNT(subject_id) AS N,
  SUM(value) As occ_cnt,
  STDDEV(value) AS sd,
  min(value) AS min,
  PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY value) as p25,
  PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY value) as median,
  PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY value) as p75,
  max(value) AS max
FROM T1
GROUP BY cohort_id, category_id, time_id, value_id
;
