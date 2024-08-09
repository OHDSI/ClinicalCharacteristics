-- Charlson Score Sql
WITH T1 AS (
  -- Get category ids corresponding to charlson age and condition presence
  SELECT cohort_id, subject_id, category_id, time_id,
  CAST(CASE WHEN category_id = {ageId} THEN value ELSE value_id END AS int) AS value_id,
  value
  FROM {{dataTable}} WHERE category_id IN ({ageId}, {{scoreId}})
),
T2 AS (
SELECT a.cohort_id, a.subject_id, a.category_id, a.time_id, a.value_id,
  CASE WHEN b.w IS NULL THEN 0 ELSE b.w END AS value
FROM T1 a
LEFT JOIN {{scoreTable}} b ON CAST(a.value_id AS INT) = CAST(b.id AS INT)
),
age AS (
  SELECT cohort_id, subject_id, SUM(value) AS age_score
  FROM T2
  WHERE category_id = {ageId}
  GROUP BY cohort_id, subject_id
),
cond AS (
  SELECT cohort_id, subject_id, time_id, SUM(value) AS cond_score
  FROM T2
  WHERE category_id = {{scoreId}}
  GROUP BY cohort_id, subject_id, time_id
)
SELECT dd.cohort_id, dd.subject_id,
  {{newId}} AS category_id,
  dd.time_id,
  -999 AS value_id,
  dd.cond_score + ag.age_score AS value
INTO {{score_dat_tmp}}
FROM cond dd
JOIN age ag ON ag.cohort_id = dd.cohort_id AND ag.subject_id = dd.subject_id
;
