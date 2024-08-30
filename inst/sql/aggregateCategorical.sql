WITH T1 AS (
  SELECT * FROM @dataTable WHERE category_id IN ({categoricalIds})
),
T2 AS (
  SELECT cohort_definition_id AS cohort_id, COUNT(subject_id) AS tot
  FROM @workDatabaseSchema.@cohortTable
  GROUP BY cohort_definition_id
),
T3 AS (
  SELECT
    cohort_id, category_id, time_id, value_id,
    COUNT(subject_id) AS n
  FROM T1
  GROUP BY cohort_id, category_id, time_id, value_id
),
T4 AS (
  SELECT
    a.cohort_id, a.category_id, a.time_id, a.value_id, a.n,
    ((1.0 * a.n) / (1.0 * b.tot))  AS pct
  FROM T3 a
  JOIN T2 b ON a.cohort_id = b.cohort_id
)
SELECT
cohort_id, category_id, time_id, value_id, n, pct
FROM T4
;
