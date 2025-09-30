INSERT INTO @categorical_table
SELECT
  t2.target_cohort_id,
  t2.ordinal_id,
  t2.time_label,
  t2.line_item_label,
  t2.patient_line,
  'scoreTransformation' AS statistic_type,
  t2.subject_count,
  t2.tot_subjects,
  t2.subject_count / t2.tot_subjects AS pct
FROM (
  SELECT
    t1.target_cohort_id,
    t1.ordinal_id,
    t1.time_label,
    t1.line_item_label,
    t1.patient_line,
    t1.tot_subjects,
    COUNT(DISTINCT SUBJECT_ID) AS subject_count
  FROM (
      SELECT p.*
      FROM @pat_ts_tab p
      WHERE p.statistic_type = 'scoreTransformation'
  ) t1
  GROUP BY target_cohort_id, ordinal_id, time_label, line_item_label, patient_line, tot_subjects
) t2
;


INSERT INTO @continuous_table
SELECT
    t.target_cohort_id,
    t.ordinal_id,
    t.time_label,
    t.line_item_label,
    t.patient_line,
    t.statistic_type,
    t.subject_count,
    t.mean,
    CASE WHEN t.sd IS NULL THEN -5 ELSE t.sd END AS sd,
    t.min,
    t.p10,
    t.p25,
    t.median,
    t.p75,
    t.p90,
    t.max
FROM (
  SELECT
    m.target_cohort_id,
    9999 AS ordinal_id,
    m.time_label,
    m.section_label AS line_item_label,
    m.patient_line,
    m.statistic_type,
    COUNT(DISTINCT subject_id) AS subject_count,
    AVG(m.charlson_score) As mean,
    STDDEV(m.charlson_score) AS sd,
    min(m.charlson_score) AS min,
    PERCENTILE_CONT(0.10) WITHIN GROUP (ORDER BY m.charlson_score) as p10,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY m.charlson_score) as p25,
    PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY m.charlson_score) as median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY m.charlson_score) as p75,
    PERCENTILE_CONT(0.90) WITHIN GROUP (ORDER BY m.charlson_score) as p90,
    max(m.charlson_score) AS max
  FROM (
      SELECT * FROM #pat_ts_score
  ) m
  GROUP BY target_cohort_id, time_label, patient_line, section_label, statistic_type
) t
;

