INSERT INTO @categorical_table
SELECT
  t2.target_cohort_id,
  t2.ordinal_id,
  t2.time_label,
  CONCAT(t2.line_item_label, ': ', t2.break_id) AS line_item_label,
  t2.patient_line,
  'breaks' AS statistict_type,
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
    t1.break_id,
    t1.tot_subjects,
    COUNT(DISTINCT SUBJECT_ID) AS subject_count
  FROM #pat_ts_break t1
  GROUP BY target_cohort_id, ordinal_id, time_label, line_item_label, patient_line, break_id, tot_subjects
) t2

;
