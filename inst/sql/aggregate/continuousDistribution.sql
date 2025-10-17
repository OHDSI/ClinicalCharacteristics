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
    m.ordinal_id,
    m.time_label,
    m.line_item_label,
    m.patient_line,
    m.statistic_type,
    COUNT(DISTINCT subject_id) AS subject_count,
    AVG(m.value) As mean,
    STDDEV(m.value) AS sd,
    min(m.value) AS min,
    PERCENTILE_CONT(0.10) WITHIN GROUP (ORDER BY m.value) as p10,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY m.value) as p25,
    PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY m.value) as median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY m.value) as p75,
    PERCENTILE_CONT(0.90) WITHIN GROUP (ORDER BY m.value) as p90,
    max(m.value) AS max
  FROM (
    /* Provide option for distribution of events for all patients */
    SELECT
        t.target_cohort_id, t.subject_id, t.ordinal_id, t.time_label, t.line_item_label,
        t.person_line_transformation AS patient_line, t.statistic_type,
        CASE WHEN m1.value IS NULL THEN 0 ELSE m1.value END AS value
    FROM (
        SELECT * FROM (
            SELECT
                t1.cohort_definition_id as target_cohort_id, t1.subject_id, d.*
            FROM (SELECT DISTINCT cohort_definition_id, subject_id FROM scratch_lavallem_rwesnow_schema.eggf9x8mtarget_cohorts) t1
            CROSS JOIN #ts_meta d
        )
        WHERE statistic_type = 'continuousDistribution'
    ) t
    LEFT JOIN (SELECT * FROM #pat_ts_tab WHERE statistic_type = 'continuousDistribution') m1
    on t.target_cohort_id = m1.target_cohort_id AND
        t.subject_id = m1.subject_id AND
        t.ordinal_id = m1.ordinal_id AND
        t.time_label = m1.time_label AND
        t.line_item_label = m1.line_item_label AND
        t.person_line_transformation = m1.patient_line AND
        t.statistic_type = m1.statistic_type
      /* Provide option for distribution of events for only those that have it
      SELECT d.*
      FROM @pat_ts_tab d
      WHERE d.statistic_type = 'continuousDistribution'
    */
  ) m
  GROUP BY target_cohort_id, ordinal_id, time_label, line_item_label, patient_line, statistic_type
) t
;
