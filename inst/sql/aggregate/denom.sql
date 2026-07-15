DROP TABLE IF EXISTS @pat_ts_tab;
CREATE TABLE @pat_ts_tab AS
SELECT ta.*,
  CASE WHEN ta.patient_line = 'adherentCount' AND observed_subjects IS NULL THEN -5
    ELSE COALESCE(observed_subjects, all_subjects) END AS tot_subjects
FROM (
  SELECT
    a.*, b.ordinal_id, b.section_label, b.line_item_label,
    b.statistic_type,
    b.aggregation_type, b.line_item_class,
    cd.tot_subjects AS observed_subjects,
    cda.tot_subjects AS all_subjects
  FROM @patient_data a
  JOIN @ts_meta b
    ON a.value_id = b.value_id AND a.value_type = b.value_description AND a.time_label = b.time_label AND a.patient_line = b.person_line_transformation
  LEFT JOIN (
            /* Get Cohort Counts within observed time for observed denominator */
            SELECT
                b.cohort_definition_id AS target_cohort_id,
                'adherentCount' AS denom_type,
                b.time_label,
                COUNT (DISTINCT b.subject_id) AS tot_subjects
            FROM (
                SELECT
                    a.cohort_definition_id, a.subject_id, a.cohort_start_date, a.cohort_end_date, a.time_label
                FROM (
                    SELECT t.*, op.observation_period_start_date, tw.time_label, tw.time_a, tw.time_b
                    FROM @target_cohort_table t
                    JOIN @cdm_database_schema.observation_period OP
                      on t.subject_id = OP.person_id and t.cohort_start_date >= OP.observation_period_start_date and t.cohort_start_date <= op.observation_period_end_date
                    CROSS JOIN @time_window tw
                ) a
                -- Ensure patient has fulfilled observation period and both left and right side of time interval to be in denom
                WHERE DATEADD(day, a.time_a, a.cohort_start_date) >= a.observation_period_start_date AND DATEADD(day, a.time_a, a.cohort_start_date) <= a.cohort_end_date
                  and DATEADD(day, a.time_b, a.cohort_start_date) >= a.observation_period_start_date AND DATEADD(day, a.time_b, a.cohort_start_date) <= a.cohort_end_date
            ) b
            GROUP BY b.cohort_definition_id, b.time_label
  ) cd
    ON a.target_cohort_id = cd.target_cohort_id
      AND a.patient_line = cd.denom_type
      AND a.time_label = cd.time_label
  JOIN (
      /* Get Cohort Counts for target cohorts */
      SELECT
        t.cohort_definition_id AS target_cohort_id,
        COUNT(DISTINCT t.subject_id) as tot_subjects
      FROM @target_cohort_table t
      GROUP BY t.cohort_definition_id
  ) cda
  ON a.target_cohort_id = cda.target_cohort_id
) ta
;
