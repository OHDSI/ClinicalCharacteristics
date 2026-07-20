INSERT INTO @patient_level_data
SELECT
        d.target_cohort_id,
        d.subject_id,
        d.time_label,
        d.domain_table,
        'adherentCount' AS patient_line,
        d.raw_occurrence_description as value_type,
        d.raw_occurrence_id as value_id,
        COUNT(DISTINCT d.event_start_date) AS value
FROM (
  SELECT cc.*, m.ordinal_id, m.statistic_type, m.line_item_class, op.observation_period_start_date, op.observation_period_end_date, tw.time_a, tw.time_b
  FROM @cohort_occurrence_table cc
  JOIN (
    SELECT * FROM @ts_meta_table WHERE person_line_transformation = 'adherentCount'
  ) m
  ON cc.raw_occurrence_id = m.value_id AND cc.time_label = m.time_label
  JOIN @time_window_table tw
    ON cc.time_label = tw.time_label
  JOIN @cdm_database_schema.observation_period OP
    on cc.subject_id = OP.person_id and cc.cohort_start_date >= OP.observation_period_start_date and cc.cohort_start_date <= op.observation_period_end_date
  WHERE 1=1
    {@cohort_analysis_type == "era"} ? {
      -- cohort event era has 1+ day overlap with time window
      AND cc.event_start_date <= DATEADD(day, tw.time_b, cc.cohort_start_date)
      AND cc.event_end_date >= DATEADD(day, tw.time_a, cc.cohort_start_date)
    }:{
      -- cohort event start date is within time window
      AND cc.event_start_date <= DATEADD(day, tw.time_b, cc.cohort_start_date)
      AND cc.event_start_date >= DATEADD(day, tw.time_a, cc.cohort_start_date)
    }
    -- Ensure patient has fulfilled observation period and both left and right side of time interval are covered
    AND DATEADD(day, tw.time_a, cc.cohort_start_date) >= OP.observation_period_start_date
    AND DATEADD(day, tw.time_b, cc.cohort_start_date) <= OP.observation_period_end_date

    -- Ensure right side of time interval is on or before cohort end date
    AND DATEADD(day, tw.time_b, cc.cohort_start_date) <= cc.cohort_end_date
) d
GROUP BY d.target_cohort_id, d.subject_id, d.time_label, d.domain_table, d.raw_occurrence_description, d.raw_occurrence_id
;
