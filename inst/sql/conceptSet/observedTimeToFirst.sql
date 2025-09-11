INSERT INTO @patient_level_data
SELECT
        d.target_cohort_id,
        d.subject_id,
        d.time_label,
        d.domain_table,
        'observedTimeToFirst' AS patient_line,
        d.raw_occurrence_description as value_type,
        d.raw_occurrence_id as value_id,
        DATEDIFF(day, d.cohort_start_date, d.event_date) AS value
FROM (
  SELECT a.target_cohort_id, a.subject_id, a.time_label, a.domain_table, a.raw_occurrence_description, a.raw_occurrence_id, a.cohort_start_date, a.event_date
      FROM (
        SELECT l.*,
          ROW_NUMBER() OVER (
            PARTITION BY l.target_cohort_id, l.subject_id
            ORDER BY l.event_date ASC
          ) as ordinal
        FROM @concept_set_occurrence_table l
        JOIN (
          SELECT * FROM @ts_meta_table WHERE person_line_transformation = 'observedTimeToFirst'
        ) m
        ON l.raw_occurrence_id = m.value_id AND l.raw_occurrence_description = m.value_description AND l.time_label = m.time_label
        JOIN @cdm_database_schema.observation_period op
        ON l.subject_id = op.person_id
        WHERE l.cohort_start_date BETWEEN op.observation_period_start_date AND op.observation_period_end_date
        AND l.event_date BETWEEN op.observation_period_start_date AND op.observation_period_end_date
        AND l.event_date >= l.cohort_start_date
      ) a
      WHERE ordinal = 1
) d
;
