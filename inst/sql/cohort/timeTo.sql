INSERT INTO @patient_level_data
SELECT
        d.target_cohort_id,
        d.subject_id,
        d.time_label,
        d.domain_table,
        'timeTo' AS patient_line,
        d.raw_occurrence_description as value_type,
        d.raw_occurrence_id as value_id,
        DATEDIFF(day, d.cohort_start_date, d.event_start_date) AS value
FROM (
  SELECT a.target_cohort_id, a.subject_id, a.time_label, a.domain_table, a.raw_occurrence_description, a.raw_occurrence_id, a.cohort_start_date, a.event_start_date
      FROM (
        SELECT l.*,
          ROW_NUMBER() OVER (
            PARTITION BY l.target_cohort_id, l.subject_id, l.time_label, l.raw_occurrence_description, l.raw_occurrence_id
            ORDER BY l.event_start_date {@first} ? {ASC} : {DESC}
          ) as ordinal
        FROM (
          SELECT target_cohort_id, subject_id, cohort_start_date, cohort_end_date, domain_table, time_label, raw_occurrence_id,
            raw_occurrence_description,
            CASE WHEN event_start_date < cohort_start_date THEN cohort_start_date ELSE event_start_date END as event_start_date,
            event_end_date
            FROM @cohort_occurrence_table
          ) l
        JOIN (
          SELECT * FROM @ts_meta_table WHERE person_line_transformation = 'timeToFirst'
        ) m
        ON l.raw_occurrence_id = m.value_id AND l.raw_occurrence_description = m.value_description AND l.time_label = m.time_label
      ) a
      WHERE ordinal = 1
) d
;
