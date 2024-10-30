SELECT
    a.target_cohort_id,
    a.subject_id,
    a.domain_table,
    a.time_label,
    --ordinal_id,
    '{aggregation_type}' AS aggregation_type,
    a.raw_occurrence_id AS value_id,
    1 AS value
FROM (
    SELECT
        d.target_cohort_id,
        d.subject_id,
        d.time_label,
        d.domain_table,
        d.raw_occurrence_id,
        COUNT(DISTINCT d.event_date) AS num
      FROM @concept_set_occurrence_table d
      GROUP BY d.target_cohort_id, d.subject_id, d.time_label, d.domain_table, d.raw_occurrence_id
) a
WHERE a.num {op} {occurrences}
