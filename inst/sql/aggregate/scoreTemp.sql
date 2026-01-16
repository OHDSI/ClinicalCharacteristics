DROP TABLE IF EXISTS #pat_ts_score1;
DROP TABLE IF EXISTS #pat_ts_score2;

/* Remove redundant categories
If patient has both mild diabetes and severe diabetes at baseline only pick severe
If patient has both mild liver disease and severe liver disease at baseline only pick severe
If patient has both any malignancy and metastatic tumor at baseline only pick metastatic tumor
*/
CREATE TABLE #pat_ts_score1 AS
WITH T1 AS (
    SELECT p.*
    FROM @pat_ts_tab p
    WHERE p.statistic_type = 'scoreTransformation'
),
T2 AS (
    SELECT *,
    CASE WHEN ordinal_id IN (7,8) THEN 1 /* the diabetes group*/
      WHEN ordinal_id IN (2,10) THEN 2 /* the liver disease group */
      WHEN ordinal_id IN (11,12) THEN 3 /* the cancer group */
      ELSE 0 END AS dup_id
    FROM T1
    ORDER BY subject_id, ordinal_id
),
T3 AS (
    SELECT *,
    CASE
      /* if there is a duplicate pick the largest ordinal as it is most severe*/
      WHEN dup_id > 0 THEN MAX(ordinal_id) OVER (PARTITION BY subject_id, dup_id)
    ELSE ordinal_id END AS idx
    FROM T2
),
T4 AS (
    /* only pick rows where the idx and ord id are same this is the prefered group*/
    SELECT *, idx - ordinal_id AS dff FROM T3
)
SELECT
  target_cohort_id,  subject_id,  time_label, domain_table,
  patient_line, value_type, value_id, value,
  ordinal_id, section_label,line_item_label, statistic_type,
  aggregation_type, line_item_class,observed_subjects, all_subjects, tot_subjects
FROM T4
WHERE dff = 0
;

/* Convert the charlson into a score */
CREATE TABLE #pat_ts_score2 AS
  SELECT  t.cohort_definition_id AS target_cohort_id, t.subject_id,
    '{timeLabel}' AS time_label,
    '{patientLine}' AS patient_line,
    '{sectionLabel}' AS section_label,
    'scoreTransformation' AS statistic_type,
    CASE WHEN d.charlson_score IS NULL THEN 0 ELSE d.charlson_score END AS charlson_score
    FROM (
      SELECT tt.target_cohort_id, tt.subject_id,
      SUM(score_value) as charlson_score
      FROM(
        SELECT a.*,
          CASE
            {scoreCaseWhen}
            ELSE 0 END AS score_value
        FROM #pat_ts_score1 a
      ) tt
      GROUP BY tt.target_cohort_id, tt.subject_id
    ) d
    RIGHT JOIN @target_cohort_table t
      ON d.target_cohort_id = t.cohort_definition_id AND d.subject_id = t.subject_id
;
