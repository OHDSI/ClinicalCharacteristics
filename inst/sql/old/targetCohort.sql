/*Make target cohort temp table*/
SELECT *
INTO @targetTable
FROM @workDatabaseSchema.@cohortTable
WHERE cohort_definition_id IN ({cohortIds})
;
