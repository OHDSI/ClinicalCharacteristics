select
  TARGET.cohort_definition_id,
  {@covariateValueSql} as covariate_value
from @workDatabaseSchema.@targetCohortTable TARGET
join
(
  select
    subject_id as person_id,
    cohort_start_date as event_start_date,
    cohort_end_date as event_end_date
  from @covariateDatabaseSchema.@covariateCohortTable
  where COVARIATE.cohort_definition_id = @covariateCohortId
) COVARIATE on TARGET.subject_id = COVARIATE.person_id
where TARGET.cohort_definition_id in (@targetCohortIds)
  and {@dateFilterSql}
group by 1
;
