select
  TARGET.cohort_definition_id,
  {@covariateValueSql} as covariate_value
from @workDatabaseSchema.@targetCohortTable TARGET
join @covariateDatabaseSchema.@covariateCohortTable COVARIATE on TARGET.subject_id = COVARIATE.subject_id
where TARGET.cohort_definition_id in (@targetCohortIds)
  and COVARIATE.cohort_definition_id = @covariateCohortId
  and {@dateFilterSql}
group by 1
;
