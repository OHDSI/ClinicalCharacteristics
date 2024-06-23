select
  TARGET.cohort_definition_id,
  {@covariateValueSql} as covariate_value
from @workDatabaseSchema.@targetCohortTable TARGET
join @cdmDatabaseSchema.person PERSON on TARGET.subject_id = PERSON.person_id
where TARGET.cohort_definition_id in (@targetCohortIds)
  and PERSON.gender_concept_id in (@genderConceptIds)
;
