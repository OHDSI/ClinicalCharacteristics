insert into @workDatabaseSchema.clinchar_cs_stg
select distinct
  CONCEPTSET.concept_set_id,
  CONCEPTSET.concept_id,
  CONCEPT.domain_id
from
(
  @conceptSetSql
) CONCEPTSET
join @cdmDatabaseSchema.concept CONCEPT on CONCEPTSET.conceptId = CONCEPT.concept_id
;
