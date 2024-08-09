drop table if exists @workDatabaseSchema.clinchar_cs_stg;

create table @workDatabaseSchema.clinchar_cs_stg
(
  concept_set_id bigint,
  concept_id bigint,
  domain_id varchar(255)
)
;
