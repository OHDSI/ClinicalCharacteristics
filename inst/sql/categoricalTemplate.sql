select
  RAW_DATA.cohort_definition_id,

from
(
  @rawDataSql
) RAW_DATA
group by 1
;
