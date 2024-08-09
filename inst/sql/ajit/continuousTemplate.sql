select
  RAW_DATA.cohort_definition_id,

  -- TODO: OHDSql friendly continuous stats
from
(
  @rawDataSql
) RAW_DATA
;
