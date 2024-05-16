insert_into_dat <- function() {
  glue::glue(
    " -- Insert into data table
    INSERT INTO {{dataTable}} (cohort_id, subject_id, category_id, time_id, value_id, value)
    SELECT i.cohort_id, i.subject_id,
    {x@orderId} AS category_id,
    i.time_id,
    i.value_id,
    1 AS value
    FROM (
    {limit_sql(x)}
    ) i
    ;
    ")
}
