# Builders --------------



# function to build clinical covariates based on domains
build_domain_covariates <- function(con,
                                    tempEmulationSchema,
                                    cohortDatabaseSchema,
                                    cohortTable,
                                    cdmDatabaseSchema,
                                    cohortIds,
                                    domain,
                                    includeConcepts = c(),
                                    excludeConcepts = c(),
                                    timeA,
                                    timeB,
                                    minValue = 10,
                                    outputFolder) {
  # ii <- seq_along(timeA)
  # time_print <- glue::glue("t{ii}: {timeA}_{timeB}") |> paste(collapse = ", ")
  # consule_txt <- glue::glue("Build {domain} Covariates")
  # cli::cat_bullet(consule_txt, bullet = "pointer", bullet_col = "yellow")
  # cli::cat_bullet(
  #   glue::glue("At time points: {time_print}"),
  #   bullet = "info",
  #   bullet_col = "blue"
  # )
  # get cov_settings
  covSettings <- purrr::map2(
    timeA,
    timeB,
    ~domain_settings_fn(domain = domain,
                        timeA = .x,
                        timeB = .y,
                        include = includeConcepts,
                        exclude = excludeConcepts))

  #run FE
  tbl <- purrr::map_dfr(
    covSettings,
    ~getCovariatesAndFormat(
      con = con,
      tempEmulationSchema = tempEmulationSchema,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortTable = cohortTable,
      cohortDatabaseSchema = cohortDatabaseSchema,
      domain = domain,
      cohortId = cohortIds,
      covSettings = .x
    )
  )

  # save output
  verboseSave(
    object = tbl,
    saveName = tolower(glue::glue("{domain}_covariates")),
    saveLocation = outputFolder
  )

  invisible(tbl)

}

# function to build clinical covariates based on demographics
build_demographic_covariates <- function(con,
                                         tempEmulationSchema,
                                         cohortDatabaseSchema,
                                         cohortTable,
                                         cdmDatabaseSchema,
                                         cohortIds,
                                         type,
                                         outputFolder) {

  consule_txt <- glue::glue("Build {type} Demographics")
  cli::cat_bullet(consule_txt, bullet = "pointer", bullet_col = "yellow")

  # get cov_settings
  covSettings <- demographic_settings_fn(type = type)

  #run FE
  cov <- FeatureExtraction::getDbCovariateData(connection = con,
                                               oracleTempSchema = tempEmulationSchema,
                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                               cohortTable = cohortTable,
                                               cohortDatabaseSchema = cohortDatabaseSchema,
                                               cohortId = cohortIds,
                                               covariateSettings = covSettings,
                                               aggregated = TRUE)

  if (type %in% c("Categorical")) {
    # collect categorical covariates
    tbl <- cov$covariates |>
      dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
      dplyr::select(
        cohortDefinitionId, analysisId,
        covariateId, covariateName,
        sumValue, averageValue) |>
      dplyr::collect() |>
      dplyr::mutate(
        covariateName = gsub(".*: ", "", covariateName)
      )

  } else{
    # collect continuous covariates
    tbl <- cov$covariatesContinuous |>
      dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
      dplyr::select(
        cohortDefinitionId, analysisId,
        covariateId, covariateName, conceptId,
        countValue,
        averageValue, standardDeviation,
        minValue, p10Value, p25Value,
        medianValue, p75Value, p90Value, maxValue) |>
      dplyr::collect()

  }

  # save output
  verboseSave(
    object = tbl,
    saveName = tolower(glue::glue("{type}_demographics_covariates")),
    saveLocation = outputFolder
  )

  invisible(tbl)

}

# function to build clinical covariates based on scores
build_score_covariates <- function(con,
                                   tempEmulationSchema,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   cdmDatabaseSchema,
                                   cohortIds,
                                   scores,
                                   outputFolder) {

  consule_txt <- glue::glue("Build Covariate Scores")
  cli::cat_bullet(consule_txt, bullet = "pointer", bullet_col = "yellow")

  # get cov_settings
  covSettings <- score_settings_fn(scores = scores)

  #run FE
  cov <- FeatureExtraction::getDbCovariateData(connection = con,
                                               oracleTempSchema = tempEmulationSchema,
                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                               cohortTable = cohortTable,
                                               cohortDatabaseSchema = cohortDatabaseSchema,
                                               cohortId = cohortIds,
                                               covariateSettings = covSettings,
                                               aggregated = TRUE)
  # collect continuous covariates
  tbl <- cov$covariatesContinuous |>
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
    dplyr::select(
      cohortDefinitionId, analysisId,
      covariateId, covariateName, conceptId,
      countValue,
      averageValue, standardDeviation,
      minValue, p10Value, p25Value,
      medianValue, p75Value, p90Value, maxValue) |>
    dplyr::collect()

  # save output
  verboseSave(
    object = tbl,
    saveName = glue::glue("score_covariates"),
    saveLocation = outputFolder
  )

  invisible(tbl)

}

# function to build clinical covariates based on cohorts
build_cohort_covariates <- function(con,
                                    tempEmulationSchema,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   cdmDatabaseSchema,
                                   targetCohortIds,
                                   covariateCohorts,
                                   timeA,
                                   timeB,
                                   outputFolder) {


  ii <- seq_along(timeA)
  time_print <- glue::glue("t{ii}: {timeA}_{timeB}") |> paste(collapse = ", ")
  consule_txt <- glue::glue("Build Cohort Covariates")
  cli::cat_bullet(consule_txt, bullet = "pointer", bullet_col = "yellow")
  cli::cat_bullet(
    glue::glue("At time points: {time_print}"),
    bullet = "info",
    bullet_col = "blue"
  )

  # get cov_settings
  covSettings <- FeatureExtraction::createCohortBasedTemporalCovariateSettings(
    analysisId = 999,
    covariateCohortDatabaseSchema = cohortDatabaseSchema,
    covariateCohortTable = cohortTable,
    covariateCohorts = covariateCohorts,
    valueType = "binary",
    temporalStartDays = timeA,
    temporalEndDays = timeB,
    includedCovariateIds = c(),
    warnOnAnalysisIdOverlap = TRUE
  )

  #run FE
  cov <- FeatureExtraction::getDbCovariateData( # cant use FeatureExtraction::getDbCohortBasedCovariatesData
    connection = con,
    oracleTempSchema = tempEmulationSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortId = targetCohortIds,
    covariateSettings = covSettings,
    aggregated = TRUE
  )

  tbl <- cov$covariates |>
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
    dplyr::select(
      cohortDefinitionId, analysisId, timeId,
      covariateId, covariateName,
      sumValue, averageValue) |>
    dplyr::collect() |>
    dplyr::mutate(
      covariateName = gsub(".*: ", "", covariateName)
    )

  # save output
  verboseSave(
    object = tbl,
    saveName = glue::glue("cohort_covariates"),
    saveLocation = outputFolder
  )

  invisible(tbl)

}

