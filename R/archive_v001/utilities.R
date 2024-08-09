# Utilities ----------------------

# check if slot is in analysis settings
checkMode <- function(settings, mode) {
  obj <- settings[[mode]]
  length(obj) > 0
}

# pull covariates and prep for save
getCovariatesAndFormat <- function(con,
                                   tempEmulationSchema,
                              cdmDatabaseSchema,
                              cohortTable,
                              cohortDatabaseSchema,
                              domain,
                              cohortId,
                              covSettings) {


  timeA <- covSettings$longTermStartDays
  timeB <- covSettings$endDays
  tb <-  glue::glue("{timeA}d:{timeB}d")
  txt <- glue::glue("Building {crayon::green(domain)} Charateristics at time: {crayon::cyan(tb)}")

  cli::cat_bullet(txt, bullet = "pointer", bullet_col = "yellow")

  cov <- FeatureExtraction::getDbCovariateData(
    connection = con,
    oracleTempSchema = tempEmulationSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortId = cohortId,
    covariateSettings = covSettings,
    aggregated = TRUE
  )


  if (domain %in% c("Drugs", "Conditions", "Procedures", "Measurements", "Observations")) { # categorical format
    tbl <- cov$covariates |>
      dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
      dplyr::select(
        cohortDefinitionId, analysisId,
        covariateId, covariateName, conceptId,
        sumValue, averageValue) |>
      dplyr::collect() |>
      dplyr::mutate(
        covariateName = gsub(".*: ", "", covariateName),
        timeId = tb
      )
  } else{ # continuous format
    tbl <- cov$covariatesContinuous |>
      dplyr::left_join(cov$covariateRef, by = c("covariateId")) |>
      dplyr::collect() |>
      dplyr::mutate(
        covariateName = gsub(".*: ", "", covariateName), # remove the junk in covariate name
        timeId = tb
      ) |>
      dplyr::select(
        cohortDefinitionId, analysisId, timeId,
        covariateId, covariateName, conceptId,
        countValue,
        averageValue, standardDeviation,
        minValue, p10Value, p25Value,
        medianValue, p75Value, p90Value, maxValue
      )
  }
  return(tbl)
}





#prints save activity to console
#TODO add logger to this
verboseSave <- function(object, saveName, saveLocation) {
  savePath <- fs::path(saveLocation, saveName, ext = "csv")
  readr::write_csv(object, file = savePath)
  cli::cat_line()
  txt <- glue::glue("Saved file {crayon::green(basename(savePath))} to: {crayon::cyan(saveLocation)}")
  cli::cat_bullet(txt, bullet = "info", bullet_col = "blue")
  cli::cat_line()
  invisible(savePath)
}


