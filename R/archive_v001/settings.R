# Covariate Settings -----------------------------


# build demographic settings
demographic_settings_fn <- function(type) {

  if (type == "Categorical") {
    dm <- c("Gender", "AgeGroup", "Race", "Ethnicity", "IndexYear")
  }

  if (type == "Continuous") {
    dm <- c("Age", "PriorObservationTime", "PostObservationTime", "TimeInCohort")
  }

  # make settings entry name
  dm <- purrr::map_chr(dm, ~glue::glue("useDemographics{.x}"))

  # create options list
  opts <- rep(TRUE, length(dm)) |> as.list()
  names(opts) <- dm

  #build cov settings call
  cmd <- rlang::call2(
    .fn = "createCovariateSettings",
    !!!opts,
    .ns = "FeatureExtraction"
  )

  cov_settings <- eval(cmd)

  return(cov_settings)

}

# build score settings
score_settings_fn <- function(scores) {

  # make settings entry name
  dm <- purrr::map_chr(scores, ~glue::glue("use{.x}"))

  # create options list
  opts <- rep(TRUE, length(dm)) |> as.list()
  names(opts) <- dm

  #build cov settings call
  cmd <- rlang::call2(
    .fn = "createCovariateSettings",
    !!!opts,
    .ns = "FeatureExtraction"
  )

  cov_settings <- eval(cmd)

  return(cov_settings)
}

clinical_domains <- function(domain = c("Drugs",
                                        "Conditions",
                                        "Procedures",
                                        "Measurements",
                                        "Observations",
                                        "Visits",
                                        "Labs",
                                        "DrugCount",
                                        "ConditionCount",
                                        "ProcedureCount",
                                        "ObservationCount",
                                        "MeasurementCount")) {

  domain <- match.arg(domain)
  dm <- switch(domain,
               Drugs = "DrugGroupEra",
               Conditions = "ConditionGroupEra",
               Procedures = "ProcedureOccurrence",
               Measurements = "Measurement",
               Observations = "Observation",
               Labs = "MeasurementValue",
               Visits = "VisitConceptCount",
               DrugCount = "DistinctIngredientCount",
               ConditionCount = "DistinctConditionCount",
               ProcedureCount = "DistinctProcedureCount",
               ObservationCount = "DistinctObservationCount",
               MeasurementCount = "DistinctMeasurementCount"
  )
  fe <- glue::glue("use{dm}LongTerm")
  return(fe)
}


# build concept domain settings
domain_settings_fn <- function(domain,
                               timeA,
                               timeB,
                               exclude = c(),
                               include = c()) {


  dm <- clinical_domains(domain)
  opts <- rlang::list2(
    !!dm := TRUE,
    longTermStartDays = timeA,
    endDays = timeB,
    excludedCovariateConceptIds = exclude,
    includedCovariateConceptIds = include
  )

  #build cov settings call
  cmd <- rlang::call2(
    .fn = "createCovariateSettings", # temporal has too many bugs
    !!!opts,
    .ns = "FeatureExtraction"
  )

  # evaluate to get cov settings
  cov_settings <- eval(cmd)

  return(cov_settings)
}

# Analysis Settings ------------------------------------

#' Define clinical characteristics settings
#' @param targetCohortIds the target cohorts to characterize
#' @param ... a set of settings to define
#' @return a list object containing the analysis settings
#' @export
defineClinicalCharacteristics <- function(targetCohortIds) {

  ll <- list(
    'clinicalCharacteristics' = list(
      'targetCohortIds' = targetCohortIds,
      'settings' = list(
        'Demographics' = list(),
        'Scores' = list(),
        'Domains' = list(),
        'Cohorts' = list()
      )
    )
  )
  class(ll) <- "clinicalCharacteristicsSettings"
  return(ll)

}

#' Add demographics settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param type the type of demographic covariates to produce, default Categorical and Continuous
#' @return list of settings
#' @export
addDemographicCovariates <- function(settings, type = c("Categorical", "Continuous")) {

  checkmate::check_class(settings, "clinicalCharacteristicsSettings")
  settings$clinicalCharacteristics$settings$Demographics <- list('type' = type)
  return(settings)
}

#' Make score settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param scores the score covariates to produce, default CharlsonIndex, DCsi, and Chads2Vasc
#' @return list of settings
#' @export
addScoreCovariates <- function(settings, scores = c("CharlsonIndex", "Dcsi", "Chads2Vasc")) {
  checkmate::check_class(settings, "clinicalCharacteristicsSettings")
  settings$clinicalCharacteristics$settings$Scores <- list('scores' = scores)
  return(settings)
}

addDomainCovariates <- function(settings, domain, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  checkmate::check_class(settings, "clinicalCharacteristicsSettings")

  domain_settings <- rlang::list2(
    !!domain := list(
      'windows' = list(
        'timeA' = timeA,
        'timeB' = timeB
      ),
      'include' = includeConcepts,
      'exclude' = excludeConcepts
    )
  )
  settings$clinicalCharacteristics$settings$Domains <- append(settings$clinicalCharacteristics$settings$Domains, domain_settings)

  return(settings)
}


#' Make drug covariate settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings adding in drugs
#' @export
addDrugCovariates <- function(settings, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  addDomainCovariates(
    settings = settings,
    domain = "Drugs",
    timeA = timeA,
    timeB = timeB,
    includeConcepts = includeConcepts,
    excludeConcepts = excludeConcepts
  )
}

#' Make condition covariate settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings adding in conditions
#' @export
addConditionCovariates <- function(settings, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  addDomainCovariates(
    settings = settings,
    domain = "Conditions",
    timeA = timeA,
    timeB = timeB,
    includeConcepts = includeConcepts,
    excludeConcepts = excludeConcepts
  )
}

#' Make procedure covariate settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings adding in procedures
#' @export
addProcedureCovariates <- function(settings, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  addDomainCovariates(
    settings = settings,
    domain = "Procedures",
    timeA = timeA,
    timeB = timeB,
    includeConcepts = includeConcepts,
    excludeConcepts = excludeConcepts
  )
}

#' Make observation covariate settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings adding in observations
#' @export
addObservationCovariates <- function(settings, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  addDomainCovariates(
    settings = settings,
    domain = "Observations",
    timeA = timeA,
    timeB = timeB,
    includeConcepts = includeConcepts,
    excludeConcepts = excludeConcepts
  )
}

#' Make measurement covariate settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings adding in measurements
#' @export
addMeasurementCovariates <- function(settings, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  addDomainCovariates(
    settings = settings,
    domain = "Measurements",
    timeA = timeA,
    timeB = timeB,
    includeConcepts = includeConcepts,
    excludeConcepts = excludeConcepts
  )
}

#' Make lab value covariate settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings adding in lab values
#' @export
addLabValueCovariates <- function(settings, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  addDomainCovariates(
    settings = settings,
    domain = "Labs",
    timeA = timeA,
    timeB = timeB,
    includeConcepts = includeConcepts,
    excludeConcepts = excludeConcepts
  )
}

#' Make visit covariate settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings adding in visits
#' @export
addVisitCovariates <- function(settings, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  addDomainCovariates(
    settings = settings,
    domain = "Visits",
    timeA = timeA,
    timeB = timeB,
    includeConcepts = includeConcepts,
    excludeConcepts = excludeConcepts
  )
}


#' Make drug count covariate settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings adding in drug count
#' @export
addDrugCountCovariates <- function(settings, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  addDomainCovariates(
    settings = settings,
    domain = "DrugCount",
    timeA = timeA,
    timeB = timeB,
    includeConcepts = includeConcepts,
    excludeConcepts = excludeConcepts
  )
}

#' Make condition count covariate settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings adding in condition count
#' @export
addConditionCountCovariates <- function(settings, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  addDomainCovariates(
    settings = settings,
    domain = "ConditionCount",
    timeA = timeA,
    timeB = timeB,
    includeConcepts = includeConcepts,
    excludeConcepts = excludeConcepts
  )
}

#' Make procedure count covariate settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings adding in procedure count
#' @export
addProcedureCountCovariates <- function(settings, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  addDomainCovariates(
    settings = settings,
    domain = "ProcedureCount",
    timeA = timeA,
    timeB = timeB,
    includeConcepts = includeConcepts,
    excludeConcepts = excludeConcepts
  )
}

#' Make observation count covariate settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings adding in observation count
#' @export
addObservationCountCovariates <- function(settings, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  addDomainCovariates(
    settings = settings,
    domain = "ObservationCount",
    timeA = timeA,
    timeB = timeB,
    includeConcepts = includeConcepts,
    excludeConcepts = excludeConcepts
  )
}

#' Make measurement count covariate settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @param includeConcepts a vector of concept ids limiting the output of what to include
#' @param excludeConcepts a vector of concept ids limiting the output of what to exclude
#' @return list of settings adding in measurement count
#' @export
addMeasurementCountCovariates <- function(settings, timeA, timeB, includeConcepts = c(), excludeConcepts = c()) {
  addDomainCovariates(
    settings = settings,
    domain = "MeasurementCount",
    timeA = timeA,
    timeB = timeB,
    includeConcepts = includeConcepts,
    excludeConcepts = excludeConcepts
  )
}


#' Make cohort settings
#' @param settings the clinicalCharacteristicsSettings object to add to
#' @param cohortId the ids of the cohorts in the initialized cohort table to use as covariates
#' @param cohortName the names of the cohorts corresponding to their id
#' @param timeA a vector of time points. This should be the left point in the time interval
#' @param timeB a vector of time points. This should be the right point in the time interval
#' @return list of settings with cohort covariates
#' @export
addCohortCovariates <- function(settings, cohortId, cohortName, timeA, timeB) {

  checkmate::check_class(settings, "clinicalCharacteristicsSettings")
  settings$clinicalCharacteristics$settings$Cohorts <- list(
    'covariates' = list(
      'cohortId' = cohortId,
      'cohortName' = cohortName
    ),
    'windows' = list(
      'timeA' = timeA,
      'timeB' = timeB
    )
  )
  return(settings)
}






