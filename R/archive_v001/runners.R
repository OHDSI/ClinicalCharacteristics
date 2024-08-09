# Runners ------------------

# functions that execute what we need done
#' Function that runs the clinical characterization analysis
#' @param con a connection to a dbms hosting OMOP data
#' @param executionSettings a list of settings used to specify aspects of the connections
#' @param analysisSettings a list of settings specifying the details of the analysis and how the
#' characterization should be done
#' @param outputFolder a path to a folder specifying where to save the results
#' @return no object return, this function will save csv files of the characterization results
#' @export
runClinicalCharacteristics <- function(con,
                                  executionSettings,
                                  analysisSettings,
                                  outputFolder) {

  cli::cat_boxx("RUN CLINICAL CHARACTERISTICS")

  ## get schema vars
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  tempEmulationSchema <- executionSettings$temptempEmulationSchema

  #get global analysis settings
  targetCohortIds <- analysisSettings$clinicalCharacteristics$targetCohortIds
  settings <- analysisSettings$clinicalCharacteristics$settings

  if (checkMode(settings, mode = "Demographics")) {
    #unpack demographics
    demographics <- settings$Demographics$type

    #run builder
    purrr::walk(
      demographics,
      ~build_demographic_covariates(
        con = con,
        tempEmulationSchema = tempEmulationSchema,
        cohortDatabaseSchema = workDatabaseSchema,
        cohortTable = cohortTable,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortIds = targetCohortIds,
        type = .x,
        outputFolder = outputFolder)
    )
  }

  if (checkMode(settings, mode = "Scores")) {
    cli::cat_bullet("Run Scores", bullet = "cross", bullet_col = "yellow")

    #unpack demographics
    scores <- settings$Scores$scores

    #run builder
    build_score_covariates(
      con = con,
      tempEmulationSchema = tempEmulationSchema,
      cohortDatabaseSchema = workDatabaseSchema,
      cohortTable = cohortTable,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortIds = targetCohortIds,
      scores = scores,
      outputFolder = outputFolder)
  }

  if (checkMode(settings, mode = "Cohorts")) {
    cli::cat_bullet("Run Cohort Characterization", bullet = "cross", bullet_col = "yellow")

    #unpack cohort settings
    covariateCohort <- tibble::tibble(
      cohortId = settings$Cohorts$covariates$cohortId,
      cohortName = settings$Cohorts$covariates$cohortName
    )
    timeA <- settings$Cohorts$windows$timeA
    timeB <- settings$Cohorts$windows$timeB

    #run builder
    build_cohort_covariates(
      con = con,
      tempEmulationSchema = tempEmulationSchema,
      cohortDatabaseSchema = workDatabaseSchema,
      cohortTable = cohortTable,
      cdmDatabaseSchema = cdmDatabaseSchema,
      targetCohortIds = targetCohortIds,
      covariateCohorts = covariateCohort,
      timeA = timeA,
      timeB = timeB,
      outputFolder = outputFolder)

  }
  ###########################
  # Domains
  ############################
  if (checkMode(settings, mode = "Domains")) {
    cli::cat_rule("Run Domain Characterization")

    for (i in seq_along(settings$Domains)) {

      domain <- names(settings$Domains)[i]
      includeConcepts <- settings$Domains[[i]]$include
      excludeConcepts <- settings$Domains[[i]]$exclude
      timeA <- settings$Domains[[i]]$windows$timeA
      timeB <- settings$Domains[[i]]$windows$timeB

      build_domain_covariates(
        con = con,
        tempEmulationSchema = tempEmulationSchema,
        cohortDatabaseSchema = workDatabaseSchema,
        cohortTable = cohortTable,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortIds = targetCohortIds,
        domain = domain,
        includeConcepts = includeConcepts,
        excludeConcepts = excludeConcepts,
        timeA = timeA,
        timeB = timeB,
        outputFolder = outputFolder)
      cli::cat_line()
    }
  }

  invisible(targetCohortIds)
}


