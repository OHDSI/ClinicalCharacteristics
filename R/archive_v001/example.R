# Instantiate Example

initializeCohortTables <- function(con, executionSettings) {

  # if (con@dbms == "snowflake") {
  #   workSchema <- paste(executionSettings$workDatabase, executionSettings$workSchema, sep = ".")
  # } else {
  #   workSchema <- executionSettings$workSchema
  # }
  #

  name <- executionSettings$cohortTable

  cohortTableNames <- list(cohortTable = paste0(name),
                           cohortInclusionTable = paste0(name, "_inclusion"),
                           cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                           cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                           cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                           cohortCensorStatsTable = paste0(name, "_censor_stats"))


  CohortGenerator::createCohortTables(connection = con,
                                      cohortDatabaseSchema = executionSettings$workDatabaseSchema,
                                      cohortTableNames = cohortTableNames,
                                      incremental = TRUE)
  invisible(cohortTableNames)

}

#' Function to instantiate an example environment
#' @param connectionDetails a list specifying connection information using DatabaseConnector::createConnectionDetails
#' @param executionSettings a list specifying the cdmDatabaseSchema, cohortTable, and workDatabaseSchema
#' @param outputFolder a path to a folder to save the check sum
#' @return invisble return, builds cohort definitions from package to the dbms
#' @export
instantiateExample <- function(connectionDetails,
                               executionSettings,
                               outputFolder) {

  # connect to database
  con <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(con))

  # instantiate cohort tables
  initializeCohortTables(con, executionSettings)

  # list out cohorts
  cohortsPath <- fs::path_package("ClinicalCharacteristics", "cohorts")
  cohortFiles <- fs::dir_ls(cohortsPath)

  # make json string
  json <- purrr::map_chr(cohortFiles, ~readr::read_file(.x))

  sql <- purrr::map_chr(
    json,
    ~CirceR::buildCohortQuery(
      CirceR::cohortExpressionFromJson(.x),
      CirceR::createGenerateOptions(generateStats = TRUE))
  )

  # get cohort names
  name <- basename(tools::file_path_sans_ext(cohortFiles)) |> tolower()

  cohortsToCreate <- tibble::tibble(
    cohortName = name,
    json = json,
    sql = sql
  ) |>
    dplyr::mutate(
      cohortId = dplyr::row_number(), .before = 1
    )
  ####################
  # Generate Cohorts
  ####################

  # Path for incremental
  incrementalFolder <- fs::path(outputFolder)


  name <- executionSettings$cohortTable

  cohortTableNames <- list(cohortTable = paste0(name),
                           cohortInclusionTable = paste0(name, "_inclusion"),
                           cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                           cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                           cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                           cohortCensorStatsTable = paste0(name, "_censor_stats"))

  # Generate cohorts
  CohortGenerator::generateCohortSet(
    connection = con,
    cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
    cohortDatabaseSchema =  executionSettings$workDatabaseSchema,
    cohortTable = cohortTableNames,
    cohortDefinitionSet = cohortsToCreate,
    incremental = TRUE,
    incrementalFolder = incrementalFolder
  )

  invisible(cohortsToCreate)

}
