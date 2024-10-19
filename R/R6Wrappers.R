#' @title
#' Create an empty TableShell object and set its title
#'
#' @param title The title of the TableShell
#' @param targetCohorts A list of TargetCohort objects
#' @param lineItems A list of lineItem objects
#'
#' @return A TableShell object
#'
#' @export
createTableShell <- function(title,
                             targetCohorts,
                             lineItems) {
    tableShell <- TableShell$new(name = title,
                                 targetCohorts = targetCohorts,
                                 lineItems = lineItems)
    return(tableShell)
}

#' @title
#' Parse cohort info from a data frame
#'
#' @param df The data frame containing the information for the cohorts (id and name)
#'
#' @return A list of CohortInfo objects
#'
#' @export
parseCohortInfoFromDf <- function(df) {
    cohortInfo <- purrr::pmap(df, function(id, name) {
        createCohortInfo(id, name)
    })
    return(cohortInfo)
}

#' @title
#' Create a CohortInfo object for a cohort and set its attributes
#'
#' @param id The ID of the cohort
#' @param name The name of the cohort
#'
#' @return A CohortInfo object
#'
#' @export
createCohortInfo <- function(id, name) {
  cohortInfo <- CohortInfo$new(id, name)
  return(cohortInfo)
}

#' @title
#' Create an ExecutionSettings object and set its attributes
#'
#' @param connectionDetails A DatabaseConnector connectionDetails object (optional if connection is specified)
#' @param connection A DatabaseConnector connection object (optional if connectionDetails is specified)
#' @param cdmDatabaseSchema The schema of the OMOP CDM database
#' @param workDatabaseSchema The schema to which results will be written
#' @param tempEmulationSchema Some database platforms like Oracle and Snowflake do not truly support temp tables. To emulate temp tables, provide a schema with write privileges where temp tables can be created.
#' @param targetCohortTable The name of the table where the target cohort(s) are stored
#' @param cdmSourceName A human-readable name for the OMOP CDM source
#'
#' @return An ExecutionSettings object
#' @export
createExecutionSettings <- function(connectionDetails,
                                    connection = NULL,
                                    cdmDatabaseSchema,
                                    workDatabaseSchema,
                                    tempEmulationSchema,
                                    targetCohortTable,
                                    cdmSourceName) {
  executionSettings <- ExecutionSettings$new(connectionDetails = connectionDetails,
                                             connection = connection,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             workDatabaseSchema = workDatabaseSchema,
                                             tempEmulationSchema = tempEmulationSchema,
                                             targetCohortTable = targetCohortTable,
                                             cdmSourceName = cdmSourceName)
  return(executionSettings)
}


defaultTableShellBuildOptions <- function(keepResultsTable = FALSE,
                                          resultsTempTable = "#results_table",
                                          codesetTempTable = "#codeset",
                                          timeWindowTempTable = "#time_windows",
                                          targetCohortTempTable = "#target_cohorts") {

  buildOpts <- BuildOptions$new(
    keepResultsTable = keepResultsTable,
    resultsTempTable = resultsTempTable,
    codesetTempTable = codesetTempTable,
    timeWindowTempTable = timeWindowTempTable,
    targetCohortTempTable = targetCohortTempTable
  )
  return(buildOpts)

}


#' @title
#' Create a single time interval
#' @param lb the left bound of the time interval
#' @param rb the right bound of the time interval
#'
#' @return A time interval object
#'
#' @export
timeInterval <- function(lb, rb) {
  ti <- TimeInterval$new(lb = lb, rb = rb)
  return(ti)
}


createPresence <- function(operator = "at_least", occurrences = 1) {
  pres <- Presence$new(operator = operator, occurrences = occurrences)
  return(pres)
}


createCount <- function(breaks = NULL) {
  occurrenceCount <- Count$new(breaks = breaks)
  return(occurrenceCount)
}

#' @title
#' Create a concept set line item and set its attributes
#'
#' @param name (OPTIONAL) The name of the line item (if not provided, the name will be set to the Capr concept set name)
#' @param statistic The Statistic object to be used to evaluate the line item
#' @param domain The domain of the concept set (must be one of 'Condition', 'Drug', 'Procedure', 'Observation', 'Measurement', 'Device')
#' @param conceptSet The Capr concept set object
#' @param timeInterval The Time Interval object used for the line item
#' @param sourceConceptSet (OPTIONAL) A Capr concept set of source concept IDs to use to limit the concept set
#' @param typeConceptIds (OPTIONAL) A list of type concept IDs to use to limit the concept set
#' @param visitOccurrenceConceptIds (OPTIONAL) A list of visit occurrence concept IDs to use to limit the concept set
#'
#' @return A ConceptSetLineItem object
#'
#' @export

createConceptSetLineItem <- function(sectionLabel = NA_character_,
                                     domain,
                                     conceptSet,
                                     timeInterval,
                                     statistic,
                                     sourceConceptSet = NULL,
                                     typeConceptIds = c(),
                                     visitOccurrenceConceptIds = c()) {

  csDefinition <- ConceptSetLineItem$new(sectionLabel = sectionLabel,
                                         domainTable = domain,
                                         conceptSet = conceptSet,
                                         timeInterval = timeInterval,
                                         statistic = statistic,
                                         sourceConceptSet = sourceConceptSet,
                                         typeConceptIds = typeConceptIds,
                                         visitOccurrenceConceptIds = visitOccurrenceConceptIds)
  return(csDefinition)
}



#' @title
#' Create a batch of concept set line items from a list of Capr concept sets.
#'
#' @description
#' The name of each line item will be set to the name of its Capr concept set. All line items will use the same statistic, domain, type concepts, and visit concepts. It is not possible to specify source concept IDs.
#' @param name The name of the concept set batch
#' @param statistic The Statistic object to be used to evaluate the line items
#' @param domain The domain of the concept sets (must be one of 'Condition', 'Drug', 'Procedure', 'Observation', 'Measurement', 'Device')
#' @param conceptSets A list of concept set Capr objects
#' @param timeIntervals A list of TimeInterval class objects
#' @param typeConceptIds (OPTIONAL) A list of type concept IDs to use to limit the concept set
#' @param visitOccurrenceConceptIds (OPTIONAL) A list of visit occurrence concept IDs to use to limit the concept set
#'
#' @return A list of ConceptSetLineItem objects
#'
#' @export
createConceptSetLineItemBatch <- function(
    name,
    statistic,
    domain,
    conceptSets,
    timeIntervals,
    typeConceptIds = c(),
    visitOccurrenceConceptIds = c()) {

  checkmate::assert_list(x = conceptSets, types = c("ConceptSet"), null.ok = FALSE, min.len = 1)
  checkmate::assert_list(x = timeIntervals, types = c("TimeInterval"), null.ok = FALSE, min.len = 1)

  # build permutations of concepts and timeIntervals
  permDf <- .permuteTi(conceptSets, timeIntervals)

  # create batch of concept set line items
  csLiBatch <- purrr::map2(
    permDf$objects,
    permDf$timeIntervals,
    ~createConceptSetLineItem(
      name = .x@Name,
      statistic = statistic,
      domain = domain,
      conceptSet = .x,
      timeInterval = .y,
      sourceConceptSet = NULL,
      typeConceptIds = typeConceptIds,
      visitOccurrenceConceptIds = visitOccurrenceConceptIds
    )
  ) |>
    unname()

  return(csLiBatch)
}

#' @title
#' Create gender demographic line item
#'
#' @return A Demographic line item type class object
#' @export
createGenderLineItem <- function() {

  gender <- DemographicLineItem$new(
    name = "Gender",
    statistic = DemographicConcept$new(conceptColumn = "gender_concept_id")
  )

  return(gender)

}


#' @title
#' Create race demographic line item
#'
#' @return A Demographic line item type class object
#' @export
createRaceLineItem <- function() {

  gender <- DemographicLineItem$new(
    name = "Race",
    statistic = DemographicConcept$new(conceptColumn = "race_concept_id")
  )

  return(gender)

}

#' @title
#' Create ethnicity demographic line item
#'
#' @return A Demographic line item type class object
#' @export
createEthnicityLineItem <- function() {

  gender <- DemographicLineItem$new(
    name = "Ethnicity",
    statistic = DemographicConcept$new(conceptColumn = "ethnicity_concept_id")
  )

  return(gender)

}

#' @title
#' Create age demographic line item
#'
#' @param breaks A breaks object describing how to categorize the continuous value
#' @return A Demographic line item type class object
#' @export
createAgeLineItem <- function(breaks = NULL) {

  age <- DemographicLineItem$new(
    name = "Age",
    statistic = DemographicAge$new(breaks = breaks)
  )

  return(age)

}

#' @title
#' Create a cohort line item and set its attributes
#'
#' @param name (OPTIONAL) The name of the line item (if not provided, the name will be set to the cohort name from the CohortInfo object)
#' @param statistic The Statistic object to be used to evaluate the line item
#' @param cohort A CohortInfo object
#' @param timeInterval The TimeInterval object used for the line item
#'
#' @return A CohortLineItem object
#'
#' @export
createCohortLineItem <- function(name = NULL,
                                 statistic,
                                 cohort,
                                 timeInterval) {
  if (is.null(name)) {
    name = cohort$getName()
  }
  cohortLineItem <- CohortLineItem$new(name = name,
                                       statistic = statistic,
                                       cohort = cohort,
                                       timeInterval = timeInterval)
  return(cohortLineItem)
}

#' @title
#' Create a batch of cohort line items from a list of CohortInfo objects.
#'
#' @description
#' The name of each line item will be set to the name of its cohort from the CohortInfo object.
#' @param name The name of the cohort batch
#' @param statistic The Statistic object to be used to evaluate the line items
#' @param cohorts A list of CohortInfo objects
#' @param timeIntervals A list of TimeInterval class objects
#'
#' @return A list of CohortLineItem objects
#'
#' @export
createCohortLineItemBatch <- function(
    name,
    statistic,
    cohorts,
    timeIntervals) {

  checkmate::assert_list(x = cohorts, types = c("CohortInfo"), null.ok = FALSE, min.len = 1)
  checkmate::assert_list(x = timeIntervals, types = c("TimeInterval"), null.ok = FALSE, min.len = 1)

  # build permutations of concepts and timeIntervals
  permDf <- .permuteTi(cohorts, timeIntervals)

  # create batch of concept set line items
  cLiBatch <- purrr::map2(
    permDf$objects,
    permDf$timeIntervals,
    ~createCohortLineItem(
      name = .x$name,
      statistic = statistic,
      cohort = .x,
      timeInterval = .y
    )
  ) |>
    unname()

  return(cLiBatch)
}


#' @title
#' Combine all lineItems to enter into the tableShell slot
#'
#' @param ... A list of lineItems created from various calls
#' @return a flattened list of lineItems
#' @export
lineItems <- function(...) {
  listOfLineItems <- list(...) |>
    purrr::list_flatten()
  # ensure that all elements are lineItems
  checkmate::assert_list(x = listOfLineItems, types = "LineItem", null.ok = FALSE, min.len = 1)

  # add in ordinals
  ii <- seq_along(listOfLineItems)
  for(i in ii) {
    listOfLineItems[[i]]$ordinal <- ii[i]
  }
  return(listOfLineItems)
}
