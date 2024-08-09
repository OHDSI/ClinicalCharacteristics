#' @title
#' Create an empty TableShell object and set its title
#'
#' @param title The title of the TableShell
#' @param targetCohorts A list of TargetCohort objects
#' @param executionSettings An ExecutionSettings object
#' @param sections A list of Section objects
#'
#' @return A TableShell object
#'
#' @export
createTableShell <- function(name,
                            targetCohorts,
                            executionSettings,
                            sections) {
    tableShell <- TableShell$new(name = name,
                                 targetCohorts = targetCohorts,
                                 executionSettings = executionSettings,
                                 sections = sections,)
    return(tableShell)
}

#' @title
#' Parse target cohorts from a data frame
#'
#' @param df The data frame containing the information for the target cohorts (id and name)
#'
#' @return A list of TargetCohort objects
#'
#' @export
parseTargetCohortsFromDf <- function(df) {
    targetCohorts <- purrr::pmap(df, function(id, name) {
        createTargetCohort(id, name)
    })
    return(targetCohorts)
}

#' @title
#' Create a TargetCohort object and set its attributes
#'
#' @param id The ID of the TargetCohort
#' @param name The name of the TargetCohort
#'
#' @return A TargetCohort object
#'
#' @export
createTargetCohort <- function(id, name) {
  targetCohort <- TargetCohort$new(id, name)
  return(targetCohort)
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
#' @param numThreads (OPTIONAL) The number of threads to use for parallel processing
#'
#' @return An ExecutionSettings object
#' @export
createExecutionSettings <- function(connectionDetails,
                                    connection,
                                    cdmDatabaseSchema,
                                    workDatabaseSchema,
                                    tempEmulationSchema,
                                    targetCohortTable,
                                    cdmSourceName,
                                    numThreads) {
  executionSettings <- ExecutionSettings$new(connectionDetails = connectionDetails,
                                             connection = connection,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             workDatabaseSchema = workDatabaseSchema,
                                             tempEmulationSchema = tempEmulationSchema,
                                             targetCohortTable = targetCohortTable,
                                             cdmSourceName = cdmSourceName,
                                             numThreads = numThreads)
  return(executionSettings)
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

#' @title
#' Create a Time Window object
#' @param ... a list of timeInterval objects
#'
#' @return A time window object
#'
#' @export
createTimeWindows <- function(...) {
  windows <- list(...)
  tw <- TimeWindow$new(windows = windows)
  return(tw)
}


createPresence <- function(operator = "at_least", occurrences = 1) {
  pres <- Presence$new(operator = operator, occurrences = occurrences)
  return(pres)
}

#' @title
#' Create a Section object and set its attributes
#' @param title The title of the Section
#' @param ordinal The ordinal of the Section
#' @param lineItems A list of LineItem objects
#'
#' @return A Section object
#'
#' @export
createSection <- function(name, ordinal, lineItems) {
  section <- Section$new(name, ordinal, lineItems)
  return(section)
}

#' @title
#' Create a concept set line item and set its attributes
#'
#' @param name (OPTIONAL) The name of the line item (if not provided, the name will be set to the Capr concept set name)
#' @param ordinal The ordinal of the line item within a section
#' @param statistic The Statistic object to be used to evaluate the line item
#' @param domain The domain of the concept set (must be one of 'Condition', 'Drug', 'Procedure', 'Observation', 'Measurement', 'Device')
#' @param conceptSet The Capr concept set object
#' @param timeInterval The Time Interval object used for the line item
#' @param sourceConceptSet (OPTIONAL) A Capr concept set of source concept IDs to use to limit the concept set
#' @param typeConceptIds (OPTIONAL) A list of type concept IDs to use to limit the concept set
#' @param visitOccurrenceConceptIds (OPTIONAL) A list of visit occurrence concept IDs to use to limit the concept set
#'
#' @return A ConceptSetDefinition object
#'
#' @export
createConceptSetLineItem <- function(name,
                                     ordinal,
                                     statistic,
                                     domain,
                                     conceptSet,
                                     timeInterval,
                                     sourceConceptSet = NULL,
                                     typeConceptIds = c(),
                                     visitOccurrenceConceptIds = c()) {
  csDefinition <- ConceptSetDefinition$new(name = name,
                                           ordinal = ordinal,
                                           statistic = statistic,
                                           domain = domain,
                                           conceptSet = conceptSet,
                                           timeInterval = timeInterval,
                                           sourceConceptSet = sourceConceptSet,
                                           typeConceptIds = typeConceptIds,
                                           visitOccurrenceConceptIds = visitOccurrenceConceptIds)
  return(csDefinition)
}

# function to get timeInterval and concept set combinations
.permuteCsTi <- function(conceptSets, timeIntervals) {

  # get number of items for each
  numCs <- length(conceptSets)
  numTis <- length(timeIntervals)

  # build out permutations
  csPerm <- rep(conceptSets, times = numTis)
  tiPerm <- rep(timeIntervals, each = numCs)

  permTiCs <- list(
    'conceptSets' = csPerm,
    'timeIntervals' = tiPerm
  )
  return(permTiCs)

}

#' @title
#' Create a batch of concept set line items from a list of Capr concept sets.
#'
#' @description
#' The name of each line item will be set to the name of its Capr concept set, and the ordinal will be set to the index of the Capr concept set in the list. All line items will use the same statistic, domain, type concepts, and visit concepts. It is not possible to specify source concept IDs.
#' @param name The name of the concept set batch
#' @param statistic The Statistic object to be used to evaluate the line items
#' @param domain The domain of the concept sets (must be one of 'Condition', 'Drug', 'Procedure', 'Observation', 'Measurement', 'Device')
#' @param conceptSets A list of concept set Capr objects
#' @param timeIntervals A list of TimeInterval class objects
#' @param sourceConceptSet (OPTIONAL) A Capr concept set of source concept IDs to use to limit the concept set
#' @param typeConceptIds (OPTIONAL) A list of type concept IDs to use to limit the concept set
#' @param visitOccurrenceConceptIds (OPTIONAL) A list of visit occurrence concept IDs to use to limit the concept set
#'
#' @return A list of ConceptSetDefinition objects
#'
#' @export
createConceptSetLineItemBatch <- function(
    name,
    statistic,
    domain,
    conceptSets,
    timeIntervals,
    sourceConceptSet = NULL,
    typeConceptIds = c(),
    visitOccurrenceConceptIds = c()) {

  checkmate::assert_list(x = conceptSets, types = c("ConceptSet"), null.ok = FALSE, min.len = 1)
  checkmate::assert_list(x = timeIntervals, types = c("TimeInterval"), null.ok = FALSE, min.len = 1)

  # build permutations of concepts and timeIntervals
  permDf <- .permuteCsTi(conceptSets, timeIntervals)

  # create batch of concept set line items
  csLiBatch <- purrr::map2(
    permDf$conceptSets,
    permDf$timeIntervals,
    ~createConceptSetLineItem(
      name = name,
      ordinal = NA_integer_,
      statistic = statistic,
      domain = domain,
      conceptSet = .x,
      timeInterval = .y,
      sourceConceptSet = sourceConceptSet,
      typeConceptIds = typeConceptIds,
      visitOccurrenceConceptIds = visitOccurrenceConceptIds
    )
  ) |>
    unname()

  return(csLiBatch)
}


createGenderLineItem <- function(ordinal) {

  gender <- DemographicDefinition$new(
    name = "Gender",
    ordinal = ordnial,
    statistic = DemoConcept$new(conceptColumn = "gender_concept_id")
  )

  return(gender)

}


createRaceLineItem <- function(ordinal) {

  gender <- DemographicDefinition$new(
    name = "Race",
    ordinal = ordnial,
    statistic = DemoConcept$new(conceptColumn = "race_concept_id")
  )

  return(gender)

}

createEthnicityLineItem <- function(ordinal) {

  gender <- DemographicDefinition$new(
    name = "Ethnicity",
    ordinal = ordnial,
    statistic = DemoConcept$new(conceptColumn = "ethnicity_concept_id")
  )

  return(gender)

}


createAgeLineItem <- function(ordinal, breaks = NULL) {

  age <- DemographicDefinition$new(
    name = "Age",
    ordinal = ordinal,
    statistic = Age$new(breaks = breaks)
  )

  return(age)

}


# addGenderItem <- function(genderConceptIds = c(),
#                           inputType) {
#   gender <- GenderDefinition$new(inputType = "Explicit", genderConceptIds = genderConceptIds)

# }
