#' @description
#' Create an empty TableShell object and set its title
#'
#' @param title The title of the TableShell
#'
#' @return A TableShell object
#'
#' @export
createTableShell <- function(title) {
    tableShell <- TableShell$new()
    tableShell$setTitle(title)
    return(tableShell)
}

#' @description
#' Add TargetCohorts to a TableShell object
#'
#' @param tableShell The TableShell object
#' @param targetCohorts A list of TargetCohort objects
#'
#' @return A TableShell object
#'
#' @export
addTargetCohorts <- function(tableShell, targetCohorts) {
    tableShell$setTargetCohorts(targetCohorts)
    invisible(tableShell)
}

#' @description
#' Add target cohorts from a data frame
#'
#' @param tableShell The table shell object to which the target cohorts will be added.
#' @param df The data frame containing the information for the target cohorts.
#'
#' @return The updated table shell object with the target cohorts added.
#'
#' @export
addTargetCohortsFromDf <- function(tableShell, df) {
    targetCohorts <- purrr::pmap(df, function(id, name) {
        createTargetCohort(id, name)
    })
    tableShell$setTargetCohorts(targetCohorts)
    invisible(tableShell)
}

#' @description
#' Add target cohorts from a CSV file
#'
#' @param tableShell The table shell object to which the target cohorts will be added.
#' @param file The path to the CSV file containing the target cohorts data.
#'
#' @return The modified table shell object.
#'
#' @export
addTargetCohortsFromCsv <- function(tableShell, file) {
    df <- read.csv(file)
    addTargetCohortsFromDf(tableShell, df)
    invisible(tableShell)
}

#' @description
#' Add one or more Sections to a TableShell object
#'
#' @param tableShell The TableShell object
#' @param section A Section object
#' @param ... Additional Section objects to be added
#'
#' @return A TableShell object
#'
#' @export
addSections <- function(tableShell, section, ...) {
    if (!inherits(section, "Section")) {
        stop("section must be a Section object")
    }

    additionalSections <- list(...)
    if (!all(sapply(additionalSections, function(x) inherits(x, "Section")))) {
        stop("All additional arguments must be Section objects")
    }

    allSections <- c(list(section), additionalSections)
    tableShell$setSections(allSections)
    invisible(tableShell)
}

#' @description
#' Set the ExecutionSettings of a TableShell object
#'
#' @param tableShell The TableShell object
#' @param executionSettings The ExecutionSettings object
#'
#' @return A TableShell object
#'
#' @export
setExecutionSettings <- function(tableShell, executionSettings) {
    tableShell$setExecutionSettings(executionSettings)
    return(tableShell)
}

#' @description
#' Create a TargetCohort object and set its attributes
#'
#' @param id The ID of the TargetCohort
#' @param name The name of the TargetCohort
#'
#' @return A TargetCohort object
#'
#' @export
createTargetCohort <- function(id, name) {
  targetCohort <- TargetCohort$new()
  targetCohort$setId(id)
  targetCohort$setName(name)
  return(targetCohort)
}
