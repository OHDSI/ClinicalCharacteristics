
.getAssertChoices <- function(category) {
  read.csv(file = system.file(package = pkgload::pkg_name(), "csv", "assertChoices.csv"),
           as.is = TRUE, stringsAsFactors = FALSE) |>
    dplyr::filter(category == !!category) |>
    dplyr::pull(choice)
}

.getStatisticTypes <- function(definitionType) {
  read.csv(file = system.file(package = pkgload::pkg_name(), "csv", "definitionType.csv"),
           as.is = TRUE, stringsAsFactors = FALSE) |>
    dplyr::filter(definitionType == !!definitionType)
}

.getStatisticSqlFile <- function(choice) {
  read.csv(file = system.file(package = pkgload::pkg_name(), "csv", "assertChoices.csv"),
           as.is = TRUE, stringsAsFactors = FALSE) |>
    dplyr::filter(category == "StatisticType" & choice == !!choice) |>
    dplyr::pull(sqlFileName)
}



.cascadeObject <- function(cascadeFrom,
                           cascadeName,
                           cascadeTo) {
  return(lapply(cascadeTo, function(object) {
    object[[cascadeName]] <- cascadeFrom[[cascadeName]]
  }))
}

.getCaseSql <- function(covariateValues,
                        then) {

  sql <- glue::glue("when covariate_value >= {thresholdMin} and covariate_value <= {thresholdMax} then {covariateId}")
  caseSql <- glue::glue("case {sql} end as covariate_id", sql = paste(caseSql, collapse = "\n"))
}

.setString <- function(private, key, value) {
  checkmate::assert_string(x = value, na.ok = FALSE, min.chars = 1, null.ok = FALSE)
  private[[key]] <- value
  invisible(private)
}

.setNumber <- function(private, key, value) {
  checkmate::assert_numeric(x = value, na.ok = FALSE, null.ok = FALSE)
  private[[key]] <- value
  invisible(private)
}

.setLogical <- function(private, key, value) {
  checkmate::assert_logical(x = value, na.ok = FALSE, null.ok = FALSE)
  private[[key]] <- value
  invisible(private)
}

.setClass <- function(private, key, value, class) {
  checkmate::assert_class(x = value, classes = class, null.ok = FALSE)
  private[[key]] <- value
  invisible(private)
}

.setListofClasses <- function(private, key, value, classes) {
  checkmate::assert_list(x = value, types = classes, null.ok = FALSE, min.len = 1)
  private[[key]] <- c(private[[key]], value)
  invisible(private)
}

.setChoice <- function(private, key, value, choices) {
  checkmate::assert_choice(x = value, choices = choices, null.ok = FALSE)
  private[[key]] <- value
  invisible(private)
}

.get <- function(private, key) {
  return(private[[key]])
}
