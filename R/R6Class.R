
# TableShell -----

#' @description
#' An R6 class to define a TableShell object
#'
#' @export
TableShell <- R6::R6Class("TableShell",
  public = list(
    shellTitle = NULL,
    targetCohortIds = NULL,
    initialize = function(shellTitle,
                          targetCohortIds = NULL) {

      checkmate::assert_string(x = shellTitle, na.ok = FALSE, min.chars = 1, null.ok = FALSE)
      checkmate::assert_numeric(x = targetCohortIds, null.ok = TRUE)

      self$shellTitle <- shellTitle
      self$targetCohortIds <- targetCohortIds
    },
    addSections = function(sections) {
      checkmate::assert_list(x = sections, types = c("Section"), min.len = 1)
      private$sections <- lapply(sections, function(section) {
        if (is.null(section$targetCohortIds) &
            !is.null(self$targetCohortIds)) {
          section$targetCohortIds <- self$targetCohortIds
        }
      })
    },
    getSections = function() {
      return(private$sections)
    }
  ),
  private = list(
    sections = NULL
  )
)

# Section ------

#' @description
#' An R6 class to define a Section object
#'
#' @export
Section <- R6::R6Class("Section",
  public = list(
    targetCohortIds = NULL,
    sectionTitle = NULL,
    sectionOrdinal = NA,

    initialize = function(sectionTitle,
                          sectionOrdinal = NULL,
                          targetCohortIds = NULL) {

      checkmate::assert_string(x = sectionTitle, na.ok = FALSE, min.chars = 1, null.ok = FALSE)
      checkmate::assert_number(x = sectionOrdinal, na.ok = TRUE, null.ok = TRUE)
      checkmate::assert_numeric(x = targetCohortIds, null.ok = TRUE)

      self$sectionTitle <- sectionTitle
      self$sectionOrdinal <- sectionOrdinal
      self$targetCohortIds <- targetCohortIds
    },

    addLineItems = function(lineItems) {
      checkmate::assert_list(x = lineItems, types = c("LineItem"), min.len = 1)
      private$lineItems <- lineItems
    }
  ),
  private = list(
    lineItems = NULL
  )
)


# LineItem -----

#' @description
#' An R6 class to define a LineItem object
#'
#' @export
LineItem <- R6::R6Class("LineItem",
  public = list(
    itemOrdinal = NA,
    itemCategory = NULL,
    itemLabel = NULL,

    initialize = function(itemOrdinal = NA,
                          itemCategory,
                          itemLabel) {

      checkmate::assert_number(x = itemOrdinal, na.ok = TRUE, null.ok = TRUE)
      checkmate::assert_string(x = itemCategory, null.ok = FALSE)
      checkmate::assert_string(x = itemLabel, null.ok = FALSE)

      self$itemOrdinal <- itemOrdinal
      self$itemCategory <- itemCategory
      self$itemLabel <- itemLabel
    },
    setDomain = function(domain) {
      checkmate::assert_choice(x = domain, choices = c(
        .getAssertChoices(category = "Domain")
      ))
      private$domain <- domain
    },
    setLimit = function(limit) {
      checkmate::assert_choice(x = limit, choices = c(
        .getAssertChoices(category = "Limit")
      ))
      private$limit <- limit
    },
    setDefinition = function(itemDefinition) {
      checkmate::assert_class(x = itemDefinition, classes = c("LineItem",
                                                              "CohortDefinition",
                                                              "ConceptSetDefinition",
                                                              "AgeDefinition",
                                                              "YearDefinition"))
      private$definition <- definition
    },
    setItemDefinition = function(itemDefinition) {
      checkmate::assert_class(x = itemDefinition, classes = c("LineItem"))
      private$itemDefinition <- itemDefinition
    },
    setSql = function(sql) {
      checkmate::assert_string(x = sql, na.ok = FALSE, null.ok = FALSE)
      private$sql <- sql
    },
    getDomain = function() {
      return(private$domain)
    },
    getLimit = function() {
      return(private$limit)
    },
    getDefinition = function() {
      return(private$definition)
    },
    getSql = function() {
      return(private$sql)
    }
  ),
  private = list(
    itemDefinition = NULL,
    sql = NULL,
    timeWindows = NULL,
    domain = NULL,
    limit = NULL,
    definition = NULL
  )
)

# GenderDefinition -----

#' @description
#' An R6 class to define a GenderDefinition object.
#'
#' @export
GenderDefinition <- R6::R6Class("GenderDefinition",
  inherit = LineItem,
  public = list(
    initialize = function(genderConceptIds = c(8507, 8532)) {
      #checkmate::assert_choice(x = genderConceptIds, choices = c(8507, 8532))
      super$setDomain(domain = "Gender")
      sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "genderTemplate.sql",
                                               packageName = pkgload::pkg_name(),
                                               dbms = "sql server",
                                               genderConceptIds = genderConceptIds)
      super$setSql(sql = sql)
    }
  )
)

# AgeDefinition -----

#' @description
#' An R6 class to handle the ...
#'
#' @export
AgeDefinition <- R6::R6Class("AgeDefinition",
  inherit = LineItem,
  public = list(
    initialize = function(minAge,
                          maxAge) {
      checkmate::assert_number(x = minAge, na.ok = FALSE)
      checkmate::assert_number(x = maxAge, na.ok = FALSE)
      super$setDomain(domain = "Age")
      sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "ageTemplate.sql",
                                               packageName = pkgload::pkg_name(),
                                               dbms = "sql server",
                                               minAge = minAge,
                                               maxAge = maxAge)
      super$setSql(sql = sql)
    }
  )
)


# YearDefinition -----

#' @description
#' An R6 class to define a YearDefinition object.
#'
#' @export
YearDefinition <- R6::R6Class("YearDefinition",
  inherit = LineItem,
  public = list(
    initialize = function() {
      super$setDomain("Year")
    },
    setMinYear = function(minYear) {
      private$minYear <- minYear
    },
    setMaxYear = function(maxYear) {
      private$maxYear <- maxYear
    }
  ),
  private = list(
    minYear = NA,
    maxYear = NA
  )
)


# ConceptSetDefinition ----

#' @description
#' An R6 class to define a ConceptSetDefinition
#'
#' @export
ConceptSetDefinition <- R6::R6Class("ConceptSetDefinition", list(
  inherit = LineItem,

  domainsToInclude = c(),
  typeConcepts = c(),
  sourceConcepts = c(),
  visitConceptIds = c(),
  specialtyConceptIds = c(),


  initialize = function() {

  },
  buildQuery = function() {

  }
))


# CohortDefinition ----

#' @description
#' An R6 class to define a CohortDefinition
#'
#' @export
CohortDefinition <- R6::R6Class("CohortDefinition", list(
  inherit = LineItem,
  covariateCohortId = NA,


  initialize = function(cohortId) {

    covariateValueSql <- "count(distinct TARGET.subject_id)"
    if (super$limit == "All") {
      covariateValueSql <- "count(TARGET.subject_id)"
    }

    dateFilterSql <- ""

    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "cohortTemplate.sql",
                                                  packageName = pkgload::pkg_name(),
                                                  dbms = super$executionSettings$dbms,
                                                  targetCohortIds = super$targetCohortIds,
                                                  workDatabaseSchema = super$executionSettings$workDatabaseSchema,
                                                  covariateCohortId = covariateCohortId,
                                                  covariateDatabaseSchema = super$executionSettings$covariateDatabaseSchema,
                                                  covariateCohortTable = super$executionSettings$covariateCohortTable,
                                                  dateFilterSql = dateFilterSql)

    super$sql <- ""
  }
))



# RaceEthnicityDefinition ------

#' @description
#' An R6 class to define a RaceEthnicityDefinition object.
#'
#' @export
RaceEthnicityDefinition <- R6::R6Class("RaceEthnicityDefinition", list(
  inherit = LineItem,
  raceConceptId = c(),
  ethnicityConceptIds = c(),
  mergeRaceEthnicity = FALSE
))


# ValueDefinition ----

#' @description
#' An R6 class to handle the ...
#'
#' @export
ValueDefinition <- R6::R6Class("ValueDefinition", list(
  inherit = LineItem,

  domainIds = c(),
  thresholdMin = NA,
  thresholdMax = NA,
  unitConceptIds = c(),
  unitConversions = c()
))

# UnitConversion ----

#' @description
#' An R6 class to handle the ...
#'
#' @export
UnitConversion <- R6::R6Class("UnitConversion", list(
  originalUnitConceptId = NA,
  targetUnitConceptId = NA,
  multiplierToOriginal = NA
))

# ExecutionSettings ----

#' @description
#' An R6 class to handle the ...
#'
#' @export
ExecutionSettings <- R6::R6Class("ExecutionSettings", list(
  cdmDatabaseSchema = NULL,
  workDatabaseSchema = NULL,
  tempEmulationSchema = NULL,
  targetCohortTable = NULL,
  covariateDatabaseSchema = NULL,
  covariateCohortTable = NULL,
  dbms = NULL,
  database = NULL,
  initialize = function() {
    # should we connect?
  }
))

# BreaksStrategy ----

#' @description
#' An R6 class to handle the ...
#'
#' @export
BreaksStrategy <- R6::R6Class("BreaksStrategy", list(
  name = NULL,
  breaks = NULL,
  initialize = function(name,
                        breaks) {
    self$name <- name
    self$breaks <- breaks
  }
))
