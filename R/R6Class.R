# TableShell -----

#' @description
#' An R6 class to define a TableShell object
#'
#' @export
TableShell <- R6::R6Class("TableShell",
  public = list(
    setTitle = function(title) {
      .setString(private = private, key = "title", value = title)
      invisible(self)
    },
    getTitle = function() {
      return(private$title)
    },
    addTargetCohorts = function(targetCohorts) {
      .setListofClasses(private = private, key = "targetCohorts", value = targetCohorts, classes = c("TargetCohort"))
      invisible(self)
    },
    # TODO - addTargetCohortsFromCsv, addTargetCohortsFromDf
    getTargetCohorts = function() {
      return(private$targetCohorts)
    },
    addSections = function(sections) {
      .setListofClasses(private = private, key = "sections", value = sections, classes = c("Section"))
      invisible(self)
    },
    getSections = function() {
      return(private$sections)
    },
    setExectionSettings = function(executionSettings) {
      .setClass(private = private, key = "executionSettings", value = executionSettings, class = "ExecutionSettings")
      invisible(self)
    },
    getExecutionSettings = function() {
      return(private$executionSettings)
    }
  ),
  private = list(
    title = NULL,
    sections = c(),
    executionSettings = NULL,
    targetCohorts = c()
  )
)

# Target Cohort -----

#' @description
#' An R6 class to define a Target Cohort object
#'
#' @export
TargetCohort <- R6::R6Class("TargetCohort",
  public = list(
    setId = function(id) {
      .setNumber(private = private, key = "id", value = id)
      invisible(self)
    },
    getId = function(id) {
      return(private$id)
    },
    setName = function(name) {
      .setString(private = private, key = "name", value = name)
      invisible(self)
    },
    getName = function(name) {
      return(private$name)
    }
  ),
  private = list(
    id = NULL,
    name = NULL
  )
)

# Section ------

#' @description
#' An R6 class to define a Section object
#'
#' @export
Section <- R6::R6Class("Section",
  public = list(
    setTitle = function(title) {
      .setString(private = private, key = "title", value = title)
      invisible(self)
    },
    getTitle = function() {
      return(private$title)
    },
    setOrdinal = function(ordinal) {
      .setNumber(private = private, key = "ordinal", value = ordinal)
      invisible(self)
    },
    getOrdinal = function() {
      return(private$ordinal)
    },
    setLineItems = function(lineItems) {
      .setListofClasses(private = private, key = "lineItems", value = lineItems, classes = c("LineItem"))
      invisible(self)
    },
    getLineItems = function() {
      return(private$lineItems)
    }
  ),
  private = list(
    title = NULL,
    ordinal = NA,
    lineItems = c()
  )
)


# LineItem -----

#' @description
#' An R6 class to define a LineItem object
#'
#' @export
LineItem <- R6::R6Class("LineItem",
  public = list(
    setOrdinal = function(ordinal) {
      .setNumber(private = private, key = "ordinal", value = ordinal)
      invisible(self)
    },
    getOrdinal = function() {
      return(private$ordinal)
    },
    setLabelCategory = function(labelCategory) {
      .setNumber(private = private, key = "labelCategory", value = labelCategory)
      invisible(self)
    },
    getLabelCategory = function() {
      return(private$labelCategory)
    },
    setLabel = function(label) {
      .setString(private = private, key = "label", value = label)
      invisible(self)
    },
    setShowMissing = function(showMissing) {
      .setLogical(private = private, key = "showMissing", value = showMissing)
      invisible(self)
    },
    getShowMissing = function() {
      return(private$showMissing)
    },
    setStatisticType = function(statisticType) {
      .setChoice(private = private, key = "statisticType", value = statisticType, choices = .getAssertChoices(category = "StatisticType"))
      invisible(self)
    },
    getStatisticType = function() {
      return(private$statisticType)
    },
    # TODO decide if we want to allow setting of domain or always look across all domains (the latter feels inefficient...)
    # setDomain = function(domain) {
    #   checkmate::assert_choice(x = domain,
    #                            choices = .getAssertChoices(category = "Domain"))
    #   private$domain <- domain
    # },
    # getDomain = function() {
    #   return(private$domain)
    # },
    setLimit = function(limit) {
      .setChoice(private = private, key = "limit", value = limit, choices = .getAssertChoices(category = "Limit"))
      invisible(self)
    },
    getLimit = function() {
      return(private$limit)
    },
    # setDefinition = function(definition) {
    #   checkmate::assert_class(x = definition,
    #                           classes = .getAssertChoices(category = "DefinitionType"))


    #   categorical <- .getStatisticTypes(definitionType)

    #   private$definition <- definition
    # },
    # getDefinition = function() {
    #   return(private$definition)
    # },
    # setSql = function(sql) {
    #   # here, we translate as a final step
    #   checkmate::assert_string(x = sql, na.ok = FALSE, null.ok = FALSE)
    #   private$sql <- SqlRender::translate(sql = sql,
    #                                       targetDialect = private$tableShell$getExecutionSettings()$dbms,
    #                                       tempEmulationSchema = private$tableShell$getExecutionSettings()$tempEmulationSchema)
    # },
    # getSql = function() {
    #   return(private$sql)
    # },
    setTimeWindows = function(timeWindows) {
      .setClass(private = private, key = "timeWindows", value = timeWindows, class = "TimeWindows")
      return(self)
    },
    getTimeWindows = function() {
      return(private$timeWindows)
    }
  ),
  private = list(
    ordinal = NA,
    labelCategory = NULL,
    label = NULL,
    showMissing = NULL,
    statisticType = NULL,
    #domain = NULL,
    #definition = NULL,
    #sql = NULL,
    limit = NULL,
    definition = NULL,
    timeWindows = NULL
  )
)

# ExecutionSettings ----

#' @description
#' An R6 class to define an ExecutionSettings object
#'
#' @export
ExecutionSettings <- R6::R6Class("ExecutionSettings", 
  public = list(
    connectionDetails = NULL,
    connection = NULL,
    cdmDatabaseSchema = NULL,
    workDatabaseSchema = NULL,
    tempEmulationSchema = NULL,
    targetCohortTable = NULL,
    cdmSourceName = NULL,
    numThreads = NULL, # Maybe
    initialize = function(connectionDetails = NULL,
                          connection = NULL,
                          cdmDatabaseSchema = NULL,
                          workDatabaseSchema = NULL,
                          tempEmulationSchema = NULL,
                          targetCohortTable = NULL,
                          cdmSourceName = NULL,
                          numThreads = NULL) {
      stopifnot(is.null(connectionDetails) || is.null(connection))
      checkmate::assert_string(x = connectionDetails, na.ok = TRUE, null.ok = TRUE)
      checkmate::assert_string(x = connection, na.ok = TRUE, null.ok = TRUE)
      checkmate::assert_string(x = cdmDatabaseSchema, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
      checkmate::assert_string(x = workDatabaseSchema, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
      checkmate::assert_string(x = tempEmulationSchema, na.ok = TRUE, null.ok = TRUE)
      checkmate::assert_string(x = targetCohortTable, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
      checkmate::assert_string(x = cdmSourceName, na.ok = TRUE, null.ok = TRUE)
      checkmate::assert_number(x = numThreads, na.ok = TRUE, null.ok = TRUE)

      if (!is.null(connection)) {
        self$connection <- connection
        self$numThreads <- 1
      } else {
        self$connectionDetails <- connectionDetails
        self$numThreads <- numThreads
      }

      self$cdmDatabaseSchema <- cdmDatabaseSchema
      self$workDatabaseSchema <- workDatabaseSchema
      self$tempEmulationSchema <- tempEmulationSchema
      self$targetCohortTable <- targetCohortTable
      self$cdmSourceName <- cdmSourceName
    }
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
      checkmate::assert_choice(x = genderConceptIds, choices = c(8507, 8532))
      super$setDomain(domain = "Gender")
      super$setStatisticType(statisticType = "Demographics")

      caseSql <- .getCaseSql(covariateValues)

      rawDataSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "genderTemplate.sql",
                                                      packageName = pkgload::pkg_name(),
                                                      dbms = "sql server",
                                                      genderConceptIds = genderConceptIds)
      statSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "categoricalTemplate.sql",
                                                   packageName = pkgload::pkg_name(),
                                                   dbms = "sql server",
                                                   rawDataSql = rawDataSql,
                                                   caseSql = caseSql)
      super$setSql(sql = statSql)
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
    initialize = function(minAges,
                          maxAges) {
      #checkmate::assert_number(x = minAge, na.ok = FALSE)
      #checkmate::assert_number(x = maxAge, na.ok = FALSE)

      checkmate::check_array(x = minAges)
      checkmate::check_array(x = maxAges)

      super$setDomain(domain = "Age")
      super$setStatisticType(statisticType = "Demographics")

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
CohortDefinition <- R6::R6Class("CohortDefinition",
  public = list(
    covariateCohortId = NA,
    initialize = function(covariateCohortId,
                          executionSettings) {
      dateFilterSql <- ""

      sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "cohortTemplate.sql",
                                               packageName = pkgload::pkg_name(),
                                               dbms = executionSettings$dbms,
                                               #targetCohortIds = super$targetCohortIds,
                                               #workDatabaseSchema = super$executionSettings$workDatabaseSchema,
                                               covariateCohortId = covariateCohortId,
                                               covariateDatabaseSchema = super$executionSettings$covariateDatabaseSchema,
                                               covariateCohortTable = super$executionSettings$covariateCohortTable,
                                               dateFilterSql = dateFilterSql)

      super$sql <- ""
    }
  ),
  private = list(

  )
)


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

