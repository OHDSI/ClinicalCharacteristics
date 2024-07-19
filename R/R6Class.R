

# TableShell -----

#' @description
#' An R6 class to define a TableShell object
#'
#' @export
TableShell <- R6::R6Class("TableShell",
  public = list(
    setTitle = function(title) {
      .setString(private = private, key = "title", value = title)
      invisible (self)
    },
    getTitle = function() {
      return (private$title)
    },
    addTargetCohorts = function(targetCohorts) {
      checkmate::assert_list(x = targetCohorts, classes = c("TargetCohort"), null.ok = FALSE, min.len = 1)
      private$targetCohorts <- c(private$targetCohorts, targetCohorts)
      invisible (self)
    },
    # TODO - addTargetCohortsFromCsv?
    getTargetCohorts = function() {
      return (private$targetCohorts)
    },
    addSections = function(sections) {
      checkmate::assert_list(x = sections, classes = c("Section"), null.ok = FALSE, min.len = 1)
      private$sections <- c(private$sections, sections)
      invisible (self)
    },
    getSections = function() {
      return (private$sections)
    },
    setExectionSettings = function(executionSettings) {
      checkmate::assert_class(x = executionSettings, class = "ExecutionSettings", null.ok = FALSE)
      private$executionSettings <- executionSettings
      invisible (self)
    },
    getExecutionSettings = function() {
      return (private$executionSettings)
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
       checkmate::assert_number(x = id, na.ok = FALSE, null.ok = FALSE)
       private$id <- id
       invisible (self)
     },
     getId = function(id) {
       return (private$id)
     },
     setName = function(name) {
       private$name <- name
       invisible (self)
     },
     getName = function(name) {
       return (private$name)
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
      invisible (self)
    },
    getTitle = function() {
      return (private$title)
    },
    setOrdinal = function(ordinal) {
      checkmate::assert_number(x = ordinal, na.ok = TRUE, null.ok = TRUE)
      private$ordinal <- ordinal
      invisible (self)
    },
    getOrdinal = function() {
      return (private$ordinal)
    },
    setLineItems = function(lineItems) {
      checkmate::assert_list(x = lineItems, types = c("LineItem"), min.len = 1)
      private$lineItems <- lineItems
      invisible (self)
    },
    getLineItems = function() {
      return (private$lineItems)
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

    setOrdinal <- function(ordinal) {
      checkmate::assert_number(x = ordinal, na.ok = TRUE, null.ok = TRUE)
    },
    getOrdinal <- function() {

    },
    setLabelCategory <- function(labelCategory) {
      checkmate::assert_number(x = labelCategory, na.ok = TRUE, null.ok = TRUE)
    },
    getLabelCategory <- function() {
      thisLabelCategory <- private$labelCategory
      return (thisLabelCategory)
    },

    checkmate::assert_string(x = itemLabel, null.ok = FALSE)

    setShowMissing = function(showMissing) {
      checkmate::assert_logical(x = showMissing, null.ok = FALSE, len = 1, any.missing = FALSE, all.missing = FALSE)
      private$showMissing <- showMissing
      invisible (private)
    },
    getShowMissing = function() {
      thisShowMissing <- private$showMissing
      return (thisShowMissing)
    },
    setStatType = function(statisticType) {
      checkmate::assert_choice(x = statisticType,
                               choices = .getAssertChoices(category = "StatisticType"))
      private$statisticType <- statisticType
      invisible (private)
    },
    getStatisticType = function() {
      thisStatType <- private$statType
      return (thisStatType)
    },
    # setDomain = function(domain) {
    #   checkmate::assert_choice(x = domain,
    #                            choices = .getAssertChoices(category = "Domain"))
    #   private$domain <- domain
    # },
    # getDomain = function() {
    #   return(private$domain)
    # },
    setLimit = function(limit) {
      checkmate::assert_choice(x = limit, choices = c(
        .getAssertChoices(category = "Limit")
      ))
      private$limit <- limit
      invisible (private)
    },
    getLimit = function() {
      thisLimit <- private$limit
      return (thisLimit)
    },
    setDefinition = function(definition) {
      # checkmate::assert_class(x = definition,
      #                         classes = .getAssertChoices(category = "DefinitionType"))


      #categorical <- .getStatisticTypes(definitionType)

      private$definition <- definition
    },
    getDefinition = function() {
      return(private$definition)
    },
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
      checkmate::assert_class(x = timeWindows, classes = c("TimeWindows"))
      private$timeWindows <- timeWindows
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
    definition = NULL,
    sql = NULL,
    timeWindows = NULL,
    domain = NULL,
    limit = NULL,
    definition = NULL,
    statisticType = NULL
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


# ExecutionSettings ----

#' @description
#' An R6 class to handle the ...
#'
#' @export
ExecutionSettings <- R6::R6Class("ExecutionSettings", list(
  connectionDetails = NULL,
  connection = NULL,
  cdmDatabaseSchema = NULL,
  workDatabaseSchema = NULL,
  tempEmulationSchema = NULL,
  targetCohortTable = NULL,
  cdmSourceName = NULL,
  numThreads = NULL, # Maybe

  initialize = function(connection = NULL,
                        connectionDetails = NULL,
                        cdmDatabaseSchema = NULL,
                        workDatabaseSchema = NULL,
                        tempEmulationSchema = NULL,
                        targetCohortTable = NULL,
                        cdmSourceName = NULL,
                        numThreads = NULL) {

    # TODO: resolve these checkmates to fit our new parameter structure

    # if connection is not null, then numThreads = 1
    # else numThreads = whatever the user stated

    checkmate::assert_string(x = cdmDatabaseSchema, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
    checkmate::assert_string(x = workDatabaseSchema, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
    checkmate::assert_string(x = tempEmulationSchema, na.ok = TRUE, null.ok = TRUE)
    checkmate::assert_string(x = targetCohortTable, na.ok = FALSE, null.ok = FALSE, min.chars = 1)



    checkmate::assert_string(x = dbms, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
    checkmate::assert_string(x = dbms, na.ok = TRUE, null.ok = TRUE)
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
