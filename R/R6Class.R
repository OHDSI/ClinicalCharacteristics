

# TableShell -----

#' @description
#' An R6 class to define a TableShell object
#'
#' @export
TableShell <- R6::R6Class("TableShell",
  public = list(
    setTitle = function(title) {
      .setString(private = private,
                 key = "title",
                 value = title)
    },
    getTitle = function() {
      return(.get(private, "title"))
    },
    setTargetCohortIds = function(targetCohortIds) {
      checkmate::assert_numeric(x = targetCohortIds, null.ok = TRUE)
      private$targetCohortIds <- targetCohortIds
    },
    getTargetCohortIds = function() {
      return(private$targetCohortIds)
    },
    setSections = function(sections) {
      checkmate::assert_list(x = sections, types = c("Section"), min.len = 1)
      private$sections <- sections
    },
    getSections = function() {
      return(private$sections)
    },
    setExectionSettings = function(executionSettings) {
      private$executionSettings <- executionSettings
    },
    getExecutionSettings = function() {
      return (private$executionSettings)
    }
  ),
  private = list(
    title = NULL,
    sections = NULL,
    executionSettings = NULL
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
      .setString(private = private,
                 key = "title",
                 value = title)
    },
    getTitle = function() {
      return(.get(private, "title"))
    },
    setOrdinal = function(ordinal) {
      checkmate::assert_number(x = sectionOrdinal, na.ok = TRUE, null.ok = TRUE)
      private$ordinal <- ordinal
    },
    getOrdinal = function() {
      return(private$ordinal)
    },
    setTableShell = function(tableShell) {
      checkmate::assert_class(x = tableShell, classes = c("TableShell"))
      private$tableShell <- tableShell
    },
    getTableShell = function() {
      return(private$tableShell)
    },
    setTargetCohortIds = function(targetCohortIds) {
      checkmate::assert_numeric(x = targetCohortIds, null.ok = TRUE)
      private$targetCohortIds <- targetCohortIds
      private$lineItems <- .cascadeObject(cascadeFrom = private,
                                          cascadeName = "targetCohortIds",
                                          cascadeObjects = private$sections)

    },
    getTargetCohortIds = function() {
      return(private$targetCohortIds)
    },
    setLineItems = function(lineItems) {
      checkmate::assert_list(x = lineItems, types = c("LineItem"), min.len = 1)
      private$lineItems <- lineItems
    },
    getLineItems = function() {
      return(private$lineItems)
    }
  ),
  private = list(
    tableShell = NULL,
    title = NULL,
    ordinal = NA,
    lineItems = NULL,
    targetCohortIds = NULL
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
    itemLabelCategory = NULL,
    itemLabel = NULL,

    initialize = function(itemOrdinal = NA,
                          itemLabelCategory = NULL,
                          itemLabel = NULL) {

      checkmate::assert_number(x = itemOrdinal, na.ok = TRUE, null.ok = TRUE)
      checkmate::assert_string(x = itemLabelCategory, null.ok = FALSE)
      checkmate::assert_string(x = itemLabel, null.ok = FALSE)

      self$itemOrdinal <- itemOrdinal
      self$itemLabelCategory <- itemLabelCategory
      self$itemLabel <- itemLabel
    },

    setTableShell = function(tableShell) {
      checkmate::assert_class(x = tableShell, classes = c("TableShell"))
      private$tableShell <- tableShell
    },
    getTableShell = function() {
      return(private$tableShell)
    },
    setShowMissing = function(showMissing) {
      checkmate::assert_logical(x = showMissing, null.ok = FALSE, len = 1, any.missing = FALSE, all.missing = FALSE)
      private$showMissing <- showMissing
    },
    getShowMissing = function() {
      return(private$showMissing)
    }
    setStatisticType = function(statisticType) {
      checkmate::assert_choice(x = statisticType,
                               choices = .getAssertChoices(category = "StatisticType"))
      private$statisticType <- statisticType
    },
    getStatisticType = function() {
      return(private$statisticType)
    },
    setDomain = function(domain) {
      checkmate::assert_choice(x = domain,
                               choices = .getAssertChoices(category = "Domain"))
      private$domain <- domain
    },
    getDomain = function() {
      return(private$domain)
    },
    setLimit = function(limit) {
      checkmate::assert_choice(x = limit, choices = c(
        .getAssertChoices(category = "Limit")
      ))
      private$limit <- limit
    },
    getLimit = function() {
      return(private$limit)
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
    setSql = function(sql) {
      # here, we translate as a final step
      checkmate::assert_string(x = sql, na.ok = FALSE, null.ok = FALSE)
      private$sql <- SqlRender::translate(sql = sql,
                                          targetDialect = private$tableShell$getExecutionSettings()$dbms,
                                          tempEmulationSchema = private$tableShell$getExecutionSettings()$tempEmulationSchema)
    },
    getSql = function() {
      return(private$sql)
    },
    setTimeWindows = function(timeWindows) {
      checkmate::assert_class(x = timeWindows, classes = c("TimeWindows"))
      private$timeWindows <- timeWindows
    },
    getTimeWindows = function() {
      return(private$timeWindows)
    }
  ),
  private = list(
    tableShell = NULL,
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
  cdmDatabaseSchema = NULL,
  workDatabaseSchema = NULL,
  tempEmulationSchema = NULL,
  targetCohortTable = NULL,
  covariateDatabaseSchema = NULL,
  covariateCohortTable = NULL,
  dbms = NULL,
  database = NULL,
  initialize = function(cdmDatabaseSchema = NULL,
                        workDatabaseSchema = NULL,
                        tempEmulationSchema = NULL,
                        targetCohortTable = NULL,
                        covariateDatabaseSchema = NULL,
                        covariateCohortTable = NULL,
                        dbms = NULL,
                        database = NULL) {

    checkmate::assert_string(x = cdmDatabaseSchema, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
    checkmate::assert_string(x = workDatabaseSchema, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
    checkmate::assert_string(x = tempEmulationSchema, na.ok = TRUE, null.ok = TRUE)
    checkmate::assert_string(x = targetCohortTable, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
    checkmate::assert_string(x = covariateDatabaseSchema, na.ok = TRUE, null.ok = TRUE)
    checkmate::assert_string(x = covariateCohortTable, na.ok = TRUE, null.ok = TRUE)
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
