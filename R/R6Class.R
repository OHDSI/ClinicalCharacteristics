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
    numThreads = NULL,
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


# TableShell -----

#' @description
#' An R6 class to define a TableShell object
#'
#' @export
TableShell <- R6::R6Class("TableShell",
  public = list(
    initialize = function(name,
                          targetCohorts,
                          executionSettings,
                          sections) {
      .setString(private = private, key = "name", value = name)
      .setListofClasses(private = private, key = "targetCohorts", value = targetCohorts, classes = c("TargetCohort"))
      .setListofClasses(private = private, key = "sections", value = sections, classes = c("Section"))
      .setClass(private = private, key = "executionSettings", value = executionSettings, class = "ExecutionSettings")
    },
    getName = function() {
      tsName <- private$name
      return(tsName)
    },
    getTargetCohorts = function() {
      tsTargetCohorts <- private$targetCohorts
      return(tsTargetCohorts)
    },
    getSections = function() {
      tsSections <- private$sections
      return(tsSections)
    }
  ),
  private = list(
    name = NULL,
    sections = NULL,
    targetCohorts = NULL,
    executionSettings = NULL
  )
)

# Target Cohort -----

#' @description
#' An R6 class to define a Target Cohort object
#' TargetCohort objects do not maintain any execution settings, just the id and name
#'
#' @export
TargetCohort <- R6::R6Class("TargetCohort",
  public = list(
    initialize = function(id, name) {
      .setNumber(private = private, key = "id", value = id)
      .setString(private = private, key = "name", value = name)
    },
    getId = function() {
      tcId <- private$id
      return(tcId)
    },
    getName = function(name) {
      tcName <- private$name
      return(tcName)
    },
    getSql = function() {
      sqlFile <- "targetCohort.sql"
      cohortIds <- private$id
      # get sql from package
      sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
        readr::read_file() |>
        glue::glue()
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
#' A Section object is a logical encapsulation of LineItems
#' Sections have names and ordinals for the final output
#'
#' @export
Section <- R6::R6Class("Section",
  public = list(
    initialize = function(name,
                          ordinal,
                          lineItems) {
      .setString(private = private, key = "name", value = name)
      .setNumber(private = private, key = "ordinal", value = ordinal)
      .setListofClasses(private = private, key = "lineItems", value = lineItems, classes = c("LineItem"))
    },
    getName = function() {
      name <- private$name
      return(name)
    },
    getOrdinal = function() {
      sectionOrdinal <- private$ordinal
      return(sectionOrdinal)
    },
    getLineItems = function() {
      sectionLineItems <- private$lineItems
      return(sectionLineItems)
    }
  ),
  private = list(
    name = NULL,
    ordinal = NA,
    lineItems = NULL
  )
)


# Statistic Class ---------------------

# Statistic Super-------------

#' @title
#' An R6 class to define a Statistic object
#'
#' @description
#' A Statistic is a type of metric to be used for characterization
#' Specific types of statistics are defined in derived classes
#'
#' @export
Statistic <- R6::R6Class("Statistic",
                         public = list(
                           initialize = function(type) {
                             .setString(private = private , key = "type", value = type)
                           },

                           # helper to get state type from class
                           getStatType = function() {
                             statType <- private$type
                             return(statType)
                           }
                         ),
                         private = list(
                           type = NULL
                         )
)

## Demographic Stats

### Demo Age -----------------
Age <- R6::R6Class("DemoAge",
                   inherit = Statistic,
                   public = list(
                     initialize = function(breaks = NULL) {
                       super$initialize(type = "Age")
                       if (!is.null(breaks)) {
                         .setClass(private = private, key = "breaks", value = breaks, class = "Breaks")
                       }
                       invisible(private)
                     }
                   ),
                   private = list(
                     breaks = NULL
                   )
)


### Demo Concept -----------------
DemoConcept <- R6::R6Class("DemoConcept",
                           inherit = Statistic,
                           public = list(
                             initialize = function(conceptColumn) {
                               super$initialize(type = "DemoConcept")
                               .setString(private = private, key = 'conceptColumn', value = conceptColumn)
                               invisible(private)
                             },
                             getDemoColumn = function() {
                               col <- private$conceptColumn
                               return(col)
                             }
                           ),
                           private = list(
                             conceptColumn = NULL
                           )
)


### Demo Year -----------------
DemoYear <- R6::R6Class("DemoYear",
                           inherit = Statistic,
                           public = list(
                             initialize = function(breaks = NULL) {
                               super$initialize(type = "Year")
                               if (!is.null(breaks)) {
                                 .setClass(private = private,
                                           key = "breaks",
                                           value = breaks,
                                           class = "Breaks")
                               }
                               invisible(private)
                             }
                           ),
                           private = list(
                             breaks = NULL
                           )
)


## Presence -----------------------

#' @title
#' An R6 class to define a Presence object
#'
#' @description
#' Child of Statistic. The Presence statistic is a binary metric the indicates the presence of a variable
#'
#' @export
Presence <- R6::R6Class("Presence",
                        inherit = Statistic,
                        public = list(
                          initialize = function(operator,
                                                occurrences) {
                            super$initialize(type = "Presence")
                            # TODO change this to enforce operator from choice list
                            .setString(private = private, key = "operator", value = operator)
                            .setNumber(private = private, key = "occurrences", value = occurrences)
                            invisible(private)
                          }
                        ),
                        private = list(
                          operator = NULL,
                          occurrences = NA
                        )
)


## Count -----------------------

#' @title
#' An R6 class to define a Count object
#'
#' @description
#' Child of Statistic. The Count statistic is a poisson metric the indicates the number of occurrences of a variable
#'
#' @export
Count <- R6::R6Class("Count",
                     inherit = Statistic,
                     public = list(
                       initialize = function(breaks = NULL) {
                         super$initialize(type = "Count")
                         # TODO change this to enforce operator from choice list
                         if (!is.null(breaks)) {
                           .setClass(private = private, key = "breaks", value = breaks, class = "Breaks")
                         }
                         invisible(private)
                       }
                     ),
                     private = list(
                       breaks = NULL
                     )
)


# LineItem Classes -----

## Line Item Super----------

#' @description
#' An R6 class to define a LineItem object
#' A LineItem is a single, explicitly defined characterization to appear in a Section
#' Derived classes exist off of LineItems
#'
#' @export
LineItem <- R6::R6Class("LineItem",
  public = list(
    initialize = function(name,
                          ordinal,
                          definitionType,
                          statistic) {
      .setString(private = private, key = "name", value = name)
      .setNumber(private = private, key = "ordinal", value = ordinal)
      # TODO change this to enforce definitionType from choice list
      .setString(private = private, key = "definitionType", value = definitionType)
      .setClass(private = private, key = "statistic", value = statistic, class = "Statistic")

      invisible(self)
    },
    getName = function() {
      name <- private$name
      return(name)
    },
    getOrdinal = function() {
      liOrdinal <- private$ordinal
      return(liOrdinal)
    },
    getDefinitionType = function() {
      liDefinitionType <- private$definitionType
      return(liDefinitionType)
    }
  ),

  private = list(
    name = NULL,
    ordinal = NA,
    definitionType = NULL,
    statistic = NULL
  )
)

## ConceptSetDefinition ----

#' @description
#' An R6 class to define a ConceptSetDefinition
#'
#' @export
ConceptSetDefinition <- R6::R6Class("ConceptSetDefinition",
  inherit = LineItem,
  public = list(

    # initialize the class
    initialize = function(name,
                          ordinal,
                          statistic,
                          domain,
                          conceptSet,
                          timeInterval,
                          sourceConceptSet = NULL,
                          typeConceptIds = c(),
                          visitOccurrenceConceptIds = c()) {
      super$initialize(name = name, ordinal = ordinal, definitionType = "ConceptSet",
                       statistic = statistic)
      .setString(private = private, key = "domain", value = domain)
      .setClass(private = private, key = "conceptSet", value = conceptSet, class = "ConceptSet")
      .setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeInterval")
      # TODO change this to enforce domain from choice list
      .setClass(private = private, key = "sourceConceptSet", value = sourceConceptSet, class = "ConceptSet", nullable = TRUE)
      .setNumber(private = private, key = "typeConceptIds", value = typeConceptIds, nullable = TRUE)
      .setNumber(private = private, key = "visitOccurrenceConceptIds", value = visitOccurrenceConceptIds, nullable = TRUE)
    },

    # helper to pull concept Capr class items
    grabConceptSet = function() {
      cs <- private$conceptSet
      return(cs)
    },

    # helper to get reference table of the concept sets in the class
    getConceptSetRef = function() {
      # # make key for cs use
      csTbl <- tibble::tibble(
        'name' = private$conceptSet@Name,
        'hash' = private$conceptSet@id
      )
      return(csTbl)
    },

    # helper to get the domain within the class
    getDomain = function() {
      dm <- private$domain
      return(dm)
    },

    # helper to retrieve the time windows in the clas
    getTimeInterval = function() {
      tw <- private$timeInterval$getTimeInterval()
      return(tw)
    },

    getStatisticType = function() {
      statNm <- private$statistic$getStatType()
      return(statNm)
    }
  ),
  private = list(
    domain = NULL,
    conceptSet = NULL,
    timeInterval = NULL,
    sourceConceptSet = NULL,
    typeConceptIds = c(),
    visitOccurrenceConceptIds = c()
  )
)

# DemographicDefinition -----

#' @description
#' An R6 class to handle the ...
#'
#' @export
DemographicDefinition <- R6::R6Class("DemographicDefinition",
  inherit = LineItem,
  public = list(
    initialize = function(name,
                          ordinal,
                          statistic) {
      super$initialize(name = name,
                       ordinal = ordinal,
                       definitionType = "Demographic",
                       statistic = statistic)
    },
    # function to get sql
    getSql = function() {

      # get stat type
      statType <- private$statistic$getStatType()

      # prep sql if Age demographic
      if (statType == "Age") {
        sqlFile <- "demoAgeChar.sql"
        ordinal <- private$ordinal
        # get sql from package
        sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
          readr::read_file() |>
          glue::glue()
      }

      # prep sql if Concept demographic
      if (statType == "DemoConcept") {
        sqlFile <- "demoConceptChar.sql"
        ordinal <- private$ordinal
        conceptColumn <- private$statistic$getDemoColumn()
        # get sql from package
        sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
          readr::read_file() |>
          glue::glue()
      }

      # prep sql if Concept demographic
      if (statType == "Year") {
        sqlFile <- "demoYearChar.sql"
        ordinal <- private$ordinal
        # get sql from package
        sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
          readr::read_file() |>
          glue::glue()
      }


      return(sql)
    }
  )
)



# Helper Classes -----

## TimeInterval ------

TimeInterval <- R6::R6Class(
  "TimeInterval",
  public = list(
    initialize = function(lb, rb) {
      .setNumber(private = private, key = "lb", value = lb)
      .setNumber(private = private, key = "rb", value = rb)
      invisible(self)
    },
    getTimeInterval = function() {
      tb <- tibble::tibble(
        lb = private$lb,
        rb = private$rb
      )
      return(tb)
    }
  ),
  private = list(
    'lb' = NA_integer_,
    'rb' = NA_integer_
  )
)


## TimeWindow ------
#
# TimeWindow <- R6::R6Class(
#   "TimeWindow",
#   public = list(
#     initialize = function(windows) {
#       .setListofClasses(
#         private = private,
#         key = "windows",
#         value = windows,
#         classes = c("TimeInterval")
#       )
#       invisible(self)
#     },
#     length = function() {
#       ll <- length(private$windows)
#       return(ll)
#     },
#     getTimeIntervals = function() {
#
#       tis <- purrr::map_dfr(
#         private$windows,
#         ~tibble::tibble(
#           'lb' = .x$getLeftBound(),
#           'rb' = .x$getRightBound()
#         )
#       )
#       return(tis)
#     }
#   ),
#   private = list(
#     windows = NULL
#   )
# )

# # GenderDefinition -----
#
# #' @description
# #' An R6 class to define a GenderDefinition object.
# #'
# #' @export
# GenderDefinition <- R6::R6Class("GenderDefinition",
#  inherit = LineItem,
#  public = list(
#    initialize = function(name,
#                          genderConceptIds) {
#      super$setDomainIds(domainIds = "Gender")
#      super$setName(name = name)
#      super$setConceptIds(conceptIds = genderConceptIds)
#
#      templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
#      super$setTemplateSql(templateSql = templateSql)
#    }
#  )
# )




# # IndexYearDefinition -----

# #' @description
# #' An R6 class to define a IndexYearDefinition object.
# #'
# #' @export
# IndexYearDefinition <- R6::R6Class("IndexYearDefinition",
#  inherit = LineItem,
#  public = list(
#    initialize = function(name = indexYear,
#                          indexYear) {
#      super$setName(name = name)
#      super$setDomainIds(domainIds = "IndexYear")
#      super$setMinThreshold(minThreshold = indexYear)
#      super$setMaxThreshold(maxThreshold = indexYear) # is it fine to just stuff this in min and max thresholds?
#
#      templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
#      super$setTemplateSql(templateSql = templateSql)
#    }

# # ConceptSetGroupDefinition ----

# #' @description
# #' An R6 class to define a ConceptSetGroupDefinition
# #' A line item that uses a Concept Set Group
# #'
# #' @export
# ConceptSetGroupDefinition <- R6::R6Class("ConceptSetGroupDefinition",
#  inherit = LineItem,
#  public = list(
#    initialize = function(conceptSetDefinitions) {
#      .setListofClasses(private = private, key = "conceptSetDefinitions",
#                        value = conceptSetDefinitions,
#                        classes = c("ConceptSetDefinition"))
#    },
#    getConceptSetDefinitions = function() {
#      conceptSetDefinitions <- private$conceptSetDefinitions
#      return(conceptSetDefinitions)
#     }
#   ),
#   private = list(
#     conceptSetDefinitions = c()
#   )
# ))




# # CohortDefinition ----

# #' @description
# #' An R6 class to define a CohortDefinition
# #'
# #' @export
# CohortDefinition <- R6::R6Class("CohortDefinition",
#   inherit = LineItem,
#   public = list(
#     initialize = function(name,
#                           cohortDefinitionId, cohortDatabaseSchema, cohortTable) {
#       super$setName(name = name)
#       super$setDomainIds(domainIds = "Cohort")
#       super$setAssetId(assetId = cohortDefinitionId)
#
#      # Could simply use an execution settings object, but the scope here is much less, as we
#      # need the same connection and database
#      .setString(private = private, key = "cohortDatabaseSchema", value = cohortDatabaseSchema)
#      .setString(private = private, key = "cohortTable", value = cohortTable)
#
#       templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
#       super$setTemplateSql(templateSql = templateSql)
#     }
#   ),
#     private = list(
#     cohortDatabaseSchema = NULL,
#     cohortTable = NULL)
# )

# # RaceDefinition ------

# #' @description
# #' An R6 class to define a RaceDefinition object.
# #'
# #' @export
# RaceDefinition <- R6::R6Class("RaceDefinition", list(
#   inherit = LineItem,
#   public = list(
#     initialize = function(name,
#                           raceConceptIds) {
#       super$setDomainIds(domainIds = "Race")
#       super$setName(name = name)
#       super$setConceptIds(conceptIds = raceConceptIds)

#       templateSql <- "select something;" ## TODO as SqlRender::loadRenderTranslateSql()
#       super$setTemplateSql(templateSql = templateSql)
#     },
#     getRaceConceptIds = function() {
#      raceConceptIds <- private$raceConceptIds
#      return(raceConceptIds)
#     }
#   )
# ))

# ######## MORE TO-DO #####


# # ValueDefinition ----

# #' @description
# #' An R6 class to handle the ...
# #'
# #' @export
# ValueDefinition <- R6::R6Class("ValueDefinition", list(
#   inherit = LineItem,

#   domainIds = c(),
#   thresholdMin = NA,
#   thresholdMax = NA,
#   unitConceptIds = c(),
#   unitConversions = c()
# ))

# # UnitConversion ----

# #' @description
# #' An R6 class to handle the ...
# #'
# #' @export
# UnitConversion <- R6::R6Class("UnitConversion", list(
#   originalUnitConceptId = NA,
#   targetUnitConceptId = NA,
#   multiplierToOriginal = NA
# ))

# # BreaksStrategy ----

# #' @description
# #' An R6 class to handle the ...
# #'
# #' @export
# BreaksStrategy <- R6::R6Class("BreaksStrategy", list(
#   name = NULL,
#   breaks = NULL,
#   initialize = function(name,
#                         breaks) {
#     self$name <- name
#     self$breaks <- breaks
#   }
# ))
