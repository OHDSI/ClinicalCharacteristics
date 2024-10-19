
LineItem <- R6::R6Class(
  classname = "LineItem",
  public = list(
    initialize = function(
      sectionLabel,
      lineItemLabel = NA_character_,
      domainTable,
      lineItemClass,
      valueId = NA_integer_,
      valueDescription  = NA_integer_,
      statistic,
      timeInterval
    ) {
      .setString(private = private, key = ".sectionLabel", value = sectionLabel)
      .setString(private = private, key = ".lineItemLabel", value = lineItemLabel, naOk = TRUE)
      .setString(private = private, key = ".domainTable", value = domainTable)
      .setString(private = private, key = ".lineItemClass", value = lineItemClass)
      .setInteger(private = private, key = ".valueId", value = valueId)
      .setString(private = private, key = ".valueDescription", value = valueDescription, naOk = TRUE)
      .setClass(private = private, key = "statistic", value = statistic, class = "Statistic")
      .setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeInterval", nullable = TRUE)
    },

    getLineItemMeta = function() {

      tw <- private$timeInterval
      if (is.null(tw)) {
        timeLabel <- "Static at Index"
      } else {
        timeLabel <- tw$getTimeLabel()
      }

      tb <- tibble::tibble(
        ordinalId = private$.ordinalId,
        sectionLabel = private$.sectionLabel,
        linetItemLabel = private$.lineItemLabel,
        valueId = private$.valueId,
        valueDescription = private$.valueDescription,
        timeLabel = timeLabel,
        statisticType = class(private$statistic)[[1]],
        domainTable = private$.domainTable,
        lineItemClass = private$.lineItemClass
      )

      return(tb)
    }

  ),
  private = list(
    .ordinalId = NA_integer_,
    .sectionLabel = NA_character_,
    .lineItemLabel = NA_character_,
    .valueId = NA_integer_,
    .valueDescription = NA_character_,
    timeInterval = NULL,
    statistic = NULL,
    .domainTable = NA_character_,
    .lineItemClass = NA_character_
  ),
  active = list(
    ordinalId = function(ordinalId) {
      .setActiveInteger(private = private, key = ".ordinalId", value = ordinalId)
    },
    sectionLabel = function(sectionLabel) {
      .setActiveString(private = private, key = ".sectionLabel", value = sectionLabel)
    },
    lineItemLabel = function(lineItemLabel) {
      .setActiveString(private = private, key = ".lineItemLabel", value = lineItemLabel)
    },
    valueId = function(valueId) {
      .setActiveInteger(private = private, key = ".valueId", value = valueId)
    },
    valueDescription = function(valueDescription) {
      .setActiveString(private = private, key = ".valueDescription", value = valueDescription)
    },
    domainTable = function(domainTable) {
      .setActiveString(private = private, key = ".domainTable", value = domainTable)
    },
    lineItemClass = function(lineItemClass) {
      .setActiveString(private = private, key = ".lineItemClass", value = lineItemClass)
    }
  )
)

Statistic <- R6::R6Class(
  classname = "Statistic",
  public = list(
    initialize = function(label, type) {
      .setString(private = private , key = "label", value = label)
      .setString(private = private , key = "type", value = type)
    },
    getStatType = function() {
      statType <- private$type
      return(statType)
    },
    getStatLabel = function() {
      statLabel <- private$label
      return(statLabel)
    }
  ),
  private = list(
    label = NA_character_,
    type = NA_character_
  )
)


ContinuousAge <- R6::R6Class(
  classname = "ContinuousAge",
  inherit = Statistic,
  public = list(
    initialize = function() {
      super$initialize(label = "Age", type = "Continuous")
    }
  )
)

CategoricalAge <- R6::R6Class(
  classname = "CategoricalAge",
  inherit = Statistic,
  public = list(
    initialize = function(breaks) {
      super$initialize(label = "Age", type = "Categorical")
      .setClass(private = private, key = "breaks", value = breaks, class = "Breaks")
    }
  ),
  private = list(
    breaks = NULL
  )
)

CategoricalDemographic <- R6::R6Class(
  classname = "CategoricalDemographic",
  inherit = Statistic,
  public = list(
    initialize = function(label, conceptColumn, conceptId) {
      super$initialize(label, type = "Categorical")
      .setString(private = private, key = "conceptColumn", value = conceptColumn)
      .setInteger(private = private, key = "conceptId", value = conceptId)
    },
    getConceptColumn = function() {
      rr <- private$conceptColumn
      return(rr)
    },
    getConceptId = function() {
      rr <- private$conceptId
      return(rr)
    }
  ),
  private = list(
    conceptColumn = NA_character_,
    conceptId = NA_integer_
  )
)

CategoricalPresence <- R6::R6Class(
  classname = "CategoricalPresence",
  inherit = Statistic,
  public = list(
    initialize = function(operator, occurrences) {
      super$initialize(label = "Presence", type = "Categorical")
      .setString(private = private, key = "operator", value = operator)
      .setNumber(private = private, key = "occurrences", value = occurrences)
    }
  ),
  private = list(
    operator = NA_character_,
    occurrences = NA
  )
)


TimeInterval <- R6::R6Class(
  "TimeInterval",
  public = list(
    initialize = function(lb, rb) {
      .setNumber(private = private, key = "lb", value = lb)
      .setNumber(private = private, key = "rb", value = rb)
      invisible(self)
    },
    getLb = function() {
      lb <- private$lb
      return(lb)
    },
    getRb = function() {
      rb <- private$rb
      return(rb)
    },
    getTimeLabel = function() {
      lbl <- glue::glue("{private$lb}d to {private$rb}d")
      return(lbl)
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

DemographicLineItem <- R6::R6Class(
  classname = "DemographicLineItem",
  inherit = LineItem,
  public = list(
    initialize = function(statistic = statistic) {
      super$initialize(
        sectionLabel = "Demographics",
        domainTable = "person",
        lineItemLabel = statistic$getStatLabel(),
        lineItemClass = "Demographic",
        statistic = statistic,
        timeInterval = NULL
      )
    }),
  private = list()
)


ConceptSetLineItem <- R6::R6Class(
  classname = "ConceptSetLineItem",
  inherit = LineItem,
  public = list(
    initialize = function(
      sectionLabel,
      domainTable,
      conceptSet,
      timeInterval,
      statistic,
      sourceConceptSet = NULL,
      typeConceptIds = c(),
      visitOccurrenceConceptIds = c()
    ) {
      super$initialize(
        sectionLabel = sectionLabel,
        domainTable = domainTable,
        lineItemClass = "ConceptSet",
        valueDescription = "codeset_id",
        statistic = statistic,
        lineItemLabel = conceptSet@Name,
        timeInterval = timeInterval
        )

      .setClass(private = private, key = "conceptSet", value = conceptSet, class = "ConceptSet")
      #.setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeInterval", nullable = TRUE)
      # TODO change this to enforce domain from choice list
      .setClass(private = private, key = "sourceConceptSet", value = sourceConceptSet, class = "ConceptSet", nullable = TRUE)
      .setNumber(private = private, key = "typeConceptIds", value = typeConceptIds, nullable = TRUE)
      .setNumber(private = private, key = "visitOccurrenceConceptIds", value = visitOccurrenceConceptIds, nullable = TRUE)

    }
  ),
  private = list(
    conceptSet = NULL,
    sourceConceptSet = NULL,
    typeConceptIds = c(),
    visitOccurrenceConceptIds = c()
  ),
  active = list()
)
