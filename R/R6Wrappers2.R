#Updates to the R6 Wrappers

createConceptSetLineItem <- function(sectionLabel = NA_character_,
                                     domain,
                                     conceptSet,
                                     timeInterval,
                                     statistic,
                                     sourceConceptSet = NULL,
                                     typeConceptIds = c(),
                                     visitOccurrenceConceptIds = c()) {

  if(is.na(sectionLabel)) {
    sectionLabel <- conceptSet@Name
  }

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


createConceptSetLineItemBatch <- function(
    sectionLabel,
    domain,
    conceptSets,
    timeIntervals,
    statistic,
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
      sectionLabel = sectionLabel,
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


createPresence <- function(operator = "at_least", occurrences = 1) {
  pres <- CategoricalPresence$new(operator = operator, occurrences = occurrences)
  return(pres)
}


createDemographicLineItem <- function(statistic) {
  dcli <- DemographicLineItem$new(
    statistic = statistic
  )
  statLabel <- class(statistic)[[1]]

  if (statLabel %in% c("CategoricalAge", "ContinuousAge")) {
    dcli$valueId <- -999
    dcli$valueDescription <- "year_of_birth"
  }

  if (statLabel == "CategoricalDemographic") {
    dcli$valueId <- statistic$getConceptId()
    dcli$valueDescription <- statistic$getConceptColumn()
  }

  return(dcli)
}

maleGender <- function() {
  maleConcept <- CategoricalDemographic$new(
    label = "Gender: Male",
    conceptColumn = "gender_concept_id",
    conceptId = 8507L
  )
  return(maleConcept)
}

femaleGender <- function() {
  femaleConcept <- CategoricalDemographic$new(
    label = "Gender: Female",
    conceptColumn = "gender_concept_id",
    conceptId = 8532L
  )
  return(femaleConcept)
}

lineItems <- function(...) {
  listOfLineItems <- list(...) |>
    purrr::list_flatten()
  # ensure that all elements are lineItems
  checkmate::assert_list(x = listOfLineItems, types = "LineItem", null.ok = FALSE, min.len = 1)

  # add in ordinals
  ii <- seq_along(listOfLineItems)
  for(i in ii) {
    listOfLineItems[[i]]$ordinalId <- ii[i]
  }
  return(listOfLineItems)
}
