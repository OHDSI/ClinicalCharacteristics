targetCohortIds <- c(519)
shellTitle <- "Table 3a"



tableShell <- TableShell$new(shellTitle = "Table 3a",
                             targetCohortIds = targetCohortIds)

createTableShell <- function(shellTitle,
                             sections = list()) {
  return(TableShell$new(shellTitle = shellTitle))
}

createSection <- function(sectionTitle,
                          sectionOrdinal = NA,
                          targetCohortIds = NULL) {

  return(Section$new(sectionTitle = sectionTitle,
                     sectionOrdinal = sectionOrdinal))
}

addSections <- function(tableShell,
                        sections) {


  tableShell$sections <- c(tableShell$sections, sections)
  return(tableShell)
}

createLineItem <- function(itemDefinition,
                           itemOrdinal,
                           itemCategory,
                           itemLabel,
                           timeWindows) {
  return(LineItem$new(itemDefinition = itemDefinition,
               itemOrdinal = itemOrdinal,
               itemCategory = itemCategory,
               itemLabel = itemLabel,
               timeWindows = timeWindows))
}

addLineItems <- function(section,
                         lineItems) {
  section$lineItems <- c(section$lineItems, lineItems)
  return(section)
}

sections <- list(
  createSection(sectionTitle = "Infections", sectionOrdinal = 1)
)

ts <- createTableShell(shellTitle = shellTitle, targetCohortIds = targetCohortIds) |>
  addSections(sections = sections)


createItemDefinition <- function(domain,
                                 limit,
                                 definition) {
  return(ItemDefinition$new(domain = domain,
                            limit = limit,
                            definition))
                            # conceptSetDefinition = conceptSetDefinition,
                            # cohortDefinition = cohortDefinition,
                            # demographicDefinition = demographicDefinition))
}

createCohortDefinition <- function() {

}

lineItem <- createLineItem(itemDefinition = itemDefinition, itemOrdinal = 1,
                           itemCategory = "Serious", itemLabel = "Opportunistic")

section <- Section$new(tableShell = tableShell, sectionTitle = "Infections", sectionOrdinal = NA)

tableShell$setSections(sections = list(section))

tableShell$sections






ts <- TableShell$new(shellTitle = "Table 3a",
                             targetCohortIds = targetCohortIds)

sec <- Section$new(sectionTitle = "blah", sectionOrdinal = 1)



addTargetCohorts(
  targetCohort(id = 1, "IPF"),
  targetCohort(id = 2, "PPF")
)
