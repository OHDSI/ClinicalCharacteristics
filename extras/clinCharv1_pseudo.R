es <- list(
  'cdmDatabaseSchema' = "my_cdm",
  'workDatabaseSchema' = "my_scratch",
  'database' = "optum_mc",
  'connectionDetails' = connectionDetails
)


tc <- idTargetCohorts(
  ids = c(304, 305),
  names = c("IPF-all", "IPF-adult")
)


tc <- list(
  'ipf' = list(
    id = 1,
    name =
  )
)

# old
addTargetCohorts(
  ids = c(304, 305),
  names = c("IPF-all", "IPF-adult"),
  cohortTable = "cohorts"
)

makeTimeWindow(a = c(-365, 0, 0), b = c(-1, 365, 730))





# helpers to infuse addTargetCohort
loadTargetCohortsFromCsv(file)
loadTargetCohortsFromDf(df)


clinChar1 <- createTableShell(
  name = "Table 1 - Describe IPF Patients",
  targetCohorts = NULL,
  executionSettings = NULL,
  sections = NULL
) |>
  addExecutionSetting(es) |>
  loadTargetCohortsFromCsv(file) |>
  addSection(
    name = "Demographics",
    lineItems = list(
      ageChar(
        ordinal = 1,
        name = "age", # default
        breaks = addBreaks(
          defineBreak(min = 0, max = 17, ordinal = 1, name = NULL),
          defineBreak(min = 18, max = Inf, ordinal = 2)
        ),
        genderChar(
          ordinal = 2,
          name = "gender" # default
        ),
        raceChar(
          ordinal = 3,
          name = "race"
        )
      )
    )
  ) |>
  addSection(
    name = "Comorbidities",
    timeWindows = addTimeWindows(
      defineWindow(start = -365, end = -1, displayName = NULL),
      defineWindow(start = 0, end = 365, displayName = NULL),
    ),
    charType = "presence", #"timeTo, count, cost
    lineItems = addLineItems(
      conceptSetPresence(
        ordinal = 1,
        name = "T2D",
        conceptSet = Capr::cs(Capr::descendants(1234), name = "T2D"),
        conceptType = NULL,
        visitConceptId = NULL,
        sourceConceptId = NULL
      ),
      conceptSetPresence(
        ordinal = 2,
        name = "CKD",
        conceptSet = importConceptSetJson("~/conceptSet.json"), #Capr::cs(Capr::descendants(1234), name = "T2D")
        typeConceptId = c(12345),
        visitConceptId = c(9201),
        sourceConceptId = c(746)
      ),
      cohortPresence(
        ordinal = 3,
        name = "HF",
        id = 14,
        table = "covariates",
        schema = "scratch_lavallem"
      )
    )
  ) |>
  addSection(
    name = "Comorbidities",
    timeWindows = addTimeWindows(
      defineWindow(start = -365, end = -1, displayName = NULL),
      defineWindow(start = 0, end = 365, displayName = NULL),
    ),
    charType = "timeTo", #presence, count, cost
    lineItems = addLineItems(
      cohortTimeTo(
        ordinal = 1,
        name = "HF",
        id = 14,
        table = "covariates",
        schema = "scratch_lavallem",
        limit = "first"
      ),
      conceptSetTimeTo(
        ordinal = 2,
        name = "CKD",
        conceptSet = Capr::cs(Capr::descendants(281), name = "CKD"),
        conceptType = c(12345),
        limit = "first"
      )
    )
  )



Capr::cs(Capr::descendants(567), name = "CKD")

  addTargetCohort(
    loadCohortsFromCsv(file)
    # targetCohort(id = 1, "IPF"),
    # targetCohort(id = 2, "PPF")
  ) |>
  addSection(
    name = "Demographics",
    lineItems = list(
      ageChar(
        ordinal = 1,
        name = "age", # default
        breaks = addBreaks(
          min = c(0, 18, 65),
          max = c(17, 64, 130),
          ordinal = c(1, 2, 3)
        ),
        genderChar(
          ordinal = 2,
          name = "gender" # default
        ),
        raceChar(
          ordinal = 3,
          name = "race"
        )
      )
    )
  ) |>
  addSection(
    name = "Comorbidities",
    lineItems = list(
      conceptSetPresence(
        ordinal = 1,
        name = "T2D",
        conceptSet = Capr::cs(Capr::descendants(1234), name = "T2D"),
        timeWindow = makeTimeWindow(a = c(-365, 0, 0), b = c(-1, 365, 730)),
        conceptType = c(12345),
        limit = "first"
      ),
      conceptSetPresence(
        ordinal = 2,
        name = "CKD",
        conceptSet = Capr::cs(Capr::descendants(567), name = "CKD"),
        timeWindow = makeTimeWindow(a = c(-365, 0, 0), b = c(-1, 365, 730)),
        conceptType = c(12345),
        limit = "first"
      ),
      cohortPresence(
        ordinal = 3,
        name = "HF",
        cohort = idCohort(name = "HF", id = 14, cohortDatabase(table = "covariates", schema = "scratch_lavallem")),
        timeWindow = makeTimeWindow(a = c(-365, 0, 0), b = c(-1, 365, 730)),
        limit = "first"
      ),
      conceptSetTimeTo(
        ordinal = 4,
        name = "IPF Symptoms",
        conceptSet = Capr::cs(Capr::descendants(281), name = "CKD"),
        timeWindow = makeTimeWindow(a = c(-365), b = c(-1)),
        conceptType = c(12345),
        limit = "first"
      )
    )
  )


describeTableShell(clinChar1) # template a description of the analysis done in a table
reviewTableShellQuery(clinChar1) # inspect master sql query
tb <- generateTableShell(clinChar1) #replace runClinChar
peakResults(tb) # review results from table shell run
saveTableResults(tb, saveName = "table1", savePath = here::here()) # save table results to file

