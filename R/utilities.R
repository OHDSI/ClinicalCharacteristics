.targetCohortLabels <- function(tcList) {
  tcl <- tibble::tibble(
    targetCohortId = purrr::map_dbl(tcList, ~.x$getId()),
    targetCohortName = purrr::map_chr(tcList, ~.x$getName())
  )
  return(tcl)
}


.insertTableSql <- function(executionSettings, tableName, data) {

  #prep data for insert
  data <- data |>
    dplyr::rename_with(snakecase::to_snake_case) |>
    as.data.frame()

  sqlDataTypes <- sapply(data, getSqlDataTypes)
  sqlTableDefinition <- paste(.sql.qescape(names(data), TRUE), sqlDataTypes, collapse = ", ")
  sqlTableName <- .sql.qescape(tableName, TRUE, quote = "")
  sqlFieldNames <- paste(.sql.qescape(names(data), TRUE), collapse = ",")


  dataTxt <- toStrings(data, sqlDataTypes)
  valuesString <- paste("(", paste(apply(dataTxt, MARGIN = 1, FUN = paste, collapse = ","), collapse = "),\n("), ")")

  insertSql <- "DROP TABLE IF EXISTS @table;
  CREATE TABLE @table (@definition);
  INSERT INTO @table (@fields) \nVALUES @values;" |>
    SqlRender::render(table = sqlTableName,
                      definition = sqlTableDefinition,
                      fields = sqlFieldNames,
                      values = valuesString) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

  return(insertSql)

}

.instantiateCsOccurrenceTable <- function(executionSettings, buildOptions) {
  sql <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql", "createOccurrenceTable.sql")
  ) |>
    readr::read_file() |>
    SqlRender::render(
      concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable
    ) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )
  return(sql)
}


.opConverter <- function(op) {
  op <- switch(op,
               'at_least' = '>=',
               'at_most' = '<=',
               'exactly' = '=')
  return(op)
}

.findLineItemId <- function(lineItems, classType) {
  # get the class types
  lineClasses <- purrr::map_chr(lineItems, ~.x$lineItemClass)
  lineItemIds <- which(lineClasses == classType)
  return(lineItemIds)
}

# .getCsFromR6 <- function(lineItems) {
#
#   idsToPluck <- c(
#     .findLineItemId(lineItems = lineItems, classType = "ConceptSet"),
#     .findLineItemId(lineItems = lineItems, classType = "ConceptSetGroup")
#   )
#   filteredLineItems <- lineItems[idsToPluck]
#
#   csCapr <- purrr::map(
#     filteredLineItems,
#     ~.x$grabConceptSet()
#   ) |>
#     purrr::list_flatten()
#
#
#   # cs_id <- !duplicated(purrr::map_chr(csCapr, ~.x@id))
#   # cs_tbl2 <- csCapr[cs_id]
#
#   return(csCapr)
# }

.getCaprCs <- function(lineItems) {
  # get list ids for class types
  conceptSetLineItems <- .findLineItemId(lineItems = lineItems, classType = "ConceptSet")
  conceptSetGroupLineItems <- .findLineItemId(lineItems = lineItems, classType = "ConceptSetGroup")

  #make a subsetting vector of list ids
  idsToPluck <- c(conceptSetLineItems, conceptSetGroupLineItems)
  #filted line items to those with concepts
  filteredLineItems <- lineItems[idsToPluck]

  # get the capr concept sets for all the concepts
  caprCs <- purrr::map(
    filteredLineItems,
    ~.x$grabConceptSet()
  ) |>
    purrr::list_flatten()
  return(caprCs)
}

.caprToMetaTable <- function(caprCs) {
  # make a table identifying the codeset id for the query, unique to each cs
  tb <- tibble::tibble(
    id = purrr::map_chr(caprCs, ~.x@id),
    name = purrr::map_chr(caprCs, ~.x@Name)
  ) |>
    dplyr::mutate(
      csId = dplyr::dense_rank(id)
    ) |>
    tibble::rownames_to_column(var = "rowId")
  return(tb)
}


.setCsValueId <- function(lineItems) {
  # get list ids for class types
  conceptSetLineItems <- .findLineItemId(lineItems = lineItems, classType = "ConceptSet")
  conceptSetGroupLineItems <- .findLineItemId(lineItems = lineItems, classType = "ConceptSetGroup")

  # get the capr concept sets for all the concepts
  caprCs <- .getCaprCs(lineItems)

  # make a table identifying the codeset id for the query, unique to each cs
  tb <- .caprToMetaTable(caprCs)

  # deal with duplicate ord ids for concept set group line items
  if (length(conceptSetGroupLineItems) > 0) {
    # get length of each conceptSetGroup
    csgLiLength <- lineItems[conceptSetGroupLineItems] |>
      purrr::map_int(~length(.x$grabConceptSet()))
    # make full list of ord ids
    fullListOfIds <- c(
      conceptSetLineItems,
      rep(conceptSetGroupLineItems, each = csgLiLength) # rep each group by num of concepts in group
    )
  } else {
    fullListOfIds <- c(
      conceptSetLineItems
    )
  }


  # add the full list to tb to identify the ordinal of csId
  tb <- tb |>
    dplyr::mutate(
      ord = fullListOfIds
    )

  # loop throug table and get the codeset ids
  for (i in 1:nrow(tb)) {

    ordId <- tb$ord[i] # plock ord
    csId <- tb |> # get vector of csId corresponding to ord slot. if cs only 1 if csg more than 1
      dplyr::filter(
        ord == ordId
      ) |>
      dplyr::pull(csId)

    lineItems[[ordId]]$valueId <- csId
  }

  return(lineItems)

}


.setSourceValueId <- function(lineItems) {
  #make a subsetting vector of list ids
  idsToPluck <- .findLineItemId(lineItems = lineItems, classType = "SourceConceptSet")
  # only manipulate if there are sourceConcept Sets
  if (length(idsToPluck) > 0) {
    #filted line items to those with concepts
    filteredLineItems <- lineItems[idsToPluck]

    # get the source concept sets for all the concepts
    scs <- purrr::map(
      filteredLineItems,
      ~.x$grabSourceConceptSet()
    )
    # make table of ids and names for source concepts
    tb <- tibble::tibble(
      id = purrr::map_chr(scs, ~.x$sourceConceptId),
      name = purrr::map_chr(scs, ~.x$sourceConceptName)
    ) |>
      dplyr::mutate(
        scsId = dplyr::dense_rank(id)
      ) |>
      tibble::rownames_to_column(var = "rowId")

    # add the full list to tb to identify the ordinal of csId
    tb <- tb |>
      dplyr::mutate(
        ord = idsToPluck
      )

    # loop throug table and get the source concept ids
    for (i in 1:nrow(tb)) {

      ordId <- tb$ord[i] # plock ord
      scsId <- tb |> # get vector of scsId corresponding to ord slot.
        dplyr::filter(
          ord == ordId
        ) |>
        dplyr::pull(scsId)

      lineItems[[ordId]]$valueId <- scsId
    }
  }

  return(lineItems)

}

# function to get timeInterval and concept set / cohort combinations
.permuteTi <- function(lineItemObjects, timeIntervals) {

  # get number of items for each
  numObj <- length(lineItemObjects)
  numTis <- length(timeIntervals)

  # build out permutations
  objPerm <- rep(lineItemObjects, times = numTis)
  tiPerm <- rep(timeIntervals, each = numObj)

  permTiObj <- list(
    'objects' = objPerm,
    'timeIntervals' = tiPerm
  )
  return(permTiObj)

}



# function that translates the columns per domain
.domainTranslate <- function(domain) {
  # read domainTranslation file
  dt <- readr::read_csv(
    fs::path_package(package = "ClinicalCharacteristics", fs::path("csv", "domainTranslation.csv")),
    show_col_types = FALSE
  ) |>
    dplyr::filter(
      domain == !!domain # filter to domain of interest
    )

  return(dt)

}

.prepConceptSetOccurrenceQuerySql <- function(csTables, domain, type = c("standard", "source")) {

  domainGroup <- csTables |>
    dplyr::filter(
      domainTable == !!domain
    )

  # get the value ids for the domain of interest and create a character string
  # to glue into sql
  codeset_ids <- domainGroup |>
    dplyr::arrange(valueId) |>
    dplyr::pull(valueId) |>
    unique() |>
    glue::glue_collapse(sep = ", ")

  time_labels <- domainGroup |>
    dplyr::arrange(timeLabel) |>
    dplyr::pull(timeLabel) |>
    unique() |>
    glue::glue_collapse(sep = "', '")

  domainTranslation <- .domainTranslate(domain)

  if (type == "standard") {
    sql <- fs::path_package(
      package = "ClinicalCharacteristics",
      fs::path("sql", "conceptSetOccurrenceQuery.sql")
    )
  }

  if (type == "source") {
    sql <- fs::path_package(
      package = "ClinicalCharacteristics",
      fs::path("sql", "sourceConceptSetOccurrenceQuery.sql")
    )
  }
   sql <- sql |>
    readr::read_file() |>
    glue::glue()

  return(sql)

}

.truncDropTempTables <- function(tempTableName) {
  sql <- glue::glue("DROP TABLE IF EXISTS {tempTableName};")
  return(sql)
}

.pasteSourceConceptsIntoCodeset <- function(sourceCodesetId, sourceConceptIds) {
  sourceConceptIds <- sourceConceptIds |> glue::glue_collapse(sep = ", ")
  sourceCodesetSql <- "
  SELECT
  {sourceCodesetId} AS codeset_id,
  c.concept_id FROM (
    SELECT concept_id
    FROM @vocabulary_database_schema.CONCEPT
    WHERE concept_id IN ({sourceConceptIds})
  ) c
  " |>
    glue::glue()
  return(sourceCodesetSql)
}


.sourceConceptQuery <- function(lineItems, scsMeta, executionSettings, buildOptions) {

  #temporary change with class
  sourceCodesetTable <-  buildOptions$sourceCodesetTempTable

  # get source concept line items
  ordIds <- scsMeta$ordinalId
  filteredLineItems <- lineItems[ordIds]

  # get the source concept ids for all source concept line items
  sourceConceptIds <- purrr::map(
    filteredLineItems, # only the source concept line items
    ~.x$grabSourceConceptSet()$getSourceConceptTable()$conceptId
  )

  sourceConceptSql <- purrr::map2(
    scsMeta$valueId, # get the value id for source concept set,
    sourceConceptIds, # list of ids for source concepts from line items
    ~.pasteSourceConceptsIntoCodeset(
      sourceCodesetId = .x,
      sourceConceptIds = .y
    )
  ) |>
    purrr::list_c() |>
    glue::glue_collapse(sep = "\n\nUNION ALL\n\n")

  sourceConceptSqlFinal <- glue::glue(
    "CREATE TABLE @source_codeset_table AS
          {sourceConceptSql}
          ;") |>
    SqlRender::render(
      vocabulary_database_schema = executionSettings$cdmDatabaseSchema,
      source_codeset_table = sourceCodesetTable
    ) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

  return(sourceConceptSqlFinal)

}

## Patient level Sql ---------------------

.buildDemoPatientLevelSql <- function(tsm, executionSettings, buildOptions){

  demoLines <- tsm |>
    dplyr::filter(
      grepl("Demographic", lineItemClass)
    )


  sqlDemographicsPath <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql", "demographics")
  )

  if (nrow(demoLines) > 0) {

  demoSql <- vector('list', length = nrow(demoLines))
  for (i in 1:nrow(demoLines)) {

    if (demoLines$personLineTransformation[i] == "binary") {
      valueId <- demoLines$valueId[i]
      valueDescription <- demoLines$valueDescription[i]
      demoSql[[i]] <- readr::read_file(file = fs::path(sqlDemographicsPath, "demoConcept.sql")) |>
        glue::glue() |>
        glue::glue_collapse("\n\n")
    }

    if (demoLines$personLineTransformation[i] == "age") {
      demoSql[[i]] <- readr::read_file(file = fs::path(sqlDemographicsPath, "demoAge.sql")) |>
        glue::glue_collapse("\n\n")
    }

    if (demoLines$personLineTransformation[i] == "year") {
      demoSql[[i]] <- readr::read_file(file = fs::path(sqlDemographicsPath, "demoYear.sql")) |>
        glue::glue_collapse("\n\n")
    }

    if (demoLines$personLineTransformation[i] == "cohort_follow_up") {
      demoSql[[i]] <- readr::read_file(file = fs::path(sqlDemographicsPath, "demoFollowupTime.sql")) |>
        glue::glue_collapse("\n\n")
    }

    if (demoLines$personLineTransformation[i] == "payer_type") {
      demoSql[[i]] <- readr::read_file(file = fs::path(sqlDemographicsPath, "demoPayerType.sql")) |>
        glue::glue_collapse("\n\n")
    }

    if (demoLines$personLineTransformation[i] == "location") {
      demoSql[[i]] <- readr::read_file(file = fs::path(sqlDemographicsPath, "demoLocation.sql")) |>
        glue::glue_collapse("\n\n")
    }

    if (demoLines$personLineTransformation[i] == "race") {
      demoSql[[i]] <- readr::read_file(file = fs::path(sqlDemographicsPath, "demoRace.sql")) |>
        glue::glue_collapse("\n\n")
    }


  }

  demoSql2 <- do.call('c', demoSql) |>
    glue::glue_collapse("\n\n") |>
    SqlRender::render(
      patient_level_data = buildOptions$patientLevelDataTempTable,
      cdm_database_schema = executionSettings$cdmDatabaseSchema,
      target_table = buildOptions$targetCohortTempTable
    )
  } else {
    demoSql2 <- ""
  }


  return(demoSql2)
}



.buildOccurrencePatientLevelSql <- function(tsm, executionSettings, buildOptions) {

  statTypes <- tsm |>
    dplyr::select(personLineTransformation, lineItemClass) |>
    dplyr::distinct()

  # limit statTYpes to only concept set
  statType <- statTypes |>
    dplyr::filter(
      grepl("ConceptSet", lineItemClass)
    ) |>
    dplyr::pull(personLineTransformation) |>
    unique()

  sqlConceptSetPath <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql", "conceptSet")
  )

  # concept set anyCount
  if (any(statType == "anyCount")) { # this label will change
    anyCountSql <- fs::path(sqlConceptSetPath, "anyCount.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable
      )
  } else{
    anyCountSql <- ""
  }

  # concept set observedCount
  if (any(statType == "observedCount")) { # this label will change
    observedCountSql <- fs::path(sqlConceptSetPath, "observedCount.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable,
        cdm_database_schema = executionSettings$cdmDatabaseSchema,
        ts_meta_table = buildOptions$tsMetaTempTable
      )
  } else{
    observedCountSql <- ""
  }

  # concept set observedCount
  if (any(statType == "adherentCount")) { # this label will change
    adherentCountSql <- fs::path(sqlConceptSetPath, "adherentCount.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable,
        cdm_database_schema = executionSettings$cdmDatabaseSchema,
        ts_meta_table = buildOptions$tsMetaTempTable
      )
  } else{
    adherentCountSql <- ""
  }

  # concept set timeTo
  if (any(statType == "timeToFirst")) {
    timeToSql <- fs::path(sqlConceptSetPath, "timeToFirst.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable,
        ts_meta_table = buildOptions$tsMetaTempTable
      )
  } else{
    timeToFirstSql <- ""
  }

  # concept set observedTimeToFirst
  if (any(statType == "observedTimeToFirst")) {
    observedTimeToFirstSql <- fs::path(sqlConceptSetPath, "observedTimeToFirst.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        cohort_occurrence_table = buildOptions$cohortOccurrenceTempTable,
        ts_meta_table = buildOptions$tsMetaTempTable,
        cdm_database_schema = executionSettings$cdmDatabaseSchema
      )
  } else{
    observedTimeToFirstSql <- ""
  }

  sql <- c(anyCountSql, observedCountSql, adherentCountSql, timeToFirstSql, observedTimeToFirstSql) |>
    glue::glue_collapse(sep = "\n\n")

  return(sql)


}



.buildCohortPatientLevelSql <- function(tsm, executionSettings, buildOptions) {

  statTypes <- tsm |>
    dplyr::select(personLineTransformation, lineItemClass) |>
    dplyr::distinct()

  # limit statTYpes to only concept set
  statType <- statTypes |>
    dplyr::filter(
      grepl("Cohort", lineItemClass)
    ) |>
    dplyr::pull(personLineTransformation) |>
    unique()

  sqlConceptSetPath <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql", "cohort")
  )

  # concept set anyCount
  if (any(statType == "anyCount")) { # this label will change
    anyCountSql <- fs::path(sqlConceptSetPath, "anyCount.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        cohort_occurrence_table = buildOptions$cohortOccurrenceTempTable
      )
  } else{
    anyCountSql <- ""
  }

  # concept set observedCount
  if (any(statType == "observedCount")) { # this label will change
    observedCountSql <- fs::path(sqlConceptSetPath, "observedCount.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        cohort_occurrence_table = buildOptions$cohortOccurrenceTempTable,
        cdm_database_schema = executionSettings$cdmDatabaseSchema,
        ts_meta_table = buildOptions$tsMetaTempTable
      )
  } else{
    observedCountSql <- ""
  }

  # concept set observedCount
  if (any(statType == "adherentCount")) { # this label will change
    adherentCountSql <- fs::path(sqlConceptSetPath, "adherentCount.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        cohort_occurrence_table = buildOptions$cohortOccurrenceTempTable,
        cdm_database_schema = executionSettings$cdmDatabaseSchema,
        ts_meta_table = buildOptions$tsMetaTempTable
      )
  } else{
    adherentCountSql <- ""
  }

  # concept set timeTo
  if (any(statType == "timeToFirst")) {
    timeToFirstSql <- fs::path(sqlConceptSetPath, "timeToFirst.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        cohort_occurrence_table = buildOptions$cohortOccurrenceTempTable,
        ts_meta_table = buildOptions$tsMetaTempTable
      )
  } else{
    timeToFirstSql <- ""
  }

  # concept set observedTimeToFirst
  if (any(statType == "observedTimeToFirst")) {
    observedTimeToFirstSql <- fs::path(sqlConceptSetPath, "observedTimeToFirst.sql") |>
      readr::read_file() |>
      SqlRender::render(
        patient_level_data = buildOptions$patientLevelDataTempTable,
        cohort_occurrence_table = buildOptions$cohortOccurrenceTempTable,
        ts_meta_table = buildOptions$tsMetaTempTable,
        cdm_database_schema = executionSettings$cdmDatabaseSchema
      )
  } else{
    observedTimeToFirstSql <- ""
  }

  sql <- c(anyCountSql, observedCountSql, adherentCountSql, timeToFirstSql, observedTimeToFirstSql) |>
    glue::glue_collapse(sep = "\n\n")

  return(sql)

}


## aggregation sql ------------------

.tempPsDatTable <- function(executionSettings, buildOptions) {


  patTsSql <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql/aggregate/denom.sql")
  ) |>
    readr::read_file() |>
    SqlRender::render(
      target_cohort_table = buildOptions$targetCohortTempTable,
      time_window = buildOptions$timeWindowTempTable,
      cdm_database_schema = executionSettings$cdmDatabaseSchema,
      patient_data = buildOptions$patientLevelDataTempTable,
      pat_ts_tab = buildOptions$patientLevelTableShellTempTable,
      ts_meta = buildOptions$tsMetaTempTable
    ) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

  #  # Create temp table joining patient date with ts meta
  # patTsSql <- "
  #       DROP TABLE IF EXISTS @pat_ts_tab;
  #       CREATE TABLE @pat_ts_tab AS
  #       SELECT
  #         a.*, b.ordinal_id, b.section_label, b.line_item_label,
  #         b.value_description, b.statistic_type,
  #         b.aggregation_type, b.line_item_class
  #       FROM @patient_data a
  #       JOIN @ts_meta b
  #       ON a.value_id = b.value_id AND a.time_label = b.time_label;" |>
  #   SqlRender::render(
  #     patient_data = buildOptions$patientLevelDataTempTable,
  #     pat_ts_tab = buildOptions$patientLevelTableShellTempTable,
  #     ts_meta = buildOptions$tsMetaTempTable
  #   ) |>
  #   SqlRender::translate(
  #     targetDialect = executionSettings$getDbms(),
  #     tempEmulationSchema = executionSettings$tempEmulationSchema
  #   )
  return(patTsSql)
}

.initAggregationTables <- function(executionSettings, buildOptions) {

  initSummaryTableSql <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql/aggregate", "initSummaryTables.sql")
  ) |>
    readr::read_file() |>
    SqlRender::render(
      categorical_table = buildOptions$categoricalSummaryTempTable,
      continuous_table = buildOptions$continuousSummaryTempTable
    ) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

  return(initSummaryTableSql)

}


.getInterval <- function(ts, ordinalId) {

  intervalRateItem <- ts$getLineItems()[ordinalId]
  kk <- intervalRateItem[[1]]$getStatistic()$interval
  return(kk)
}

.prepBreaksTable <- function(ts, tsm, executionSettings) {

  breakLines <- tsm |>
    dplyr::filter(
      statisticType == "breaks"
    ) |>
    dplyr::pull(ordinalId)


  breakLineItems <- ts$getLineItems()[c(breakLines)]
  kk <- purrr::map(breakLineItems, ~.x$getStatistic()) |>
    purrr::map(~.x$getBreaksIfAny())

  sql <- purrr::map2(kk, breakLines,
                     ~.x$makeCaseWhenSql(ordinalId = .y)) |>
    glue::glue_collapse(sep = "\n\nUNION ALL\n\n")

  breaksSql <- glue::glue(
    "DROP TABLE IF EXISTS #pat_ts_break;
    CREATE TABLE #pat_ts_break AS
    {sql};"
  )

  return(breaksSql)
}


.prepScoreTable <- function(ts) {

  scoreLines <- ts$getTableShellMeta() |>
    dplyr::filter(
      statisticType == "scoreTransformation"
    )
  timeLabel <- scoreLines |> dplyr::distinct(timeLabel) |> dplyr::pull()
  patientLine <- scoreLines |> dplyr::distinct(personLineTransformation) |> dplyr::pull()
  sectionLabel <- scoreLines |> dplyr::distinct(sectionLabel) |> dplyr::pull()

  scoreOrd <- scoreLines |>
    dplyr::pull(ordinalId)

  # collect the weights
  scoreLineItems <- ts$getLineItems()[scoreOrd]
  ww <- purrr::map(scoreLineItems, ~.x$getStatistic()) |>
    purrr::map_dbl(~.x$getWeightsIfAny())

  # match the ordinals to the weights
  scoreTb <- tibble::tibble(
    scoreOrdId = scoreOrd,
    scoreWeights = ww
  )
  # create a case when statement to link the weights to the ord ids
  scoreCaseWhen <- scoreTb |>
    dplyr::mutate(
      expr_weights = glue::glue("WHEN a.ordinal_id = {scoreOrdId} THEN {scoreWeights}")
    ) |>
    dplyr::pull(expr_weights) |>
    glue::glue_collapse(sep = "\n\t")

  # Make the score temp table
  scoreSql <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql/aggregate", "scoreTemp.sql")
  ) |>
    readr::read_file() |>
    glue::glue()

  return(scoreSql)

}


.prepAggTables <- function(buildOptions) {
  aggregateSqlPaths <- fs::path_package(
    package = "ClinicalCharacteristics",
    fs::path("sql/aggregate")
  )

  # Step 1: Make a table with all the aggregation types
  aggSqlTb <- tibble::tibble(
    aggName = c("presence", "continuousDistribution", "breaks", "scoreTransformation", "monthly_intervalRate", "yearly_intervalRate"),
    aggType = c("categorical", "continuous", "categorical", "both", "continuous", "continuous"),
    aggSqlPath = fs::path(aggregateSqlPaths, aggName, ext = "sql")
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      aggSql = readr::read_file(aggSqlPath) # read in file
    )
  # Step 2: identify the correct temp tables
  aggSqlTb <- aggSqlTb |>
    dplyr::left_join(
      tibble::tibble(
        aggType = c("categorical", "continuous", "both"),
        aggTable = c(buildOptions$categoricalSummaryTempTable, buildOptions$continuousSummaryTempTable, "both")
      ),
      by = c("aggType")
    )

  return(aggSqlTb)
}

.aggregateSql <- function(ts, executionSettings, buildOptions) {

  tsm <- ts$getTableShellMeta()

  # Step 1: Prep Agg Sqls
  aggSqlTb <- .prepAggTables(buildOptions)

  # Step 3: Find the distinct stat types from the table shell
  statTypes <- tsm |>
    dplyr::select(statisticType) |>
    dplyr::distinct()

  # Step 4: Reduce the agg Sql to only the stat types

  aggSqlTb2 <- aggSqlTb |>
    dplyr::inner_join(statTypes, by = c("aggName" = "statisticType"))

  # Step 5:
  aggSqlBind <- vector('list', length = nrow(aggSqlTb2))
  for (i in 1:nrow(aggSqlTb2)) {

  # if categorical render with table
    if (aggSqlTb2$aggType[i] == "categorical") {

      if (aggSqlTb2$aggName[i] == "breaks") {
        breaksSql <- .prepBreaksTable(ts, tsm, executionSettings)
        aggSql <- c(breaksSql, aggSqlTb2$aggSql[i]) |>
          glue::glue_collapse("\n\n")
      } else {
        aggSql <- aggSqlTb2$aggSql[i]
      }

      aggSqlBind[[i]] <- SqlRender::render(
        sql = aggSql,
        categorical_table = aggSqlTb2$aggTable[i],
        pat_ts_tab  = buildOptions$patientLevelTableShellTempTable
      ) |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )
    }
    # if continuous render with table
    if (aggSqlTb2$aggType[i] == "continuous") {


      if (any(aggSqlTb2$aggName[i] %in% c("monthly_intervalRate", "yearly_intervalRate"))) {


        aggSqlBind[[i]] <- SqlRender::render(
          sql = aggSqlTb2$aggSql[i],
          continuous_table = aggSqlTb2$aggTable[i],
          pat_ts_tab  = buildOptions$patientLevelTableShellTempTable,
          target_cohort_table = buildOptions$targetCohortTempTable,
          ts_meta = buildOptions$tsMetaTempTable,
          time_window = buildOptions$timeWindowTempTable,
          continuous_event_type = buildOptions$continuousEventType
        ) |>
          SqlRender::translate(
            targetDialect = executionSettings$getDbms(),
            tempEmulationSchema = executionSettings$tempEmulationSchema
          )

      } else {
        aggSqlBind[[i]] <- SqlRender::render(
          sql = aggSqlTb2$aggSql[i],
          continuous_table = aggSqlTb2$aggTable[i],
          pat_ts_tab  = buildOptions$patientLevelTableShellTempTable,
          ts_meta = buildOptions$tsMetaTempTable,
          target_cohort_table = buildOptions$targetCohortTempTable,
          continuous_event_type = buildOptions$continuousEventType
        ) |>
          SqlRender::translate(
            targetDialect = executionSettings$getDbms(),
            tempEmulationSchema = executionSettings$tempEmulationSchema
          )
      }
    }

    # if both render to both tables
    if (aggSqlTb2$aggType[i] == "both") {

      scoreSql <- .prepScoreTable(ts)
      aggSql <- c(scoreSql, aggSqlTb2$aggSql[i]) |>
        glue::glue_collapse("\n\n")

      aggSqlBind[[i]] <- SqlRender::render(
        sql = aggSql,
        categorical_table = buildOptions$categoricalSummaryTempTable,
        continuous_table = buildOptions$continuousSummaryTempTable,
        pat_ts_tab  = buildOptions$patientLevelTableShellTempTable,
        target_cohort_table = buildOptions$targetCohortTempTable
      ) |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )
    }

  }
  aggSqlBind <- do.call('c', aggSqlBind) |>
    glue::glue_collapse("\n\n")

  return(aggSqlBind)
}


# Get Results ------------------

# .checkAnyScoreTransformation <- function(tsm) {
#
#
# }

.getCategoricalResults <- function(tsm, tc, executionSettings, buildOptions) {

  sql <- "SELECT * FROM @categorical_table;" |>
    SqlRender::render(
      categorical_table = buildOptions$categoricalSummaryTempTable
    ) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

  jj <- DatabaseConnector::querySql(
    connection = executionSettings$getConnection(),
    sql = sql,
    snakeCaseToCamelCase = TRUE
  )

  categoricalResults <- jj |>
    dplyr::inner_join(
      tsm |> dplyr::select(ordinalId, personLineTransformation, sectionLabel, statisticType),
      by = c("ordinalId", "patientLine" = "personLineTransformation", "statisticType")
    ) |>
    dplyr::left_join(
      tc,
      by = c("targetCohortId")
    ) |>
    dplyr::select(
      targetCohortId, targetCohortName, ordinalId, sectionLabel, lineItemLabel,
      patientLine, statisticType, timeLabel, subjectCount, totSubjects, pct
    ) |>
    dplyr::arrange(
      targetCohortId, ordinalId, lineItemLabel
    ) |>
    dplyr::distinct()

  #categoricalResults <- .completeMissingData(categoricalResults, tsm)


  return(categoricalResults)

}


.getContinuousResults <- function(tsm, tc, executionSettings, buildOptions) {

  sql <- "SELECT * FROM @continuous_table;" |>
    SqlRender::render(
      continuous_table = buildOptions$continuousSummaryTempTable
    ) |>
    SqlRender::translate(
      targetDialect = executionSettings$getDbms(),
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

  jj <- DatabaseConnector::querySql(
    connection = executionSettings$getConnection(),
    sql = sql,
    snakeCaseToCamelCase = TRUE
  )

  if (any(grepl("scoreTransformation", tsm$statisticType))) {
    tsm_score <- tsm |>
      dplyr::filter(statisticType == "scoreTransformation") |>
      dplyr::select(sectionLabel, personLineTransformation, statisticType) |>
      dplyr::distinct() |>
      dplyr::mutate(
        ordinalId = 9999, .before = 1
      )
    tsm_select <- dplyr::bind_rows(
      tsm |> dplyr::select(ordinalId, sectionLabel, personLineTransformation, statisticType),
      tsm_score
    )
  } else {
    tsm_select <- tsm |> dplyr::select(ordinalId, sectionLabel, personLineTransformation, statisticType)
  }


  continuousResults <- jj |>
    dplyr::inner_join(
      tsm_select,
      by = c("ordinalId", "patientLine" = "personLineTransformation", "statisticType")
    ) |>
    dplyr::left_join(
      tc,
      by = c("targetCohortId")
    ) |>
    dplyr::select(
      targetCohortId, targetCohortName, ordinalId, sectionLabel, lineItemLabel,
      patientLine, statisticType, timeLabel, subjectCount:maximumValue
    ) |>
    dplyr::arrange(
      targetCohortId, ordinalId, lineItemLabel
    ) |>
    dplyr::distinct()

  return(continuousResults)

}


.completeMissingData <- function(categoricalResults, tsm) {

  missingLines <- tsm[!(tsm$ordinalId %in% categoricalResults$ordinalId), ]

  if (nrow(missingLines) > 0) {
    cohortCounts <- categoricalResults |>
      dplyr::filter(sectionLabel != "Demographics") |>
      dplyr::select(
        targetCohortId, targetCohortName, patientLine, timeLabel, totSubjects
      ) |>
      dplyr::distinct()


    missingLinesDf <- missingLines |>
      dplyr::filter(
        !(statisticType %in% c("continuousDistribution"))
      ) |>
      dplyr::cross_join(cohortCounts) |>
      dplyr::select(
        targetCohortId, targetCohortName, ordinalId,
        sectionLabel, lineItemLabel, patientLine = personLineTransformation,
        statisticType, timeLabel = timeLabel.x, totSubjects
      ) |>
      dplyr::mutate(
        subjectCount = 0,
        pct = 0
      )

    finalCatResults <- categoricalResults |>
      dplyr::bind_rows(missingLinesDf) |>
      dplyr::arrange(
        targetCohortId, ordinalId, lineItemLabel
      )
  } else {
    finalCatResults <- categoricalResults
  }


  return(finalCatResults)

}
