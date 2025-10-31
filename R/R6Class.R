# TableShell -----

#' @title Table Shell
#' @description
#' An R6 class to define a TableShell object
#'
#' @export
TableShell <- R6::R6Class("TableShell",
  public = list(
    #' @param title the title of the table shell
    #' @param targetCohorts a list of CohortInfo class objects that describe the index cohorts
    #' @param lineItems a list of line item class objects
    initialize = function(title,
                          targetCohorts,
                          lineItems) {
      .setString(private = private, key = "title", value = title)
      .setListofClasses(private = private, key = "targetCohorts", value = targetCohorts, classes = c("CohortInfo"))
      #.setClass(private = private, key = "executionSettings", value = executionSettings, class = "ExecutionSettings")
      .setListofClasses(private = private, key = "lineItems", value = lineItems, classes = c("LineItem"))
    },
    #' @description get the title of the table shell
    getTitle = function() {
      tsName <- private$title
      return(tsName)
    },
    #' @description get the meta information for the table shell build
    getTableShellMeta = function() {
      tsLi <- self$getLineItems()
      tsMeta <- purrr::map_dfr(
        tsLi, ~.x$getLineItemMeta()
      )
      return(tsMeta)
    },
    #' @description get the target cohorts from the table shell
    getTargetCohorts = function() {
      tsTargetCohorts <- private$targetCohorts
      return(tsTargetCohorts)
    },
    #' @description get the lineItems from the table shell
    getLineItems = function() {
      tsLineItems <- private$lineItems
      return(tsLineItems)
    },
    #' @description print the job details of the table shell
    printJobDetails = function() {

      tcs <- self$getTargetCohorts()
      cohortPrintInfo <- purrr::map_chr(tcs, ~.x$cohortDetails()) |>
        glue::glue_collapse("\n")

      # get line item info
      tsm <- self$getTableShellMeta()
      tsmInfo <- tsm |>
        dplyr::select(ordinalId, sectionLabel, lineItemLabel, timeLabel, statisticType, personLineTransformation) |>
        dplyr::distinct() |>
        glue::glue_data_col(
          "{ordinalId}) {green {sectionLabel}}: {yellow {lineItemLabel}} ({magenta {timeLabel}}) || Stat Type: {blue {statisticType}} || Patient Line: {blue {personLineTransformation}}"
        )|>
        glue::glue_collapse("\n")


      cli::cat_line(
        glue::glue("Target Cohort Details:\n{cohortPrintInfo}")
      )

      cli::cat_line(
        glue::glue("Line Item tasks:\n{tsmInfo}")
      )

      invisible(tsmInfo)

    },

    #' @description function creates the table shell sql needed for the execution
    #' @param executionSettings an executionSettings class obj
    #' @param buildOptions a buildOptions class obj
    buildTableShellSql = function(executionSettings, buildOptions) {

      # ensure R6 object used
      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)
      checkmate::assert_class(buildOptions, classes = "BuildOptions", null.ok = FALSE)

      cli::cat_bullet(
        glue::glue_col("{yellow Preparing Table Shell Sql}"),
        bullet = "pointer",
        bullet_col = "yellow"
      )
      self$printJobDetails()

      # collect all the sql
      fullSql <- c(

        # Step 1: Insert ts meta
        .insertTableSql(
          executionSettings = executionSettings,
          tableName = buildOptions$tsMetaTempTable,
          data = self$getTableShellMeta()
        ),

        # Step 2: Insert time windows
        private$.insertTimeWindows(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 3: Add occurrence table
        .instantiateCsOccurrenceTable(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # step 4: Make target Cohort table
        private$.makeTargetCohortTable(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # step 5: create targe cohort table
        private$.buildCodesetQueries(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 6a: create concept set query
        private$.buildConceptSetOccurrenceQuery(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 6b: create source concept set query
        private$.buildSourceConceptOccurrenceQuery(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 6c: create cohort occ query
        private$.buildCohortOccurrenceQuery(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 7: transform to patient line data
        private$.transformToPatientLineData(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 8: add denom to patient data
        private$.prepDenominator(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        ),

        # Step 9: Aggregate results
        private$.aggregateResults(
          executionSettings = executionSettings,
          buildOptions = buildOptions
        )
      ) |>
        glue::glue_collapse(sep = "\n")


      return(fullSql)

    },
    #' @description retrieves results from dbms and formats for review
    #' @param executionSettings an executionSettings class obj
    #' @param buildOptions a buildOptions class obj
    outputResults = function(executionSettings, buildOptions) {

      tsm <- self$getTableShellMeta()

      tc <- self$getTargetCohorts() |>
        .targetCohortLabels()

      # categorical results
      cat_res <- .getCategoricalResults(tsm, tc, executionSettings, buildOptions) |>
        tibble::as_tibble()

      # categorical results
      cts_res <- .getContinuousResults(tsm, tc, executionSettings, buildOptions) |>
        tibble::as_tibble()


      res <- list(
        'categorical' = cat_res,
        'continuous' = cts_res
      )

      return(res)
    },

    #' @description drop all temp tables from the tableShell build
    #' @param executionSettings an executionSettings class obj
    #' @param buildOptions a buildOptions class obj
    dropTempTables = function(executionSettings, buildOptions) {

      # get temp table slot names
      tempTableSlots <- names(buildOptions)[grepl("TempTable",names(buildOptions))]

      # get table names
      tempTableNames <- purrr::map_chr(
        tempTableSlots,
        ~buildOptions[[.x]]
      )

      tempTables <- tibble::tibble(
        tempTableSlots = tempTableSlots,
        tempTableNames = tempTableNames
      ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          drop = grepl("\\#", tempTableNames) # check if temp
        )

      dropTempTableSql <- vector('list', length = nrow(tempTables))
      for (i in 1:nrow(tempTables)) {
        if (tempTables$drop[i]) {
          dropTempTableSql[[i]] <- .truncDropTempTables(
            tempTableName = tempTables$tempTableNames[i]
          )
        } else {
          dropTempTableSql[[i]] <- ""
        }
      }
      dropTempTableSql <- do.call("c", dropTempTableSql) |>
        glue::glue_collapse("\n") |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )

      DatabaseConnector::executeSql(
        connection = executionSettings$getConnection(),
        sql = dropTempTableSql
      )

      return(dropTempTableSql)
    }

  ),

  private = list(
    title = NULL,
    targetCohorts = NULL,
    lineItems = NULL,


    ### private methods ---------------

    .insertTimeWindows = function(executionSettings, buildOptions) {
      # ensure that executionSettings R6 object used
      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)

      # get concept set line items
      twLineItems <- self$getTableShellMeta() |>
        dplyr::filter(!grepl("Static at Index", timeLabel))

      if (nrow(twLineItems) > 0) {
        # make the time windows table
        time_tbl <- tibble::tibble(
          time_label = twLineItems$timeLabel
        ) |>
          dplyr::distinct() |>
          tidyr::separate_wider_delim(
            time_label,
            delim = " to ",
            names = c("time_a", "time_b"),
            cols_remove = FALSE
          ) |>
          dplyr::mutate(
            time_a = as.integer(gsub("d", "", time_a)),
            time_b = as.integer(gsub("d", "", time_b))
          ) |>
          dplyr::select(
            time_label, time_a, time_b
          )
      } else{

        time_tbl <- tibble::tibble(
          time_label = NA_character_,
          time_a = NA_integer_,
          time_b = NA_integer_
        )

      }

      # Insert time windows sql
      time_sql <- .insertTableSql(
        executionSettings = executionSettings,
        tableName = buildOptions$timeWindowTempTable,
        data = time_tbl
      )

      return(time_sql)
    },

    # function to get target cohort sql
    .makeTargetCohortTable = function(executionSettings, buildOptions) {

      sqlFile <- "targetCohort.sql"
      cohortIds <- purrr::map_int(
        private$targetCohorts,
        ~.x$getId()
      ) |>
        glue::glue_collapse(", ")

      # get sql from package
      sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
        readr::read_file() |>
        glue::glue()

      renderedSql <- sql |>
        SqlRender::render(
          target_table = buildOptions$targetCohortTempTable,
          work_database_schema = executionSettings$workDatabaseSchema,
          cohort_table = executionSettings$cohortTable
        ) |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )

      return(renderedSql)
    },



    # function to create sql for codset query
    .buildCodesetQueries = function(executionSettings, buildOptions) {

      #temporary change with class
      codesetTable <-  buildOptions$codesetTempTable

      # get concept set line items
      li <- self$getLineItems()

      # pluck the capr concept sets
      caprCs <- .getCaprCs(li)


      if (length(caprCs) >= 1) {
        #turn into query
        cs_query <- .bindCodesetQueries(caprCs, codesetTable = codesetTable) |>
          SqlRender::render(
            vocabulary_database_schema = executionSettings$cdmDatabaseSchema
          ) |>
          SqlRender::translate(
            targetDialect = executionSettings$getDbms(),
            tempEmulationSchema = executionSettings$tempEmulationSchema
          )
      } else{
        cs_query <- ""
      }

      return(cs_query)

    },

    # function to extract concept level information
    .buildConceptSetOccurrenceQuery = function(executionSettings, buildOptions) {

      # Step 1: Get the concept set meta
      csMeta <- self$getTableShellMeta() |>
        dplyr::filter(lineItemClass %in% c("ConceptSet", "ConceptSetGroup"))

      # only run if CSD in ts
      if (nrow(csMeta) > 0) {

        # Step 2: Prep the concept set extraction
        csTables <- csMeta |>
          dplyr::select(valueId, valueDescription, timeLabel, domainTable)

        # Step 3: get the domains to join
        domainTablesInUse <- unique(csTables$domainTable)
        # step 4: make the concept set occurrence sql
        conceptSetOccurrenceSqlGrp <- purrr::map(
          domainTablesInUse,
          ~.prepConceptSetOccurrenceQuerySql(
            csTables = csTables,
            domain = .x,
            type = "standard"
          )
        ) |>
          glue::glue_collapse(sep = "\n\nUNION ALL\n\n")

        conceptSetOccurrenceSql <- glue::glue(
          "INSERT INTO @concept_set_occurrence_table
          {conceptSetOccurrenceSqlGrp}
          ;
          "
        )
        conceptSetOccurrenceSql <- conceptSetOccurrenceSql |>
          SqlRender::render(
            target_cohort_table = buildOptions$targetCohortTempTable,
            concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable,
            time_window_table = buildOptions$timeWindowTempTable,
            codeset_table = buildOptions$codesetTempTable,
            cdm_database_schema = executionSettings$cdmDatabaseSchema
          ) |>
          SqlRender::translate(
            targetDialect = executionSettings$getDbms(),
            tempEmulationSchema = executionSettings$tempEmulationSchema
          )

      } else {
        conceptSetOccurrenceSql <- ""
      }

      return(conceptSetOccurrenceSql)

    },

    .buildSourceConceptOccurrenceQuery = function(executionSettings, buildOptions) {

      # Step 1: Get the concept set meta
      scsMeta <- self$getTableShellMeta() |>
        dplyr::filter(lineItemClass == "SourceConceptSet")

      # only run if CSD in ts
      if (nrow(scsMeta) > 0) {

        li <- self$getLineItems()
        sourceCodeSetQuery <- .sourceConceptQuery(
          lineItems = li,
          scsMeta = scsMeta,
          executionSettings = executionSettings,
          buildOptions = buildOptions
        )

        # Step 2: Prep the concept set extraction
        scsTables <- scsMeta |>
          dplyr::select(valueId, valueDescription, timeLabel, domainTable)

        # Step 3: get the domains to join
        domainTablesInUse <- unique(scsTables$domainTable)
        # step 4: make the source concept set occurrence sql
        sourceSonceptSetOccurrenceSqlGrp <- purrr::map(
          domainTablesInUse,
          ~.prepConceptSetOccurrenceQuerySql(
            csTables = scsTables,
            domain = .x,
            type = "source"
          )
        ) |>
          glue::glue_collapse(sep = "\n\nUNION ALL\n\n")

        sourceConceptSetOccurrenceSql <- glue::glue(
          "{sourceCodeSetQuery}
          INSERT INTO @concept_set_occurrence_table
          {sourceSonceptSetOccurrenceSqlGrp}
          ;
          ")
        sourceConceptSetOccurrenceSql <- sourceConceptSetOccurrenceSql |>
          SqlRender::render(
            target_cohort_table = buildOptions$targetCohortTempTable,
            concept_set_occurrence_table = buildOptions$conceptSetOccurrenceTempTable,
            time_window_table = buildOptions$timeWindowTempTable,
            source_codeset_table = buildOptions$sourceCodesetTempTable,
            cdm_database_schema = executionSettings$cdmDatabaseSchema
          ) |>
          SqlRender::translate(
            targetDialect = executionSettings$getDbms(),
            tempEmulationSchema = executionSettings$tempEmulationSchema
          )

      } else {
        sourceConceptSetOccurrenceSql <- ""
      }

      return(sourceConceptSetOccurrenceSql)

    },

    .buildCohortOccurrenceQuery = function(executionSettings, buildOptions) {
      # Step 1: Get the cohort meta
      chMeta <- self$getTableShellMeta() |>
        dplyr::filter(grepl("Cohort", lineItemClass))

      # only run if Cohorts are in ts
      if (nrow(chMeta) > 0) {

        # Step 2: Prep the cohort extraction
        cohort_ids <- chMeta$valueId |> unique() |> glue::glue_collapse(", ")
        time_labels <- chMeta$timeLabel |> unique() |> glue::glue_collapse("', '")
        domain <- chMeta$domainTable |> unique()

        # Step 3: make the cohort occurrence sql

        cohortOccurrenceSql <- fs::path_package(
          package = "ClinicalCharacteristics",
          fs::path("sql/cohortOccurrenceQuery.sql")
        ) |>
          readr::read_file() |>
          glue::glue()

        # render sql
        cohortOccurrenceSql <- cohortOccurrenceSql |>
          SqlRender::render(
            target_cohort_table = buildOptions$targetCohortTempTable,
            cohort_occurrence_table = buildOptions$cohortOccurrenceTempTable,
            time_window_table = buildOptions$timeWindowTempTable,
            work_database_schema = executionSettings$workDatabaseSchema,
            cohort_analysis_type = buildOptions$cohortAnalysisType
          ) |>
          SqlRender::translate(
            targetDialect = executionSettings$getDbms(),
            tempEmulationSchema = executionSettings$tempEmulationSchema
          )

      } else {
        cohortOccurrenceSql <- ""
      }

      return(cohortOccurrenceSql)

    },

    .transformToPatientLineData = function(executionSettings, buildOptions) {

      # Step 1: make patient level data table
      ptDatTbSql <- fs::path_package(
        "ClinicalCharacteristics",
        fs::path("sql", "patientLevelData.sql")
      ) |>
        readr::read_file() |>
        SqlRender::render(
          patient_level_data = buildOptions$patientLevelDataTempTable
        )

      # Step 2: run patient level queries
      tsm <- self$getTableShellMeta()
      # step 2a demographics

      demoPatientLevelSql <- .buildDemoPatientLevelSql(tsm, executionSettings, buildOptions)

      # step 2b concept set pat level
      csPatientLevelSql <- .buildOccurrencePatientLevelSql(tsm, executionSettings, buildOptions)

      # step 2c cohort pat level
      chPatientLevelSql <- .buildCohortPatientLevelSql(tsm, executionSettings, buildOptions)

      # full sql for sql
      ptFullSql <- c(ptDatTbSql, demoPatientLevelSql, csPatientLevelSql, chPatientLevelSql) |>
        glue::glue_collapse(sep = "\n\n") |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )

      return(ptFullSql)
    },

    .prepDenominator = function(executionSettings, buildOptions){
      # Create temp table joining patient date with ts meta
      patTsSql <- .tempPsDatTable(executionSettings, buildOptions) |>
        glue::glue_collapse(sep = "\n\n") |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )
      return(patTsSql)
    },

    .aggregateResults = function(executionSettings, buildOptions) {

      # tsm <- self$getTableShellMeta()
      ts <- self

      # make temp continuous + categorical table
      initSummaryTableSql <- .initAggregationTables(executionSettings, buildOptions)

      # make all the aggregate sql queries
      aggregateSqlQuery <- .aggregateSql(ts, executionSettings, buildOptions)

      allSql <- c(initSummaryTableSql, aggregateSqlQuery) |>
        glue::glue_collapse(sep = "\n\n")

      return(allSql)
    }
  )
)


# BuildOptions ----

#' @title BuildOptions
#' @description
#' An R6 class to define build options for the tableShell
#'
#' @export
BuildOptions <- R6::R6Class(
  classname = "BuildOptions",
  public = list(
    #' @param codesetTempTable the name of the codeset table used in execution. Defaults as a temp table #codeset
    #' @param sourceCodesetTempTable the name of the source codeset table used in execution
    #' @param timeWindowTempTable the name of the time Window table used in execution. Defaults as a temp table #time_windows
    #' @param targetCohortTempTable the name of the target cohort table used in execution. Defaults as a temp table #target_cohorts
    #' @param tsMetaTempTable the name of the table shell meta table used in execution. Defaults as a temp table #ts_meta
    #' @param conceptSetOccurrenceTempTable the name of the concept set occurrence table used in execution. Defaults as a temp table #concept_set_occ
    #' @param cohortOccurrenceTempTable the name of the cohort occurrence  table used in execution. Defaults as a temp table #cohort_occ
    #' @param patientLevelDataTempTable the name of the patient level data table used in execution. Note this does not contain info of the table shell. Defaults as a temp table #patient_data
    #' @param patientLevelTableShellTempTable the name of the patient level data table with additional meta info used in execution. Defaults as a temp table #pat_ts_tab
    #' @param categoricalSummaryTempTable the name of the categorical summary table used in execution. Defaults as a temp table #categorical_table
    #' @param continuousSummaryTempTable the name of the continuous summary table used in execution. Defaults as a temp table #continuous_table
    #' @param cohortAnalysisType a toggle specifying if in a cohort Char whether to use the cohort era ('era') or just the start date ('startDate')
    #' @param continuousEventType a toggle specifying if the continuous aggregation should be on the full population or persons with events
    initialize = function(codesetTempTable = NULL,
                          sourceCodesetTempTable = NULL,
                          timeWindowTempTable = NULL,
                          targetCohortTempTable = NULL,
                          tsMetaTempTable = NULL,
                          conceptSetOccurrenceTempTable = NULL,
                          cohortOccurrenceTempTable = NULL,
                          patientLevelDataTempTable = NULL,
                          patientLevelTableShellTempTable = NULL,
                          categoricalSummaryTempTable = NULL,
                          continuousSummaryTempTable = NULL,
                          cohortAnalysisType = NULL,
                          continuousEventType = NULL
                          ) {
      .setString(private = private, key = ".codesetTempTable", value = codesetTempTable)
      .setString(private = private, key = ".sourceCodesetTempTable", value = sourceCodesetTempTable)
      .setString(private = private, key = ".timeWindowTempTable", value = timeWindowTempTable)
      .setString(private = private, key = ".tsMetaTempTable", value = tsMetaTempTable)
      .setString(private = private, key = ".targetCohortTempTable", value = targetCohortTempTable)
      .setString(private = private, key = ".conceptSetOccurrenceTempTable", value = conceptSetOccurrenceTempTable)
      .setString(private = private, key = ".cohortOccurrenceTempTable", value = cohortOccurrenceTempTable)
      .setString(private = private, key = ".patientLevelDataTempTable", value = patientLevelDataTempTable)
      .setString(private = private, key = ".patientLevelTableShellTempTable", value = patientLevelTableShellTempTable)
      .setString(private = private, key = ".categoricalSummaryTempTable", value = categoricalSummaryTempTable)
      .setString(private = private, key = ".continuousSummaryTempTable", value = continuousSummaryTempTable)
      .setString(private = private, key = ".cohortAnalysisType", value = cohortAnalysisType)
      .setString(private = private, key = ".continuousEventType", value = continuousEventType)
    }
  ),
  private = list(
    .codesetTempTable = NULL,
    .sourceCodesetTempTable = NULL,
    .timeWindowTempTable = NULL,
    .targetCohortTempTable = NULL,
    .tsMetaTempTable = NULL,
    .conceptSetOccurrenceTempTable = NULL,
    .cohortOccurrenceTempTable = NULL,
    .patientLevelDataTempTable = NULL,
    .patientLevelTableShellTempTable = NULL,
    .categoricalSummaryTempTable = NULL,
    .continuousSummaryTempTable = NULL,
    .cohortAnalysisType = NULL,
    .continuousEventType = NULL
  ),

  active = list(

    #' @field codesetTempTable table name for codeset table
    codesetTempTable = function(value) {
      .setActiveString(private = private, key = ".codesetTempTable", value = value)
    },

    #' @field sourceCodesetTempTable table name for source codeset table
    sourceCodesetTempTable = function(value) {
      .setActiveString(private = private, key = ".sourceCodesetTempTable", value = value)
    },

    #' @field timeWindowTempTable table name for time windows
    timeWindowTempTable = function(value) {
      .setActiveString(private = private, key = ".timeWindowTempTable", value = value)
    },

    #' @field targetCohortTempTable table name for target cohorts
    targetCohortTempTable = function(value) {
      .setActiveString(private = private, key = ".targetCohortTempTable", value = value)
    },

    #' @field tsMetaTempTable table name for table shell meta
    tsMetaTempTable = function(value) {
      .setActiveString(private = private, key = ".tsMetaTempTable", value = value)
    },

    #' @field conceptSetOccurrenceTempTable table name for concept set occurrence table
    conceptSetOccurrenceTempTable = function(value) {
      .setActiveString(private = private, key = ".conceptSetOccurrenceTempTable", value = value)
    },

    #' @field cohortOccurrenceTempTable table name for cohort occurrence table
    cohortOccurrenceTempTable = function(value) {
      .setActiveString(private = private, key = ".cohortOccurrenceTempTable", value = value)
    },

    #' @field patientLevelDataTempTable table name for patient level data
    patientLevelDataTempTable = function(value) {
      .setActiveString(private = private, key = ".patientLevelDataTempTable", value = value)
    },

    #' @field patientLevelTableShellTempTable table name for patient level data table merged with ts meta
    patientLevelTableShellTempTable = function(value) {
      .setActiveString(private = private, key = ".patientLevelTableShellTempTable", value = value)
    },

    #' @field categoricalSummaryTempTable table name for categorical summary table
    categoricalSummaryTempTable = function(value) {
      .setActiveString(private = private, key = ".categoricalSummaryTempTable", value = value)
    },

    #' @field continuousSummaryTempTable table name for continuous summary table
    continuousSummaryTempTable = function(value) {
      .setActiveString(private = private, key = ".continuousSummaryTempTable", value = value)
    },

    #' @field cohortAnalysisType toggle to choose if using cohort era or start date
    cohortAnalysisType = function(value) {
      .setActiveString(private = private, key = ".cohortAnalysisType", value = value)
    },

    #' @field continuousEventType toggle to choose if you are summarizing all pop or just events
    continuousEventType = function(value) {
      .setActiveString(private = private, key = ".continuousEventType", value = value)
    }
  )
)

# ExecutionSettings ----

#' @title ExecutionSettings
#' @description
#' An R6 class to define an ExecutionSettings object
#'
#' @export
ExecutionSettings <- R6::R6Class(
  classname = "ExecutionSettings",
  public = list(
    #' @param connectionDetails a connectionDetails object
    #' @param connection a connection to a dbms
    #' @param cdmDatabaseSchema The schema of the OMOP CDM database
    #' @param workDatabaseSchema The schema to which results will be written
    #' @param tempEmulationSchema Some database platforms like Oracle and Snowflake do not truly support temp tables. To emulate temp tables, provide a schema with write privileges where temp tables can be created.
    #' @param cohortTable The name of the table where the cohort(s) are stored
    #' @param cdmSourceName A human-readable name for the OMOP CDM source
    initialize = function(connectionDetails = NULL,
                          connection = NULL,
                          cdmDatabaseSchema = NULL,
                          workDatabaseSchema = NULL,
                          tempEmulationSchema = NULL,
                          cohortTable = NULL,
                          cdmSourceName = NULL) {
      stopifnot(is.null(connectionDetails) || is.null(connection))
      .setClass(private = private, key = "connectionDetails", value = connectionDetails,
                class = "ConnectionDetails", nullable = TRUE)
      .setClass(private = private, key = ".connection", value = connection,
                class = "DatabaseConnectorJdbcConnection", nullable = TRUE)
      .setString(private = private, key = ".cdmDatabaseSchema", value = cdmDatabaseSchema)
      .setString(private = private, key = ".workDatabaseSchema", value = workDatabaseSchema)
      .setString(private = private, key = ".tempEmulationSchema", value = tempEmulationSchema)
      .setString(private = private, key = ".cohortTable", value = cohortTable)
      .setString(private = private, key = ".cdmSourceName", value = cdmSourceName)
    },
    #' @description extract the dbms dialect
    getDbms = function() {
      conObj <- private$.connection
      if (!is.null(conObj)) {
        dbms <- conObj@dbms
      } else {
        dbms <- private$connectionDetails$dbms
      }
      return(dbms)
    },
    #' @description connect to dbms
    connect = function() {

      # check if private$connection is NULL
      conObj <- private$.connection
      if (is.null(conObj)) {
        private$.connection <- DatabaseConnector::connect(private$connectionDetails)
      } else{
        cli::cat_bullet(
          "Connection object already open",
          bullet = "info",
          bullet_col = "blue"
        )
      }
    },

    #' @description disconnect from dbms
    disconnect = function() {

      # check if private$connection is NULL
      conObj <- private$.connection
      if (class(conObj) == "DatabaseConnectorJdbcConnection") {
        # disconnect connection
        DatabaseConnector::disconnect(private$.connection)
        private$.connection <- NULL
      }

      cli::cat_bullet(
        "Connection object has been disconected",
        bullet = "info",
        bullet_col = "blue"
      )
      invisible(conObj)
    },

    #TODO make this more rigorous
    # add warning if no connection available
    #' @description retrieve the connection object
    getConnection = function() {
      conObj <- private$.connection
      return(conObj)
    }

  ),

  private = list(
    connectionDetails = NULL,
    .connection = NULL,
    .cdmDatabaseSchema = NULL,
    .workDatabaseSchema = NULL,
    .tempEmulationSchema = NULL,
    .cohortTable = NULL,
    .cdmSourceName = NULL
  ),

  active = list(
    #' @field cdmDatabaseSchema the schema containing the OMOP CDM
    cdmDatabaseSchema = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        cds <- private$.cdmDatabaseSchema
        return(cds)
      }
      # replace the cdmDatabaseSchema
      .setString(private = private, key = ".cdmDatabaseSchema", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('cdmDatabaseSchema')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },

    #' @field workDatabaseSchema the schema containing the cohort table
    workDatabaseSchema = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        cds <- private$.workDatabaseSchema
        return(cds)
      }
      # replace the workDatabaseSchema
      .setString(private = private, key = ".workDatabaseSchema", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('workDatabaseSchema')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },

    #' @field tempEmulationSchema the schema needed for temp tables
    tempEmulationSchema = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        tes <- private$.tempEmulationSchema
        return(tes)
      }
      # replace the tempEmulationSchema
      .setString(private = private, key = ".tempEmulationSchema", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('tempEmulationSchema')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    #' @field cohortTable the table containing the cohorts
    cohortTable = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        tct <- private$.cohortTable
        return(tct)
      }
      # replace the cohortTable
      .setString(private = private, key = ".cohortTable", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('cohortTable')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },
    #' @field cdmSourceName the name of the source data of the cdm
    cdmSourceName = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        csn <- private$.cdmSourceName
        return(csn)
      }
      # replace the cdmSourceName
      .setString(private = private, key = ".cdmSourceName", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('cdmSourceName')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    }

  )
)


# Cohort Info -----

#' @title CohortInfoe
#' @description
#' An R6 class to define a Cohort Info object.
#' CohortInfo objects do not maintain any execution settings, just the id and name
#'
#' @export
CohortInfo <- R6::R6Class("CohortInfo",
  public = list(
    #' @param id the cohort definition id
    #' @param name the name of the cohort definition
    initialize = function(id, name) {
      .setNumber(private = private, key = "id", value = id)
      .setString(private = private, key = "name", value = name)
    },
    #' @description get the cohort id
    getId = function() {
      cId <- private$id
      return(cId)
    },
    #' @description get the cohort name
    getName = function() {
      cName <- private$name
      return(cName)
    },
    #' @description print the cohort details
    cohortDetails = function(){
      id <- self$getId()
      name <- self$getName()

      info <- glue::glue_col(
        "\t- Cohort Id: {green {id}}; Cohort Name: {green {name}}"
      )

      return(info)

    }
  ),
  private = list(
    id = NULL,
    name = NULL
  )
)


# Statistic Class ---------------------

## Statistic Super-------------

#' @title
#' An R6 class to define a Statistic object
#'
#' @description
#' A Statistic is a type of metric to be used for characterization.
#' Specific types of statistics are defined in derived classes
#'
#' @export
Statistic <- R6::R6Class(
  classname = "Statistic",
  public = list(
    #' @param statType the type of statistic
    #' @param personLine the means of converting occurrences to a single event per patient
    #' @param aggType the way the metric is reported either categorical or continuous
    initialize = function(statType, personLine, aggType) {
      .setString(private = private , key = "statisticType", value = statType)
      .setString(private = private , key = "personLineTransformation", value = personLine)
      .setString(private = private , key = "aggregationType", value = aggType)

    },
    #' @description retrieve the statistic type
    getStatisticType = function() {
      statType <- private$statisticType
      return(statType)
    },
    #' @description retrieve the aggregation type
    getAggregationType = function() {
      aggType <- private$aggregationType
      return(aggType)
    },
    #' @description retrieve the person line transformation
    getPersonLineTransformation = function() {
      plt <- private$personLineTransformation
      return(plt)
    },
    #' @description retrieve the breaks object from the statistic object
    getBreaksIfAny = function() {
      if (self$getStatisticType() == "breaks") {
        br <- private$breaks
      } else {
        br <- NULL
      }
      return(br)
    },
    #' @description retrieve the weights object from the statistic object
    getWeightsIfAny = function() {
      if (self$getStatisticType() == "scoreTransformation") {
        ww <- self$weight
      } else {
        ww <- NULL
      }
      return(ww)
    }
  ),
  private = list(
    statisticType = NA_character_,
    personLineTransformation = NA_character_,
    aggregationType = NA_character_
  )
)

## Demographic Stats----------------------


### Demographic Concept -----------------
#' @title
#' Demographic Concept Statistic
#'
#' @description
#' A Demographic Statistic that considers concepts in person table
#'
#' @export
DemographicConcept <- R6::R6Class(
  classname = "DemographicConcept",
  inherit = Statistic,
  public = list(
    #' @param demoCategory the category name of the demographic
    #' @param demoLine the line item name of the demographic concept
    #' @param conceptColumn the name of column in the person table to extract demographic concept
    #' @param conceptId the concept to search for in the concept column
    initialize = function(demoCategory, demoLine, conceptColumn, conceptId) {
      super$initialize(
        personLine = "binary",
        statType = "presence",
        aggType = "categorical")
      .setString(private = private, key = "demoCategory", value = demoCategory)
      .setString(private = private, key = "demoLine", value = demoLine)
      .setString(private = private, key = "conceptColumn", value = conceptColumn)
      .setNumber(private = private, key = "conceptId", value = conceptId)
    },
    #' @description retrieve the concept column
    getConceptColumn = function() {
      rr <- private$conceptColumn
      return(rr)
    },
    #' @description create a label for the demographic concept
    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}: {private$demoLine}")
      return(rr)
    },
    #' @description retrieve the concept id
    getConceptId = function() {
      rr <- private$conceptId
      return(rr)
    }
  ),
  private = list(
    demoCategory = NA_character_,
    demoLine = NA_character_,
    conceptColumn = NA_character_,
    conceptId = NA_integer_
  )
)

### Demographic Age ---------------------
#' @title
#' Demographic Age Statistic
#'
#' @description
#' A Demographic Statistic that calculates age from the person table
#'
#' @export
DemographicAge <- R6::R6Class(
  classname = "DemographicAge",
  inherit = Statistic,
  public = list(
    #' @param statType the type of statistic
    #' @param aggType the way the metric is reported either categorical or continuous
    #' @param demoCategory the name of the demographic category
    #' @param breaks a breaks strategy object to categorize results
    initialize = function(statType, aggType, demoCategory, breaks = NULL) {
      super$initialize(
        personLine = "age",
        statType = statType,
        aggType = aggType)
      .setString(private = private, key = "demoCategory", value = "Age")
      .setClass(private = private, key = "breaks", value = breaks,
                class = "BreaksStrategy", nullable = TRUE)
    },
    #' @description retrieve the demographic label
    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    },
    #' @description update the breaks labels within the statistics class
    #' @param newLabels a character string of new labels for the breaks
    modifyBreaksLabels = function(newLabels) {
      br <- private$breaks
      br$labels <- newLabels
    }

  ),
  private = list(
    demoCategory = NA_character_,
    breaks = NULL
  )
)

### Demographic Index Year ---------------------
#' @title
#' Demographic Index Year Statistic
#'
#' @description
#' A Demographic Statistic that retrieves the index year for each patient
#'
#' @export
DemographicIndexYear <- R6::R6Class(
  classname = "DemographicIndexYear",
  inherit = Statistic,
  public = list(
    #' @param breaks a breaks strategy object to categorize results
    initialize = function(breaks) {
      super$initialize(
        personLine = "year",
        statType = "breaks",
        aggType = "categorical")
      .setString(private = private, key = "demoCategory", value = "Year")
      .setClass(private = private, key = "breaks", value = breaks,
                class = "BreaksStrategy", nullable = FALSE)
    },
    #' @description retrieve the demographic label
    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    },
    #' @description update the breaks labels within the statistics class
    #' @param newLabels a character string of new labels for the breaks
    modifyBreaksLabels = function(newLabels) {
      br <- private$breaks
      br$labels <- newLabels
    }

  ),
  private = list(
    demoCategory = NA_character_,
    breaks = NULL
  )
)


### Demographic Cohort Follow up ---------------------
#' @title
#' Demographic Cohort Time Statistic
#'
#' @description
#' A Demographic Statistic that calculates the time (in days) in the target cohort
#'
#' @export
DemographicCohortTime <- R6::R6Class(
  classname = "DemographicCohortTime",
  inherit = Statistic,
  public = list(
    #' @description initialize cohort time stat
    initialize = function() {
      super$initialize(
        personLine = "cohort_follow_up",
        statType = "continuousDistribution",
        aggType = "continuous")
      .setString(private = private, key = "demoCategory", value = "Cohort Follow up")
    },
    #' @description retrieve the demographic label
    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    }

  ),
  private = list(
    demoCategory = NA_character_
  )
)


### Demographic Location ---------------------
#' @title
#' Demographic Location Statistic
#'
#' @description
#' A Demographic Statistic that retrieves and categorizes the location of the persons in the target cohort
#'
#' @export
DemographicLocation <- R6::R6Class(
  classname = "DemographicLocation",
  inherit = Statistic,
  public = list(
    #' @param breaks a breaks strategy object to categorize results
    initialize = function(breaks) {
      super$initialize(
        personLine = "location",
        statType = "breaks",
        aggType = "categorical")
      .setString(private = private, key = "demoCategory", value = "Location")
      .setClass(private = private, key = "breaks", value = breaks,
                class = "BreaksStrategy", nullable = FALSE)
    },
    #' @description retrieve the demographic label
    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    },
    #' @description update the breaks labels within the statistics class
    #' @param newLabels a character string of new labels for the breaks
    modifyBreaksLabels = function(newLabels) {
      br <- private$breaks
      br$labels <- newLabels
    }

  ),
  private = list(
    demoCategory = NA_character_,
    breaks = NULL
  )
)


### Demographic Payer Type ---------------------
#' @title
#' Demographic Payer Statistic
#'
#' @description
#' A Demographic Statistic that retrieves and categorizes the payer type from the payer plan period table
#'
#' @export
DemographicPayerType <- R6::R6Class(
  classname = "DemographicPayerType",
  inherit = Statistic,
  public = list(
    #' @param breaks a breaks strategy object to categorize results
    initialize = function(breaks) {
      super$initialize(
        personLine = "payer_type",
        statType = "breaks",
        aggType = "categorical")
      .setString(private = private, key = "demoCategory", value = "Payer Type")
      .setClass(private = private, key = "breaks", value = breaks,
                class = "BreaksStrategy", nullable = FALSE)
    },
    #' @description retrieve the demographic label
    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    },
    #' @description update the breaks labels within the statistics class
    #' @param newLabels a character string of new labels for the breaks
    modifyBreaksLabels = function(newLabels) {
      br <- private$breaks
      br$labels <- newLabels
    }

  ),
  private = list(
    demoCategory = NA_character_,
    breaks = NULL
  )
)

### Demographic Race ---------------------
#' @title
#' Demographic Race Statistic
#'
#' @description
#' A Demographic Statistic that retrieves and categorizes the patient race from the person table
#'
#' @export
DemographicRace <- R6::R6Class(
  classname = "DemographicRace",
  inherit = Statistic,
  public = list(
    #' @param breaks a breaks strategy object to categorize results
    initialize = function(breaks) {
      super$initialize(
        personLine = "race",
        statType = "breaks",
        aggType = "categorical")
      .setString(private = private, key = "demoCategory", value = "Race")
      .setClass(private = private, key = "breaks", value = breaks,
                class = "BreaksStrategy", nullable = FALSE)
    },
    #' @description retrieve the demographic label
    getDemoLabel = function() {
      rr <- glue::glue("{private$demoCategory}")
      return(rr)
    },
    #' @description update the breaks labels within the statistics class
    #' @param newLabels a character string of new labels for the breaks
    modifyBreaksLabels = function(newLabels) {
      br <- private$breaks
      br$labels <- newLabels
    }

  ),
  private = list(
    demoCategory = NA_character_,
    breaks = NULL
  )
)


## CS, CSG, Cohort Stats -----------------------------

### Presence -----------------------
#' @title
#' Presence Statistic
#'
#' @description
#' A statistic that determines whether at least one clinical event was present during the specified time interval. It
#' is summarized as a categorical value.
#'
#' @export
Presence <- R6::R6Class(
  classname = "Presence",
  inherit = Statistic,
  public = list(
    #' @param personLine the means of converting occurrences to a single event per patient.
    #' For presence this could be any, observed or adherent
    initialize = function(personLine) {
      super$initialize(
        personLine = personLine,
        statType = "presence",
        aggType = "categorical"
      )
    }
  ),
  private = list()
)

### Breaks ------------------------
#' @title
#' Breaks Statistic
#'
#' @description
#' A statistic that converts a continuous value to a categorical value by grouping
#' the number of events into discrete buckets.
#'
#' @export
Breaks <- R6::R6Class(
  classname = "Breaks",
  inherit = Statistic,
  public = list(
    #' @param personLine the means of converting occurrences to a single event per patient.
    #' For presence this could be any, observed or adherent
    #' @param breaks a breaks strategy object to categorize results
    initialize = function(personLine, breaks) {
      super$initialize(
        personLine = personLine,
        statType = "breaks",
        aggType = "categorical"
      )
      .setClass(private = private, key = "breaks", value = breaks, class = "BreaksStrategy")
    }
  ),
  private = list(
    breaks = NULL
  )
)

### Distribution ------------------------
#' @title
#' Continuous Distribution Statistic
#'
#' @description
#' A statistic that summarizes the number of occurrences as continuous value using mean, standard deviation and order statistics
#'
#' @export
ContinuousDistribution <- R6::R6Class(
  classname = "ContinuousDistribution",
  inherit = Statistic,
  public = list(
    #' @param personLine the means of converting occurrences to a single event per patient.
    #' For presence this could be any, observed or adherent
    initialize = function(personLine) {
      super$initialize(
        personLine = personLine,
        statType = "continuousDistribution",
        aggType = "continuous"
      )
    }
  ),
  private = list()
)

### Score ------------------------
#' @title
#' Score Statistic
#'
#' @description
#' A statistic that converts a categorical value to a continuous value by modifying the occurrence of
#' an event by a weight and summing across patients.
#'
#' @export
Score <- R6::R6Class(
  classname = "Score",
  inherit = Statistic,
  public = list(
    #' @param personLine the means of converting occurrences to a single event per patient.
    #' For a score currently only enabled for any occurrence
    #' @param weight a numeric value to modify the value of an occurrence
    initialize = function(personLine, weight) {
      super$initialize(
        personLine = personLine,
        statType = "scoreTransformation",
        aggType = "continuous"
      )
      .setNumber(private = private, key = ".weight", value = weight)
    }
  ),
  private = list(
    .weight = NULL
  ),
  active =list(
    #' @field weight a numeric value to modify the value of an occurrence
    weight = function(weight) {
      .setActiveNumber(private = private, key = ".weight", value = weight)
    }
  )
)

### Interval Rate --------------------
#' @title
#' Interval Rate Statistic
#'
#' @description
#' A statistic that calculates the rate of occurrence by taking the number of events per person
#' in the desired interval and dividing by the observed time during the interval. An interval rate
#' can either be monthly or yearly.
#'
#' @export
IntervalRate <- R6::R6Class(
  classname = "IntervalRate",
  inherit = Statistic,
  public = list(
    #' @param interval the type of interval to use for the rate. can be either monthly or yearly.
    initialize = function(interval) {
      super$initialize (
        personLine = "observedCount",
        statType = glue::glue("{interval}_intervalRate"),
        aggType = "continuous"
      )
    }

  ),

  private = list()
)

# LineItem Classes -----

## Line Item Super----------

#' @title LineItem
#' @description
#' An R6 class to define a LineItem object.
#' A LineItem is a single, explicitly defined characterization to appear in a Section.
#' Derived classes exist off of LineItems
#'
#' @export
LineItem <- R6::R6Class(
  classname = "LineItem",
  public = list(
    #' @param sectionLabel a label for the table shell section
    #' @param lineItemLabel a label for the line item
    #' @param domainTable the domain table in the cdm
    #' @param lineItemClass the type of line item (ie Demographic, ConceptSet, SourceConceptSet, ConceptSetGroup, Cohort)
    #' @param valueId the id for the line item; either a codeset id, a concept id or a -999 to indicate no true id
    #' @param valueDescription the describer for the value id
    #' @param statistic a Statistic Class object used to determine what type of analytic should be done for the line item
    #' @param timeInterval a time interval class object to determine the time frame to consider the analytic
    initialize = function(
      sectionLabel,
      lineItemLabel = NA_character_,
      domainTable,
      lineItemClass,
      valueId = NA_integer_,
      valueDescription  = NA_integer_,
      statistic,
      timeInterval = NULL
    ) {
      .setString(private = private, key = ".sectionLabel", value = sectionLabel)
      .setString(private = private, key = ".lineItemLabel", value = lineItemLabel, naOk = TRUE)
      .setCharacter(private = private, key = ".domainTable", value = domainTable)
      .setString(private = private, key = ".lineItemClass", value = lineItemClass)
      .setNumber(private = private, key = ".valueId", value = valueId)
      .setString(private = private, key = ".valueDescription", value = valueDescription, naOk = TRUE)
      .setClass(private = private, key = "statistic", value = statistic, class = "Statistic")
      .setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeIntervalClass", nullable = TRUE)
    },
    #' @description retrieve the line item meta information
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
        lineItemClass = private$.lineItemClass,
        lineItemLabel = private$.lineItemLabel,
        valueId = private$.valueId,
        valueDescription = private$.valueDescription,
        timeLabel = timeLabel,
        personLineTransformation = private$statistic$getPersonLineTransformation(),
        statisticType = private$statistic$getStatisticType(),
        aggregationType = private$statistic$getAggregationType(),
        domainTable = private$.domainTable
      )

      return(tb)
    },
    #' @description retrieve the statistic class object
    getStatistic = function() {
      st <- private$statistic
      return(st)
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
    #' @field ordinalId the order identifier of the line item in the table shell
    ordinalId = function(ordinalId) {
      .setActiveNumber(private = private, key = ".ordinalId", value = ordinalId)
    },
    #' @field sectionLabel a label for the table shell section
    sectionLabel = function(sectionLabel) {
      .setActiveString(private = private, key = ".sectionLabel", value = sectionLabel)
    },
    #' @field lineItemLabel a label for the line item
    lineItemLabel = function(lineItemLabel) {
      .setActiveString(private = private, key = ".lineItemLabel", value = lineItemLabel)
    },
    #' @field valueId the id for the line item; either a codeset id, a concept id or a -999 to indicate no true id
    valueId = function(valueId) {
      .setActiveNumber(private = private, key = ".valueId", value = valueId)
    },
    #' @field valueDescription the describer for the value id
    valueDescription = function(valueDescription) {
      .setActiveString(private = private, key = ".valueDescription", value = valueDescription)
    },
    #' @field domainTable the domain table in the cdm
    domainTable = function(domainTable) {
      .setActiveString(private = private, key = ".domainTable", value = domainTable)
    },
    #' @field lineItemClass the type of line item (ie Demographic, ConceptSet, SourceConceptSet, ConceptSetGroup, Cohort)
    lineItemClass = function(lineItemClass) {
      .setActiveString(private = private, key = ".lineItemClass", value = lineItemClass)
    }
  )
)


## ConceptSetLineItem ----

#' @title ConceptSetLineItem
#' @description
#' An R6 class to define a ConceptSetLineItem
#'
#' @export
ConceptSetLineItem <- R6::R6Class(
  classname = "ConceptSetLineItem",
  inherit = LineItem,
  public = list(
    #' @param sectionLabel a label for the table shell section
    #' @param domainTable the domain table in the cdm
    #' @param conceptSet a concept set class from Capr
    #' @param timeInterval a time interval class object to determine the time frame to consider the analytic
    #' @param statistic a Statistic Class object used to determine what type of analytic should be done for the line item
    initialize = function(
      sectionLabel,
      domainTable,
      conceptSet,
      timeInterval,
      statistic
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
      #.setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeIntervalClass", nullable = TRUE)
      # TODO change this to enforce domain from choice list
      # .setClass(private = private, key = "sourceConceptSet", value = sourceConceptSet, class = "ConceptSet", nullable = TRUE)
      # .setNumber(private = private, key = "typeConceptIds", value = typeConceptIds, nullable = TRUE)
      # .setNumber(private = private, key = "visitOccurrenceConceptIds", value = visitOccurrenceConceptIds, nullable = TRUE)

    },

     #' grabConceptSet
     #' @description helper to pull concept Capr class items
     grabConceptSet = function() {
        cs <- private$conceptSet
        return(cs)
     }
  ),
  private = list(
    conceptSet = NULL
  )
)

SourceConcepSet <- R6::R6Class(
  classname = "SourceConceptSet",
  public = list(

    initialize = function(
      sourceConceptId,
      sourceConceptName,
      sourceConceptSet
    ) {
      .setString(private = private, key = ".sourceConceptId", value = sourceConceptId)
      .setString(private = private, key = ".sourceConceptName", value = sourceConceptName)
      .setDataFrame(private = private, key = "sourceConceptSet",
                    colN = 4,
                    value = sourceConceptSet)
    },

    getSourceConceptTable = function() {
      tt <- private$sourceConceptSet
      return(tt)
    }

  ),
  private = list(
    .sourceConceptId = NA_character_,
    .sourceConceptName = NA_character_,
    sourceConceptSet = NULL
  ),
  active = list(
    sourceConceptName = function(sourceConceptName) {
      .setActiveString(private = private, key = ".sourceConceptName", value = sourceConceptName)
    },
    sourceConceptId = function(sourceConceptId) {
      .setActiveString(private = private, key = ".sourceConceptId", value = sourceConceptId)
    }
  )
)

## SourceConceptSetLineItem ----

#' @title SourceConceptSetLineItem
#' @description
#' An R6 class to define a SourceConceptSetLineItem
#'
#' @export
SourceConceptSetLineItem <- R6::R6Class(
  classname = "SourceConceptSetLineItem",
  inherit = LineItem,
  public = list(
    #' @param sectionLabel a label for the table shell section
    #' @param domainTable the domain table in the cdm
    #' @param sourceConceptSet a source concept Set
    #' @param timeInterval a time interval class object to determine the time frame to consider the analytic
    #' @param statistic a Statistic Class object used to determine what type of analytic should be done for the line item
    initialize = function(
    sectionLabel,
    domainTable,
    sourceConceptSet,
    timeInterval,
    statistic
    ) {
      super$initialize(
        sectionLabel = sectionLabel,
        domainTable = domainTable,
        lineItemClass = "SourceConceptSet",
        valueDescription = "source_codeset_id",
        statistic = statistic,
        lineItemLabel = sourceConceptSet$sourceConceptName,
        timeInterval = timeInterval
      )

      .setClass(private = private, key = "sourceConceptSet", value = sourceConceptSet, class = "SourceConceptSet")
      #.setNumber(private = private, key = "typeConceptIds", value = typeConceptIds, nullable = TRUE)

    },
    #' @description retrieve the source concept set
    grabSourceConceptSet = function() {
      scs <- private$sourceConceptSet
      return(scs)
    }
  ),
  private = list(
    sourceConceptSet = NULL,
    typeConceptIds = c()
  ),
  active = list()
)



# DemographicLineItem -----

#' @title DemographicLineItem
#' @description
#' An R6 class to handle a Demographic line item
#'
#' @export
DemographicLineItem <- R6::R6Class(
  classname = "DemographicLineItem",
  inherit = LineItem,
  public = list(
    #' @param statistic a Statistic Class object used to determine what type of analytic should be done for the line item
    initialize = function(statistic = statistic) {
      super$initialize(
        sectionLabel = "Demographics",
        domainTable = "person",
        lineItemLabel = statistic$getDemoLabel(),
        lineItemClass = "Demographic",
        statistic = statistic,
        timeInterval = NULL
      )
    }),
  private = list()
)

## CohortLineItem ----

#' @title CohortLineItem
#' @description
#' An R6 class to define a CohortLineItem
#'
#' @export
CohortLineItem <- R6::R6Class(
  classname = "CohortLineItem",
  inherit = LineItem,
  public = list(
    #' @param sectionLabel a label for the table shell section
    #' @param domainTable the domain table in the cdm
    #' @param covariateCohort a CohortInfo class with cohorts for covariates
    #' @param timeInterval a time interval class object to determine the time frame to consider the analytic
    #' @param statistic a Statistic Class object used to determine what type of analytic should be done for the line item
   initialize = function(
      sectionLabel,
      domainTable,
      covariateCohort,
      timeInterval,
      statistic
    ) {
      super$initialize(
        sectionLabel = sectionLabel,
        domainTable = domainTable,
        lineItemClass = "Cohort",
        valueDescription = "cohort_definition_id",
        statistic = statistic,
        lineItemLabel = covariateCohort$getName(),
        timeInterval = timeInterval
      )
      # add cohortInfo class object
      .setClass(private = private, key = "covariateCohort", value = covariateCohort, class = "CohortInfo")
    }
  ),
  private = list(
    covariateCohort = NULL
  )
)


## Concept Set Group -----------------

#' @title ConceptSetGroupLineItem
#' @description
#' An R6 class to define a ConceptSetGroupLineItem
#'
#' @export
ConceptSetGroupLineItem <- R6::R6Class(
  classname = "ConceptSetGroupLineItem",
  inherit = LineItem,
  public = list(
    #' @param sectionLabel a label for the table shell section
    #' @param groupLabel a label for the group
    #' @param conceptSets a group of concept sets
    #' @param domainTables the domain tables in the cdm
    #' @param timeInterval a time interval class object to determine the time frame to consider the analytic
    #' @param statistic a Statistic Class object used to determine what type of analytic should be done for the line item
    initialize = function(
      sectionLabel,
      groupLabel,
      conceptSets,
      domainTables,
      timeInterval,
      statistic
    ) {
      super$initialize(
        sectionLabel = sectionLabel,
        domainTable = domainTables,
        lineItemClass = "ConceptSetGroup",
        valueDescription = "codeset_id",
        statistic = statistic,
        lineItemLabel = groupLabel,
        timeInterval = timeInterval
      )
      csClasses <- rep("ConceptSet", length(conceptSets))
      .setListofClasses(private = private, key = "conceptSets", value = conceptSets, classes = csClasses)

    },
    #' @description retrieve the concept sets
    grabConceptSet = function() {
      cs <- private$conceptSets
      return(cs)
    }
  ),
  private = list(
    conceptSets = NULL
  ),
  active = list()
)


# Helper Classes -----

## TimeIntervalClass ------

#' @title TimeIntervalClass
#' @description
#' An R6 class to define a TimeIntervalClass
#'
#' @export
TimeIntervalClass <- R6::R6Class(
  "TimeIntervalClass",
  public = list(
    #' @param lb left bound - the start of the time interval
    #' @param rb right bound - the end of the time interval
    initialize = function(lb, rb) {
      .setNumber(private = private, key = "lb", value = lb)
      .setNumber(private = private, key = "rb", value = rb)
      invisible(self)
    },
    #' @description return the left bound
    getLb = function() {
      lb <- private$lb
      return(lb)
    },
    #' @description return the right bound
    getRb = function() {
      rb <- private$rb
      return(rb)
    },
    #' @description create and return time labels for left and right bounds
    getTimeLabel = function() {
      lbl <- glue::glue("{private$lb}d to {private$rb}d")
      return(lbl)
    },
    #' @description return a tibble with the left and right bounds
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

## Breaks Strategy -----------------

#' @title BreaksStrategy
#' @description
#' An R6 class to define a BreaksStrategy object
#'
#' @export
BreaksStrategy <- R6::R6Class(
  classname = "BreaksStrategy",
  public = list(
    #' @param name the name of the breaks strategy
    #' @param labels a character vector indicating how to label each break interval
    #' @param breaks a vector with cut points
    #' @param type the type of breaks strategy. Could be 'value' or 'concept'
    initialize = function(name, labels, breaks, type) {
      .setString(private = private, key = ".name", value = name)
      .setCharacter(private = private, key = ".labels", value = labels)
      .setListofClasses(private = private, key = ".breaks", classes = character(0), value = breaks)
      .setString(private = private, key = ".type", value =  type)
    },
    #' @description
    #' Generate SQL code for a CASE WHEN statement based on the break strategy
    #' @param ordinalId the order identifier of the line item in the table shell
    makeCaseWhenSql = function(ordinalId) {

      # if the breaks are values do it this way
      if (self$type == "value") {
        sql_when <- tibble::tibble(
          lhs = self$breaks |> as.numeric(),
          rhs = dplyr::lead(self$breaks |> as.numeric()) - 0.01,
          label = self$labels
        ) |>
          dplyr::mutate(
            #ord = dplyr::row_number(),
            expr_left = glue::glue("{lhs} <= a.value"),
            expr_right = dplyr::if_else(!is.na(rhs), glue::glue("a.value <= {rhs}"), ""),
            expr_both = glue::glue("WHEN ({expr_left} AND {expr_right}) THEN '{label}'"),
            expr_both = dplyr::if_else(is.na(rhs), gsub(" AND ", "", expr_both), expr_both)
          ) |>
          dplyr::pull(expr_both) |>
          glue::glue_collapse(sep = "\n")
      }

      # if breaks are concepts do it this way
      if (self$type == "concept") {

        sql_when <- tibble::tibble(
          lhs = purrr::map(self$breaks, ~glue::glue_collapse(.x, sep = ", ")),
          label = self$labels
        ) |>
          dplyr::mutate(
            expr_both = glue::glue("WHEN a.value IN ({lhs}) THEN '{label}'")
          ) |>
          dplyr::pull(expr_both) |>
          glue::glue_collapse(sep = "\n")

      }

      # make final case when sql
      case_when_sql <- c(
        "SELECT *,",
        "\nCASE ",
        glue::glue_collapse(sql_when, sep = "\n\t"),
        "\nELSE 'Other' END AS break_id",
        "\nFROM @pat_ts_tab a",
        "\nWHERE ordinal_id = {ordinalId}"
      ) |>
        glue::glue_collapse() |>
        glue::glue()

      return(case_when_sql)
    }

  ),
  private = list(
    .name = NA_character_,
    .type = NA_character_,
    .labels = NA_character_,
    .breaks = NULL
  ),

  active = list(
    #' @field name the name of the breaks strategy
    name = function(name) {
      .setActiveString(private = private, key = ".name", value = name)
    },
    #' @field type the type of breaks strategy. Could be 'value' or 'concept'
    type = function(type) {
      .setActiveString(private = private, key = ".type", value = type)
    },
    #' @field labels A character vector used to label each break interval
    labels = function(labels) {
      .setActiveCharacter(private = private, key = ".labels", value = labels)
    },
    #' @field breaks a vector with cut points
    breaks = function(breaks) {
      .setActiveList(private = private, key = ".breaks", value = breaks, classes = character(0))
    }
  )
)

