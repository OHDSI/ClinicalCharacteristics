# TableShell -----

#' @description
#' An R6 class to define a TableShell object
#'
#' @export
TableShell <- R6::R6Class("TableShell",
  public = list(
    initialize = function(name,
                          targetCohorts,
                          lineItems) {
      .setString(private = private, key = "name", value = name)
      .setListofClasses(private = private, key = "targetCohorts", value = targetCohorts, classes = c("CohortInfo"))
      #.setClass(private = private, key = "executionSettings", value = executionSettings, class = "ExecutionSettings")
      .setListofClasses(private = private, key = "lineItems", value = lineItems, classes = c("LineItem"))
    },
    getName = function() {
      tsName <- private$name
      return(tsName)
    },
    getTargetCohorts = function() {
      tsTargetCohorts <- private$targetCohorts
      return(tsTargetCohorts)
    },
    getLineItems = function() {
      tsLineItems <- private$lineItems
      return(tsLineItems)
    },

    printJobDetails = function() {

      titleNm <- self$getName()

      tcs <- self$getTargetCohorts()
      cohortPrintInfo <- purrr::map_chr(tcs, ~.x$targetCohortDetails())

      # get line item info
      lineItems <- self$getLineItems()
      liPrintInfo <- purrr::map_chr(lineItems, ~.x$lineItemDetails())

      cli::cat_bullet(
        glue::glue_col("{yellow Job Details for Table Shell: {titleNm}}"),
        bullet = "pointer",
        bullet_col = "yellow"
      )

      cli::cat_bullet(
        glue::glue("Target Cohort Details:"),
        bullet = "pointer",
        bullet_col = "yellow"
      )

      cli::cat_line(
        glue::glue("\t{cohortPrintInfo}")
      )


      cli::cat_bullet(
        glue::glue("Line Item tasks:"),
        bullet = "pointer",
        bullet_col = "yellow"
      )

      cli::cat_line(
        glue::glue("\t{liPrintInfo}")
      )

      invisible(liPrintInfo)

    },

    # function to insert time windows
    insertTimeWindows = function(executionSettings, buildOptions) {

      # ensure that executionSettings R6 object used
      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)

      # get concept set line items
      csLineItems <- private$.pluckLineItems(classType = "ConceptSetLineItem")

      # make the time windows table
      time_tbl <- purrr::map_dfr(csLineItems, ~.x$getTimeInterval()) |>
        dplyr::distinct() |>
        dplyr::mutate(
          time_id = dplyr::row_number(), .before = 1
        ) |>
        dplyr::rename(
          time_a = lb,
          time_b = rb
        )

      cli::cat_bullet(
        glue::glue("Insert time window tables for characterization"),
        bullet = "pointer",
        bullet_col = "yellow"
      )

      # establish connection to database
      connection <- executionSettings$getConnection()

      if (is.null(connection)) {
        connection <- executionSettings$connect()
      }

      # insert the time windows into the database
      DatabaseConnector::insertTable(
        connection = connection,
        tableName = buildOptions$timeWindowTempTable,
        tempEmulationSchema = executionSettings$tempEmulationSchema,
        data = time_tbl,
        tempTable = TRUE
      )

      invisible(time_tbl)

    },

    #key function to generate the table shell
    buildTableShellSql = function(executionSettings, buildOptions) {

      # ensure that executionSettings R6 object used
      checkmate::assert_class(executionSettings, classes = "ExecutionSettings", null.ok = FALSE)


      cli::cat_bullet(
        glue::glue_col("{yellow Preparing table shell sql}"),
        bullet = "pointer",
        bullet_col = "yellow"
      )

      # collect all the sql
      fullSql <- c(

        # step 1: dat table ddl
        private$.makeDatTable(),

        # step 2: create targe cohort table
        private$.getTargetCohortSql(),

        # Step 3: Create line items

          # A) Demographics
        private$.buildDemographicsQuery(),

          # B) Concept Set
        # create codeset query
        private$.buildCodesetQueries(buildOptions),
        # create concept set query
        private$.buildConceptLineItemQuery(),

          # C) Multi-domain groups
        # grp_Sql <- private$.buildGroupLineItemQuery()

          # D) Cohorts
        #cd_sql <- private$.buildCohortLineItemQuery()

        # Step 4: Make table drop sql
        private$.dropCsTempTables()
      ) |>
        glue::glue_collapse(sep = "\n")

      # render it with schema info
      renderedSql <- SqlRender::render(
        sql = fullSql,
        cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
        vocabularyDatabaseSchema = executionSettings$cdmDatabaseSchema,
        workDatabaseSchema = executionSettings$workDatabaseSchema,
        cohortTable = executionSettings$targetCohortTable,
        dataTable = buildOptions$resultsTempTable,
        codesetTable = buildOptions$codesetTempTable,
        targetTable = buildOptions$targetCohortTempTable,
        timeWindowTable = buildOptions$timeWindowTempTable
      )

      # translate and prep for execution
      finalSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = executionSettings$getDbms(),
        tempEmulationSchema = executionSettings$tempEmulationSchema
      )

      return(finalSql)

    },

    # function to aggregate categorical vars into table
    aggregateTableShell = function(executionSettings, type, buildOptions) {

      #identify which lineItems are continuous or categorical
      idList <- private$.identifyCategoryIds()

      # get sql for categorical
      if (type == "categorical") {

        categoricalIds <- idList |>
          dplyr::filter(distributionType == "categorical") |>
          dplyr::pull(categoryId) |>
          paste(collapse = ", ")


        sqlFile <- "aggregateCategorical.sql"
        # get sql from package
        sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
          readr::read_file() |>
          glue::glue() |>
          SqlRender::render(
            workDatabaseSchema = executionSettings$workDatabaseSchema,
            cohortTable = executionSettings$targetCohortTable,
            dataTable = buildOptions$resultsTempTable
          )
      }

      # get sql for continuous
      if (type == "continuous") {

        continuousIds <- idList |>
          dplyr::filter(distributionType == "continuous") |>
          dplyr::pull(categoryId) |>
          paste(collapse = ", ")

        sqlFile <- "aggregateContinuous.sql"
        # get sql from package
        sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
          readr::read_file() |>
          glue::glue() |>
          SqlRender::render(
            dataTable = buildOptions$resultsTempTable
          )
      }

      finalSql <- sql |>
        SqlRender::translate(
          targetDialect = executionSettings$getDbms(),
          tempEmulationSchema = executionSettings$tempEmulationSchema
        )

      # get aggregateTable
      aggregateTable <- DatabaseConnector::querySql(
        connection = executionSettings$getConnection(),
        sql = finalSql
      ) |>
        tibble::as_tibble() |>
        dplyr::rename_with(tolower) |>
        dplyr::arrange(cohort_id, category_id, time_id, value_id)

      # format results
      formattedTable <- private$.labelResults(
        results = aggregateTable,
        type = type
      )

      return(formattedTable)

    }


  ),
  private = list(
    name = NULL,
    targetCohorts = NULL,
    lineItems = NULL,


    ### private methods ---------------

    #function to create dat table
    .makeDatTable = function(){
      sqlFile <- "datTable.sql"
      # get sql from package
      sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
        readr::read_file()
      return(sql)
    },

    # function to get target cohort sql
    .getTargetCohortSql = function() {
      sqlFile <- "targetCohort.sql"
      cohortIds <- purrr::map_int(
        private$targetCohorts,
        ~.x$getId()
      )
      # get sql from package
      sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
        readr::read_file() |>
        glue::glue()
      return(sql)
    },

    # pluck Concept Set Line Items
    .pluckLineItems = function(classType) {
      lineItems <- self$getLineItems() |>
        .getLineItemClassType(classType)
      return(lineItems)
    },

    .identifyCategoryIds = function() {
      # get line items
      li <- self$getLineItems()

      #figure if base stat is continuous or categorical
      categoryIdTbl <- dplyr::bind_rows(
        .findDemographicCategoryIds(li),
        .findConceptSetCategoryIds(li)
      )

      return(categoryIdTbl)

    },

    .grabDemographicsMetaTable = function() {
      demoLineItems <- private$.pluckLineItems(classType = "DemographicLineItem")

      demoMetaTable <- tibble::tibble(
        'categoryId' = purrr::map_int(demoLineItems, ~.x$ordinal),
        'catName' = purrr::map_chr(demoLineItems, ~.x$getName()),
        'timeId' = -999,
        'twLabel' = "Static at Index"
      ) |>
        dplyr::mutate(
          categoryLabel = glue::glue("Demographics: {catName}")
        ) |>
        dplyr::select(
          -c(catName)
        )

      return(demoMetaTable)

    },

    # function to prep demographics sql
    .buildDemographicsQuery = function() {

      # get concept set line items
      demoLineItems <- private$.pluckLineItems(classType = "DemographicLineItem")
      if (length(demoLineItems) >= 1) {
        demoSql <- purrr::map(
          demoLineItems,
          ~.x$getSql()
        )|>
          glue::glue_collapse(sep = "\n\n")
      } else {
        demoSql <- ""
      }

      return(demoSql)

    },


    # function to create sql for codset query
    .buildCodesetQueries = function(buildOptions) {

      #temporary change with class
      codesetTable <-  buildOptions$codesetTempTable

      # get concept set line items
      csLineItems <- private$.pluckLineItems(classType = "ConceptSetLineItem")
      if (length(csLineItems) >= 1) {
        # retrieve each concept set from the line items and flatten
        csCapr <- purrr::map(
          csLineItems,
          ~.x$grabConceptSet()
        )
        # remove duplicated ids
        cs_id <- !duplicated(purrr::map_chr(csCapr, ~.x@id))
        cs_tbl2 <- csCapr[cs_id]

        # change function name to .camel
        cs_query <- bind_codeset_queries(cs_tbl2, codesetTable = codesetTable)
      } else{
        cs_query <- ""
      }

      return(cs_query)

    },

    .grabConceptSetMetaTable = function() {
      csLineItems <- private$.pluckLineItems(classType = "ConceptSetLineItem")
      # only run if CSD in ts
      if (length(csLineItems) >= 1) {

        # Step 1: Get the concept set meta
        csMeta <- .conceptSetMeta(csLineItems)
      } else {
        csMeta <- NULL
      }
      return(csMeta)
    },

    # function to extract concept level information
    .buildConceptLineItemQuery = function() {

      # Step 1: Get the concept set meta
      csMeta <- private$.grabConceptSetMetaTable()

      # only run if CSD in ts
      if (!is.null(csMeta)) {

        # Step 2: Prep the concept set extraction
        csTables <- csMeta |>
          dplyr::select(csIdSet, twIdSet, domain, tempTableName) |>
          dplyr::distinct()
        # Use sql template to create different domain joins
        csExtractSql <- purrr::pmap_chr(
          csTables,
          ~.prepCsExtract(
            csIdSet = ..1,
            twIdSet = ..2,
            domain = ..3,
            tempTableName = ..4
          )
        ) |>
          glue::glue_collapse(sep = "\n\n")

        # step 3: transform extraction based on stat
        statTb <- csMeta |>
          dplyr::select(categoryId, tempTableName, sql) |>
          dplyr::distinct()

        csTransformSql <- purrr::pmap_chr(
          statTb,
          ~.prepCsTransform(
            categoryId = ..1,
            tempTableName = ..2,
            sql = ..3
          )
        ) |>
          glue::glue_collapse(sep = "\n\n")


        csSql <- c(csExtractSql, csTransformSql) |>
          glue::glue_collapse(sep = "\n\n")

      } else {
        csSql <- ""
      }
      return(csSql)

    },

    # function to drop all cs Tables
    .dropCsTempTables = function() {

      csMeta <- private$.grabConceptSetMetaTable()

      # only run if CSD in ts
      if (!is.null(csMeta)) {

      tempTables <- csMeta  |>
        dplyr::select(tempTableName) |>
        dplyr::distinct()

      dropTmpTb0 <- purrr::pmap(tempTables, ~.truncDropTempTables(tempTableName = ..1)) |>
        glue::glue_collapse(sep = "\n\n")

      dropTmpTb <- c(
        "\n-- Drop CS Temp Tables",
        .truncDropTempTables(tempTableName = "#Codeset"),
        dropTmpTb0
      )|>
        glue::glue_collapse(sep = "\n\n")
      } else {
        dropTmpTb <- ""
      }

      return(dropTmpTb)
    },

    .labelResults = function(results, type) {

      #get target Cohort labels
      targetCohortKey <- .makeTargetCohortLabels(private$targetCohorts)


      # label the demographic results
      resultsDemoLabelled <- .formatDemographics(
        results = results,
        type = type,
        targetCohortKey = targetCohortKey,
        demoMeta = private$.grabDemographicsMetaTable())

      # get Concept Set Labels
      resultsCsLabelled <- .formatConceptSets(
        results = results,
        type = type,
        targetCohortKey = targetCohortKey,
        categoryKey = private$.identifyCategoryIds(),
        conceptSetMeta = private$.grabConceptSetMetaTable()
      )

      # Bind for output
      labelledResults <- dplyr::bind_rows(
        resultsDemoLabelled,
        resultsCsLabelled
        #add others
      )

      return(labelledResults)

    }

  )
)


# BuildOptions ----

#' @description
#' An R6 class to define build options for the tableShell
#'
#' @export
BuildOptions <- R6::R6Class(
  classname = "BuildOptions",
  public = list(
    initialize = function(keepResultsTable = NULL,
                          resultsTempTable = NULL,
                          codesetTempTable = NULL,
                          timeWindowTempTable = NULL,
                          targetCohortTempTable = NULL) {
      .setLogical(private = private, key = ".keepResultsTable", value = keepResultsTable)
      .setString(private = private, key = ".resultsTempTable", value = resultsTempTable)
      .setString(private = private, key = ".codesetTempTable", value = codesetTempTable)
      .setString(private = private, key = ".timeWindowTempTable", value = timeWindowTempTable)
      .setString(private = private, key = ".targetCohortTempTable", value = targetCohortTempTable)
    }
  ),
  private = list(
    .keepResultsTable = NULL,
    .resultsTempTable = NULL,
    .codesetTempTable = NULL,
    .timeWindowTempTable = NULL,
    .targetCohortTempTable = NULL
  ),

  active = list(

    keepResultsTable = function(value) {
      .setActiveLogical(private = private, key = ".keepResultsTable", value = value)
    },


    resultsTempTable = function(value) {
      .setActiveString(private = private, key = ".resultsTempTable", value = value)
    },


    codesetTempTable = function(value) {
      .setActiveString(private = private, key = ".codesetTempTable", value = value)
    },


    timeWindowTempTable = function(value) {
      .setActiveString(private = private, key = ".timeWindowTempTable", value = value)
    },

    targetCohortTempTable = function(value) {
      .setActiveString(private = private, key = ".targetCohortTempTable", value = value)
    }

  )
)

# ExecutionSettings ----

#' @description
#' An R6 class to define an ExecutionSettings object
#'
#' @export
ExecutionSettings <- R6::R6Class(
  classname = "ExecutionSettings",
  public = list(
    initialize = function(connectionDetails = NULL,
                          connection = NULL,
                          cdmDatabaseSchema = NULL,
                          workDatabaseSchema = NULL,
                          tempEmulationSchema = NULL,
                          targetCohortTable = NULL,
                          cdmSourceName = NULL) {
      stopifnot(is.null(connectionDetails) || is.null(connection))
      .setClass(private = private, key = "connectionDetails", value = connectionDetails, class = "ConnectionDetails")
      .setClass(private = private, key = ".connection", value = connection,
                class = "DatabaseConnectorJdbcConnection", nullable = TRUE)
      .setString(private = private, key = ".cdmDatabaseSchema", value = cdmDatabaseSchema)
      .setString(private = private, key = ".workDatabaseSchema", value = workDatabaseSchema)
      .setString(private = private, key = ".tempEmulationSchema", value = tempEmulationSchema)
      .setString(private = private, key = ".targetCohortTable", value = targetCohortTable)
      .setString(private = private, key = ".cdmSourceName", value = cdmSourceName)
    },

    getDbms = function() {
      dbms <- private$connectionDetails$dbms
      return(dbms)
    },

    # connect to database
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

    # disconnect to database
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
    .targetCohortTable = NULL,
    .cdmSourceName = NULL
  ),

  active = list(

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

    targetCohortTable = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        tct <- private$.targetCohortTable
        return(tct)
      }
      # replace the targetCohortTable
      .setString(private = private, key = ".targetCohortTable", value = value)
      cli::cat_bullet(
        glue::glue("Replaced {crayon::cyan('targetCohortTable')} with {crayon::green(value)}"),
        bullet = "info",
        bullet_col = "blue"
      )
    },

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

#' @description
#' An R6 class to define a Cohort Info object
#' CohortInfo objects do not maintain any execution settings, just the id and name
#'
#' @export
CohortInfo <- R6::R6Class("CohortInfo",
  public = list(
    initialize = function(id, name) {
      .setNumber(private = private, key = "id", value = id)
      .setString(private = private, key = "name", value = name)
    },
    getId = function() {
      cId <- private$id
      return(cId)
    },
    getName = function(name) {
      cName <- private$name
      return(cName)
    },
    cohortDetails = function(){
      id <- self$getId()
      name <- self$getName()

      info <- glue::glue_col(
        "- CohortId: {green {id}}; CohortName: {green {name}}"
      )

      return(info)

    }
    # DEFUNCT: this is now one cohort per class
    # getSql = function() {
    #   sqlFile <- "targetCohort.sql"
    #   cohortId <- private$id
    #   # get sql from package
    #   sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
    #     readr::read_file() |>
    #     glue::glue()
    # }
  ),
  private = list(
    id = NULL,
    name = NULL
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
Statistic_old <- R6::R6Class("Statistic",
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

### Demographic Age -----------------
DemographicAge <- R6::R6Class("DemographicAge",
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


### Demographic Concept -----------------
DemographicConcept <- R6::R6Class("DemographicConcept",
                           inherit = Statistic,
                           public = list(
                             initialize = function(conceptColumn) {
                               super$initialize(type = "Concept")
                               #TODO make this a setChoics of Concept, Age, Year
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


### Demographic Year -----------------
DemographicYear <- R6::R6Class("DemographicYear",
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
                          },
                          getSql = function() {

                            sqlFile <- "presenceStat.sql"
                            op <- .opConverter(private$operator)
                            occurrences <- private$occurrences
                            # get sql from package
                            sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
                              readr::read_file() |>
                              glue::glue()
                            return(sql)
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
                         if (!is.null(breaks)) {
                           .setClass(private = private, key = "breaks", value = breaks, class = "Breaks")
                         }
                         invisible(private)
                       },
                       getSql = function() {

                         sqlFile <- "countStat.sql"
                          # get sql from package
                         sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
                           readr::read_file() |>
                           glue::glue()
                         return(sql)
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
LineItem_old <- R6::R6Class("LineItem",
  public = list(
    initialize = function(name,
                          #ordinal,
                          definitionType,
                          statistic) {
      .setString(private = private, key = "name", value = name)
      #.setNumber(private = private, key = "ordinal", value = ordinal)
      # TODO change this to enforce definitionType from choice list
      .setString(private = private, key = "definitionType", value = definitionType)
      .setClass(private = private, key = "statistic", value = statistic, class = "Statistic")

      invisible(self)
    },
    getName = function() {
      name <- private$name
      return(name)
    },
    getDefinitionType = function() {
      liDefinitionType <- private$definitionType
      return(liDefinitionType)
    },
    identifyStatType = function() {
      statType <- private$statistic$getStatType() |>
        .isLineItemContinuous()
      if (statType) {
        statType <- "continuous"
      } else {
        statType <- "categorical"
      }
      return(statType)
    }

  ),

  private = list(
    name = NULL,
    .ordinal = NA_integer_,
    definitionType = NULL,
    statistic = NULL
  ),

  active = list(
    ordinal = function(value) {
      # return the value if nothing added
      if(missing(value)) {
        ord <- private$.ordinal
        return(ord)
      }
      private$.ordinal <- as.integer(value)
      invisible(private)
    }
  )

)

## ConceptSetLineItem ----

#' @description
#' An R6 class to define a ConceptSetLineItem
#'
#' @export
ConceptSetLineItem_old <- R6::R6Class("ConceptSetLineItem",
  inherit = LineItem,
  public = list(

    # initialize the class
    initialize = function(name,
                          #ordinal,
                          statistic,
                          domain,
                          conceptSet,
                          timeInterval,
                          sourceConceptSet = NULL,
                          typeConceptIds = c(),
                          visitOccurrenceConceptIds = c()) {
      super$initialize(name = name, definitionType = "ConceptSet",
                       statistic = statistic)
      .setString(private = private, key = "domain", value = domain)
      .setClass(private = private, key = "conceptSet", value = conceptSet, class = "ConceptSet")
      .setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeInterval")
      # TODO change this to enforce domain from choice list
      .setClass(private = private, key = "sourceConceptSet", value = sourceConceptSet, class = "ConceptSet", nullable = TRUE)
      .setNumber(private = private, key = "typeConceptIds", value = typeConceptIds, nullable = TRUE)
      .setNumber(private = private, key = "visitOccurrenceConceptIds", value = visitOccurrenceConceptIds, nullable = TRUE)
    },

    # gather information for print
    lineItemDetails = function() {

      ord <- self$ordinal |>
        tibble::as_tibble() |>
        dplyr::rename(
          ordinal = value
          )

      info <- self$getConceptSetRef() |>
        dplyr::select(
          -c(hash)
        ) |>
        dplyr::bind_cols(
          ord
        ) |>
        glue::glue_data_col(
          "{ordinal}) \\
          ConceptSetName: {green {name}}; \\
          ConceptSetDomain: {green {domain}}; \\
          TimeWindow: {magenta {lb}d} to {magenta {rb}d}"
        )

      return(info)
    },

    # helper to pull concept Capr class items
    grabConceptSet = function() {
      cs <- private$conceptSet
      return(cs)
    },

    # helper to retrieve the time windows in the clas
    getTimeInterval = function() {
      tw <- private$timeInterval$getTimeInterval()
      return(tw)
    },


    # helper to get reference table of the concept sets in the class
    getConceptSetRef = function() {

      # # make key for cs use
      csTbl <- tibble::tibble(
        'name' = private$name,
        'hash' = private$conceptSet@id,
        'domain' = private$domain,
        'lb' = private$timeInterval$getLb(),
        'rb' = private$timeInterval$getRb()
      )
      return(csTbl)
    },

    getStatisticInfo = function() {
      tb <- tibble::tibble(
        'ord' = self$ordinal,
        'statType' = private$statistic$getStatType(),
        'sql' = private$statistic$getSql()
      )
      return(tb)
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

# DemographicLineItem -----

#' @description
#' An R6 class to handle the ...
#'
#' @export
DemographicLineItem <- R6::R6Class("DemographicLineItem",
  inherit = LineItem,
  public = list(
    initialize = function(name,
                          #ordinal,
                          statistic) {
      super$initialize(name = name,
                       #ordinal = ordinal,
                       definitionType = "Demographic",
                       statistic = statistic)
    },

    # gather information for print
    lineItemDetails = function() {

      ord <- self$ordinal

      nm <- self$getName()

      info <- glue::glue_col(
          "{ord}) \\
          DemographicName: {green {nm}}"
      )

      return(info)
    },


    # function to get sql
    getSql = function() {

      # get stat type
      statType <- private$statistic$getStatType()

      # prep sql if Age demographic
      if (statType == "Age") {
        sqlFile <- "demoAgeChar.sql"
        ordinal <- self$ordinal
        # get sql from package
        sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
          readr::read_file() |>
          glue::glue()
      }

      # prep sql if Concept demographic
      if (statType == "Concept") {
        sqlFile <- "demoConceptChar.sql"
        ordinal <- self$ordinal
        conceptColumn <- private$statistic$getDemoColumn()
        # get sql from package
        sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
          readr::read_file() |>
          glue::glue()
      }

      # prep sql if Concept demographic
      if (statType == "Year") {
        sqlFile <- "demoYearChar.sql"
        ordinal <- self$ordinal
        # get sql from package
        sql <- fs::path_package("ClinicalCharacteristics", fs::path("sql", sqlFile)) |>
          readr::read_file() |>
          glue::glue()
      }

      return(sql)
    }
  )
)

## CohortLineItem ----

#' @description
#' An R6 class to define a CohortLineItem
#'
#' @export
CohortLineItem <- R6::R6Class("CohortLineItem",
  inherit = LineItem,
  public = list(

    # initialize the class
    initialize = function(name,
                          #ordinal,
                          statistic,
                          cohort,
                          timeInterval) {
      super$initialize(name = name,
                       definitionType = "Cohort",
                       statistic = statistic)
      .setClass(private = private, key = "cohort", value = cohort, class = "CohortInfo")
      .setClass(private = private, key = "timeInterval", value = timeInterval, class = "TimeInterval")
    },

    # gather information for print
    lineItemDetails = function() {

      ord <- self$ordinal |>
        tibble::as_tibble() |>
        dplyr::rename(
          ordinal = value
          )

      info <- self$getCohortRef() |>
        dplyr::select(
          -c(hash)
        ) |>
        dplyr::bind_cols(
          ord
        ) |>
        glue::glue_data_col(
          "{ordinal}) \\
          CohortName: {green {name}}; \\
          TimeWindow: {magenta {lb}d} to {magenta {rb}d}"
        )

      return(info)
    },

    # helper to pull cohort items
    grabCohort = function() {
      c <- private$cohort
      return(c)
    },

    # helper to retrieve the time windows in the class
    getTimeInterval = function() {
      tw <- private$timeInterval$getTimeInterval()
      return(tw)
    },

    # helper to get reference table of the cohorts in the class
    getCohortRef = function() {
      cTbl <- tibble::tibble(
        'name' = private$name,
        'lb' = private$timeInterval$getLb(),
        'rb' = private$timeInterval$getRb()
      )
      return(cTbl)
    },

    getStatisticInfo = function() {
      tb <- tibble::tibble(
        'ord' = self$ordinal,
        'statType' = private$statistic$getStatType(),
        'sql' = private$statistic$getSql()
      )
      return(tb)
    }
  ),
  private = list(
    cohort = NULL,
    timeInterval = NULL
  )
)


# Helper Classes -----

## TimeInterval ------

TimeInterval_old <- R6::R6Class(
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


# Legacy -------------


## Section ------

## @description
###' An R6 class to define a Section object
## A Section object is a logical encapsulation of LineItems
## Sections have names and ordinals for the final output
##
## @export
## Section <- R6::R6Class("Section",
#   public = list(
#     initialize = function(name,
#                           ordinal,
#                           lineItems) {
#       .setString(private = private, key = "name", value = name)
#       .setNumber(private = private, key = "ordinal", value = ordinal)
#       .setListofClasses(private = private, key = "lineItems", value = lineItems, classes = c("LineItem"))
#     },
#     getName = function() {
#       name <- private$name
#       return(name)
#     },
#     getOrdinal = function() {
#       sectionOrdinal <- private$ordinal
#       return(sectionOrdinal)
#     },
#     getLineItems = function() {
#       sectionLineItems <- private$lineItems
#       return(sectionLineItems)
#     }
#   ),
#   private = list(
#     name = NULL,
#     ordinal = NA,
#     lineItems = NULL
#   )
# )



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
#    initialize = function(ConceptSetLineItems) {
#      .setListofClasses(private = private, key = "ConceptSetLineItems",
#                        value = ConceptSetLineItems,
#                        classes = c("ConceptSetLineItem"))
#    },
#    getConceptSetLineItems = function() {
#      ConceptSetLineItems <- private$ConceptSetLineItems
#      return(ConceptSetLineItems)
#     }
#   ),
#   private = list(
#     ConceptSetLineItems = c()
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
