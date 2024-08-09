


#' Make a location table using data from the CDM
#' @param connection a database connection to the OMOP CDM using DatabaseConnector
#' @param cdmDatabaseSchema the database schema specifying identifying the cdm tables
#' @param locationColumn the column in the location table to draw labels
#' @return a locationTable class object that specifies the locationColumn used and the merge key
#' of the location Char.
#' @export
makeLocationTable <- function(connection, cdmDatabaseSchema, locationColumn = "location_source_value") {
  #TODO map the location id to a group if grouping is done
  sql <- glue::glue("SELECT l.location_id, l.{locationColumn}
  FROM {cdmDatabaseSchema}.location l") |>
    SqlRender::translate(targetDialect = connection@dbms)

  cli::cat_bullet(
    glue::glue("Note in {crayon::blue(crayon::italic('makeLocationTable()'))}"),
    bullet = "info",
    bullet_col = "blue"
  )
  txt <- glue::glue("Grabbing {crayon::green(crayon::italic(locationColumn))} from CDM")
  cli::cat_line(
    glue::glue("   {crayon::yellow('> Database Query')}: {txt}")
  )

  loc_col_sym <- rlang::sym(locationColumn)

  loc_tbl <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
    dplyr::rename_with(tolower) |>
    dplyr::rename(
      value_id = location_id,
      value_name = !!locationColumn
    )
  loc_obj <- new("locationTable",
                 column = locationColumn,
                 key = loc_tbl)

  return(loc_obj)
}


#' Function to make visit detail table
#' @param connection a database connection to the OMOP CDM using DatabaseConnector
#' @param vocabDatabaseSchema the database schema specifying identifying the vocabulary tables
#' @param detailType either specialty or care_site
#' @param detailIds the OMOP concept ids to get names for
#' @return a visitDetailTable object used in the visitDetailChar characterization
#' @export
makeVisitDetailTable <- function(connection,
                                 vocabDatabaseSchema,
                                 detailType = c("specialty", "care_site"),
                                 detailIds) {

  detailType <- match.arg(detailType)
  domain <- switch(detailType,
    'specialty' = "provider",
    'care_site' = "care_site"
  )
  column <- switch(detailType,
                   'specialty' = "specialty_concept_id",
                   'care_site' = "place_of_service_concept_id"
  )
  # make string of detailIds
  detailIds <- paste(detailIds, collapse = ", ")
  sql <- glue::glue(
    "SELECT concept_id, concept_name FROM {vocabDatabaseSchema}.concept WHERE concept_id IN ({detailIds});"
  ) |>
    SqlRender::translate(targetDialect = connection@dbms)

  txt <- glue::glue("Grabbing {crayon::green(crayon::italic('concept_name'))} from CDM")
  cli::cat_bullet(
    glue::glue("{crayon::yellow('Database Query')}: {txt}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  det_tbl <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
    tibble::as_tibble() |>
    dplyr::rename_with(tolower)

  vdTb <- new("visitDetailTable", domain = domain,
              column = column,
              key = det_tbl)

  return(vdTb)
}


#' Exploratory function to find common lab-unit combinations
#' @description
#' This function is meant to be used as a preperatory step to making a labChar slot to clinChar.
#' The input of the lab characterization is a lab-unit combination to search in the CDM. This
#' function takes labIds of interest and returns the most common unit ids found in tandem within
#' the CDM of the database of interest. From this table users can build reasonable lab characterizations
#' where summaries are based on valid lab-unit combinations. Note many lab measurements do not have
#' corresponding unit value (noted as unit_concept_id = 0). These values may be useful but require
#' assumptions about the data to make them usable in an analysis. It is recommended that users stick
#' to unit values that have a corresponding unit concept.
#' @param connection a database connection to the OMOP CDM using DatabaseConnector
#' @param cdmDatabaseSchema the database schema specifying identifying the cdm tables
#' @param vocabDatabaseSchema the database schema specifying identifying the vocabulary tables. By
#' default we use the same input as the cdmDatabaseSchema
#' @param labIds OMOP concept ids of lab measures of interest
#' @return a tibble summarizing the most common lab-unit combinations with their names
#' @export
exploreLabUnits <- function(connection,
                         cdmDatabaseSchema,
                         vocabDatabaseSchema = cdmDatabaseSchema,
                         labIds) {
  # make string of labIds
  labIds <- paste(labIds, collapse = ", ")
  sql <- glue::glue(
    "WITH T1 AS(
      SELECT m.measurement_concept_id, m.unit_concept_id, COUNT(m.unit_concept_id) AS nn
      FROM {cdmDatabaseSchema}.measurement m
      WHERE m.measurement_concept_id IN ({labIds})
      GROUP BY m.measurement_concept_id, m.unit_concept_id
    )
    SELECT t.measurement_concept_id, c1.concept_name AS measurement_name,
    t.unit_concept_id, c2.concept_name AS unit_name, t.nn
    FROM T1 t
    JOIN {vocabDatabaseSchema}.concept c1 ON t.measurement_concept_id = c1.concept_id
    JOIN {vocabDatabaseSchema}.concept c2 ON t.unit_concept_id = c2.concept_id
    ;"
    ) |>
    SqlRender::translate(targetDialect = connection@dbms)

  cli::cat_bullet(
    glue::glue("Note in {crayon::blue(crayon::italic('findLabUnits()'))}"),
    bullet = "info",
    bullet_col = "blue"
  )
  txt <- glue::glue("Grabbing {crayon::magenta('Measurement-Unit Combinations')} from CDM")
  cli::cat_line(
    glue::glue("  {crayon::yellow('> Database Query')}: {txt}")
  )


  unit_tbl <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
    tibble::as_tibble() |>
    dplyr::rename_with(tolower) |>
    dplyr::group_by(measurement_concept_id) |>
    dplyr::mutate(
      ord = dplyr::row_number(dplyr::desc(nn)),
      .before = 1
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      ord, measurement_concept_id
    )
  return(unit_tbl)
}


get_concept_name <- function(connection, vocabDatabaseSchema, ids) {
  ids <- paste(ids, collapse = ", ")
  sql <- glue::glue(
    "SELECT concept_id, concept_name FROM {vocabDatabaseSchema}.concept WHERE concept_id IN ({ids});"
  ) |>
    SqlRender::translate(targetDialect = connection@dbms)

  nm_tbl <- DatabaseConnector::querySql(connection = connection, sql = sql) |>
    tibble::as_tibble() |>
    dplyr::rename_with(tolower)
  return(nm_tbl)
}


set_lab_unit_combos <- function(connection, vocabDatabaseSchema, labId, unitIds){

  if(length(labId) != 1) {
    stop("labId must be a single id")
  }
  tb <- tibble::tibble(
    measurement_concept_id = rep(labId, length.out = length(unitIds)),
    unit_concept_id = unitIds
  )

  nm_tbl <- get_concept_name(connection = connection,
                              vocabDatabaseSchema = vocabDatabaseSchema,
                              ids = c(labId, unitIds))

  final_tb <- tb |>
    dplyr::left_join(
      nm_tbl, by = c("measurement_concept_id" = "concept_id")
    )  |>
    dplyr::rename(
      measurement_name = concept_name
    ) |>
    dplyr::relocate(
      measurement_name, .after = measurement_concept_id
    ) |>
    dplyr::left_join(
      nm_tbl, by = c("unit_concept_id" = "concept_id")
    ) |>
    dplyr::rename(
      unit_name = concept_name
    ) |>
    dplyr::relocate(
      unit_name, .after = unit_concept_id
    )
  return(final_tb)
}

#' Function to list labUnit combos
#' @description
#' This function lists all possible combinations of labs with units
#' @param labIds a list of lab ids
#' @param unitList a list of unit ids
#' @param comboNames a list of combination names
#' @return a list of lab unit combinations
#' @export
listLabUnitCombos <- function(labIds, unitList, comboNames = NULL) {
  check <- length(labIds) == length(unitList)
  ll <- purrr::map2(labIds, unitList,
              ~list('lab' = .x,
                    'units' = .y))
  if (is.null(comboNames)) {
    comboNames <- paste0("labUnitCombo_", seq_along(labIds))
  }
  names(ll) <- comboNames
  return(ll)
}

#' Function to set lab-unit combo
#' @description
#' This function is meant to be build combinations of lab-unit concepts for the lab characterization
#' @param connection a database connection to the OMOP CDM using DatabaseConnector
#' @param vocabDatabaseSchema the database schema specifying identifying the vocabulary tables
#' @param labUnitList a list to coerce into a labUnit Table
#' @return a labUnitTable object used in the labChar characterization
#' @export
makeLabUnitTable <- function(connection, vocabDatabaseSchema, labUnitList) {

  txt <- glue::glue("Grabbing {crayon::green(crayon::italic('concept_name'))} from CDM")
  cli::cat_bullet(
    glue::glue("{crayon::yellow('Database Query')}: {txt}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  tb <- purrr::map_dfr(
    labUnitList,
    ~set_lab_unit_combos(
      connection = connection, vocabDatabaseSchema = vocabDatabaseSchema,
      labId = .x$lab,
      unitIds = .x$units
    )
  ) |>
    dplyr::mutate(
      lab_unit_code = as.numeric((measurement_concept_id * 1000000) + (unit_concept_id - (floor(unit_concept_id/1000) * 1000)))
    )
  lut <- new("labUnitTable", key = tb)
  return(lut)

}


check_lab_unit_tibble <- function(tbl) {

  tbl_names <- names(tbl)
  expected_names <- c('measurement_concept_id', 'measurement_name', 'unit_concept_id', 'unit_name')
  check <- all(expected_names %in% tbl_names)
  return(check)
}

#' Function to convert table to labUnitTable
#' @description
#' This function is meant to convert the tibble output from exploreLabUnits into
#' a labUnitTable needed for a labChar
#' @param tbl a tibble with names measurement_concept_id, measurement_name, unit_concept_id and unit_name.
#' This table most naturally comes from exploreLabUnits function. But may be made custom but not recommended
#' @param noZeroConcept a toggle to remove the 0 unit concept id which stands for no matching unit
#' @param ordLimit keep the top 3 combos
#' @return a labUnitTable object used in the labChar characterization
#' @export
convertToLabUnitTable <- function(tbl, noZeroConcept = FALSE, ordLimit = 3) {

  if (!check_lab_unit_tibble(tbl)) {
    stop("The input tibble is not the right format. Must have names measurement_concept_id, measurement_name, unit_concept_id and unit_name.")
  }

  if(noZeroConcept) {
    tbl <- tbl |>
      dplyr::filter(
        unit_concept_id != 0
      )
  }

  if (!is.null(ordLimit)) {
    tbl <- tbl |>
      dplyr::filter(
        ord <= 3
      )
  }


  tb <- tbl |>
    dplyr::select(measurement_concept_id, measurement_name, unit_concept_id, unit_name) |>
    dplyr::mutate(
      lab_unit_code = as.numeric(glue::glue("{measurement_concept_id}000{unit_concept_id}"))
    )

  lut <- new("labUnitTable", key = tb)

  return(lut)
}

