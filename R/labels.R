# function to make an empty labels
.emptyLabels <- function(type ) {
  if (type == "categorical") {
    tb <- tibble::tibble(
      cohortId = NA_real_,
      cohortName = NA_character_,
      categoryId = NA_real_,
      categoryLabel = NA_character_,
      timeId = NA_real_,
      twLabel = NA_character_,
      valueId = NA_real_,
      name = NA_character_,
      n = NA_real_,
      pct = NA_real_
    )
  }

  if (type == "continuous") {
    tb <- tibble::tibble(
      cohortId = NA_real_,
      cohortName = NA_character_,
      categoryId = NA_real_,
      categoryLabel = NA_character_,
      timeId = NA_real_,
      twLabel = NA_character_,
      valueId = NA_real_,
      name = NA_character_,
      n = NA_real_,
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      p25 = NA_real_,
      median = NA_real_,
      p75 = NA_real_,
      max = NA_real_
    )
  }

  return(tb)
}


.makeTargetCohortLabels <- function(targetCohort) {
  targetCohortKey <- tibble::tibble(
    cohortId = purrr::map_int(targetCohort, ~.x$getId()),
    cohortName = purrr::map_chr(targetCohort, ~.x$getName())
  )
  return(targetCohortKey)
}

# function to make concept set labels
.formatConceptSets <- function(results, type, targetCohortKey, categoryKey, conceptSetMeta){

  if (!is.null(conceptSetMeta)) {
    csLabels <- conceptSetMeta |>
      dplyr::left_join(
        categoryKey, by = c("categoryId")
      ) |>
      dplyr::mutate(
        categoryLabel = glue::glue("{snakecase::to_title_case(domain)}: {statType}"),
        valueId = csId,
        timeId = twId
      ) |>
      dplyr::select(
        categoryId, categoryLabel, timeId, twLabel, valueId, name
      )
    # label the rsuls
    resultsCsLabelled <- results |>
      dplyr::rename_with(snakecase::to_lower_camel_case) |>
      dplyr::inner_join(
        csLabels,
        by = c("categoryId",
               "timeId",
               "valueId")
      )
    if (type == "categorical") {
      resultsCsLabelled <- resultsCsLabelled  |>
        dplyr::left_join(
          targetCohortKey,
          by = c("cohortId")
        ) |>
        dplyr::select(
          cohortId, cohortName, categoryId, categoryLabel, timeId, twLabel, valueId, name, n, pct
        )
    }

    if (type == "continuous") {
      resultsCsLabelled <- resultsCsLabelled |>
        dplyr::left_join(
          targetCohortKey,
          by = c("cohortId")
        ) |>
        dplyr::mutate(
          mean = occCnt / n
        ) |>
        dplyr::select(
          cohortId, cohortName, categoryId, categoryLabel, timeId, twLabel, valueId, name,
          n, mean, sd, min, p25, median, p75, max
        )
    }

  } else {
    resultsCsLabelled <- .emptyLabels(type = type)

  }
  return(resultsCsLabelled)
}


# function to make concept set labels
.formatDemographics <- function(results, type, targetCohortKey, demoMeta){

  if (!is.null(demoMeta)) {

    resultsDemoLabelled <- results |>
      dplyr::rename_with(snakecase::to_lower_camel_case) |>
      dplyr::inner_join(
        demoMeta,
        by = c("categoryId",
               "timeId")
      ) |>
      dplyr::mutate(
        name = NA_character_
      )

    if (type == "categorical") {
      resultsDemoLabelled <- resultsDemoLabelled  |>
        dplyr::left_join(
          targetCohortKey,
          by = c("cohortId")
        ) |>
        dplyr::select(
          cohortId, cohortName, categoryId, categoryLabel, timeId, twLabel, valueId, name, n, pct
        )
    }

    if (type == "continuous") {
      resultsDemoLabelled <- resultsDemoLabelled |>
        dplyr::left_join(
          targetCohortKey,
          by = c("cohortId")
        ) |>
        dplyr::mutate(
          mean = occCnt / n
        ) |>
        dplyr::select(
          cohortId, cohortName, categoryId, categoryLabel, timeId, twLabel, valueId, name,
          n, mean, sd, min, p25, median, p75, max
        )
    }

  } else {
    resultsDemoLabelled <- .emptyLabels(type = type)

  }
  return(resultsDemoLabelled)
}
