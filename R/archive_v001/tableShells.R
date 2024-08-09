domain_to_id <- function(domain = c("Drugs", "Conditions")) {

  #TODO add domains
  domain <- match.arg(domain)
  switch(domain,
         Drugs = "410",
         Conditions = "210")

}

clean_cat_demo <- function(dt) {

  # initial clean up of demographics data
  dd <- dt |>
    dplyr::mutate(
      covariateName = dplyr::case_when(
        analysisId %in% c(1, 4, 5) ~ snakecase::to_title_case(gsub(".*\\=\\s", "", covariateName)),
        TRUE ~ covariateName
      ),
      category = dplyr::case_when(
        analysisId == 1 ~ "Gender",
        analysisId == 3 ~ "Age",
        analysisId == 4 ~ "Race",
        analysisId == 5 ~ "Ethnicity",
        analysisId == 6 ~ "Year"
      ),
      sumValue = scales::label_comma()(sumValue),
      averageValue = scales::label_percent(accuracy = 0.01)(averageValue)
    ) |>
    dplyr::arrange(analysisId, covariateName) |>
    dplyr::select(
      cohortDefinitionId, category, covariateName, sumValue, averageValue
    )

  return(dd)

}


pluck_demographics <- function(folder, type = c("categorical", "continuous"), ids = NULL) {

  type <- match.arg(type)
  file <- glue::glue("{type}_demographics_covariates.csv")

  # set path
  filePath <- fs::path(folder, file)

  dt <- readr::read_csv(
    file = filePath, show_col_types = FALSE
  )

  # if ids is not null subset by the analysisIds
  if (!is.null(ids)) {
    dt <- dt |>
      dplyr::filter(
        analysisId %in% ids
      )
  }

  if (type == "Categorical") {
    # clean categorical demographics
    dt <- clean_cat_demo(dt)
  }

  return(dt)
}

pluck_domain <- function(folder, domain, timeWindow = NULL, ids) {

  if (!is.null(timeWindow)) {
    file <- glue::glue("{domain}_{timeWindow}.csv")
  } else{
    file <- glue::glue("{domain}.csv")
  }

  # make file

  # set path
  filePath <- fs::path(folder, file)
  #read in csv
  dt <- readr::read_csv(
    file = filePath,
    show_col_types = FALSE
  )


  #make the covariate id
  covariateIds <- paste0(conceptIds, domain_to_id(domain)) |> as.integer()

  #build snippet
  snippet <- dt |>
    dplyr::filter(
      covariateId %in% covariateIds
    ) |>
    dplyr::mutate(
      averageValue = averageValue * 100
    ) |>
    dplyr::select(
      -c(analysisId, covariateId)
    )

  return(snippet)

}

