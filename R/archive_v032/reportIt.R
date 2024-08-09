# Function to convert time windows into report text
report_time <- function(x) {
  txt <- x@time |>
    dplyr::mutate(
      txt = glue::glue("* {time_a} days to {time_b} days")
    ) |>
    dplyr::pull(txt)
  txt <- glue::glue_collapse(txt, sep = "\n")
  return(txt)
}

# get the concept ids from the expression and add tags for descendants and exclusion
get_concept_ids <- function(ex) {
  cids <- purrr::map_int(ex, ~.x@Concept@concept_id)
  desc <- purrr::map_lgl(ex, ~.x@includeDescendants)
  exclude <- purrr::map_lgl(ex, ~.x@isExcluded)

  tb <- tibble::tibble(
    cids = cids,
    desc = desc,
    exclude = exclude
  ) |>
    dplyr::mutate(
      desc = dplyr::if_else(desc, "^+^", ""),
      exclude = dplyr::if_else(exclude, "^*^", ""),
      txt = glue::glue("{cids}{desc}{exclude}")
    )

  concept_txt <- paste(tb$txt, collapse = ", ")

  return(concept_txt)
}

# report the concepts as a table
report_concepts <- function(x) {

  cs <- x@conceptSets
  nms <- purrr::map_chr(cs, ~.x@Name)
  cids <- purrr::map_chr(cs, ~get_concept_ids(.x@Expression))

  tb <- tibble::tibble(
    name = nms,
    concepts = cids
  )

  tbl_txt <- knitr::kable(tb, format = "pipe")
  return(tbl_txt)
}

# Markdown section  --------------------

make_intro_section <- function(clinChar) {

  tb <- tibble::tibble(
    id = clinChar@targetCohort@id,
    name = clinChar@targetCohort@name
  )

  tbl_txt <- knitr::kable(tb, format = "pipe")


  txt <- c(
    "% Clinical Characteristics",
    "\nIn this study we descrice the clinical charateristics for the following cohorts of interest:\n",
    tbl_txt,
    "\nContinuous variables are described using mean, standard deviation, minimum, median, maximum and the 25th and 75th percentiles.",
    "Categorical variables are described by the number of observations and percentage relative to the population."
  ) |>
    glue::glue_collapse(sep = "\n")

  return(txt)

}


make_demo_section <- function(clinChar) {

  cat_ids <- purrr::map_int(clinChar@extractSettings, ~.x@categoryId)
  demo_ids <- which(cat_ids %in% c(1001, 1002, 1003, 1004, 1005, 1006))
  if (length(demo_ids) > 0) {
    demo_char <- clinChar@extractSettings[demo_ids]
    demo_txt <- purrr::map(demo_char, ~report_it(.x)) |>
      purrr::list_c()

    txt <- c(
      "## Demographics",
      "\nThe following demographics are described for the cohorts of interest:\n",
      demo_txt
    ) |>
      glue::glue_collapse(sep = "\n")
  } else{
    txt <- "\n"
  }

  return(txt)
}


make_presence_section <- function(clinChar) {

  cat_class <- purrr::map_chr(clinChar@extractSettings, ~methods::is(.x))
  pres_ids <- which(cat_class == "presenceChar")
  if (length(pres_ids) > 0) {
    pres_char <- clinChar@extractSettings[pres_ids]
    pres_txt <- purrr::map_chr(pres_char, ~report_it(.x)) |>
      glue::glue_collapse(sep = "\n\n")

    txt <- c(
      "## Presence of Clinical Events",
      "\n",
      pres_txt
    ) |>
      glue::glue_collapse(sep = "\n")
  } else{
    txt <- "\n"
  }

  return(txt)
}


make_count_section <- function(clinChar) {

  cat_class <- purrr::map_chr(clinChar@extractSettings, ~methods::is(.x))
  cnt_ids <- which(cat_class == "countChar")
  if (length(cnt_ids) > 0) {
    cnt_char <- clinChar@extractSettings[cnt_ids]
    cnt_txt <- purrr::map_chr(cnt_char, ~report_it(.x)) |>
      glue::glue_collapse(sep = "\n")

    txt <- c(
      "## Number of Clinical Events",
      "\n",
      cnt_txt
    ) |>
      glue::glue_collapse(sep = "\n")
  } else{
    txt <- ""
  }

  return(txt)
}


make_cost_section <- function(clinChar) {

  cat_class <- purrr::map_chr(clinChar@extractSettings, ~methods::is(.x))
  cost_ids <- which(cat_class == "costChar")
  if (length(cost_ids) > 0) {
    cost_char <- clinChar@extractSettings[cost_ids]
    cost_txt <- purrr::map_chr(cost_char, ~report_it(.x)) |>
      glue::glue_collapse(sep = "\n")

    txt <- c(
      "## Cost of Clinical Events",
      "\n",
      cost_txt
    ) |>
      glue::glue_collapse(sep = "\n")
  } else{
    txt <- ""
  }

  return(txt)
}


build_report_txt <- function(clinChar) {

  txt <- c(
    # Intro section
    make_intro_section(clinChar),

    # Demographics
    make_demo_section(clinChar),

    # Presence
    make_presence_section(clinChar),

    # Count
    make_count_section(clinChar),

    # Cost
    make_cost_section(clinChar)
    ) |>
      glue::glue_collapse(sep = "\n\n")

  #TODO add other sections
  #timeTo, timeIn, labs

  return(txt)

}

md_to_viewer <- function(txt) {
  #create a temp dir for md file
  tempDir <- tempfile()
  dir.create(tempDir)
  tmpRmd <- file.path(tempDir, "clinChar.md")
  #write file to tmp
  readr::write_lines(txt, file = tmpRmd)
  tmpHtml <- rmarkdown::render(tmpRmd, params = "ask", quiet = TRUE)
  rstudioapi::viewer(tmpHtml)
}

#' Convert ClinChar options into a report
#' @param clinChar the clinChar object
#' @param outputFolder the location to save the report output
#' @param previewReport look at the html rendering of the report, default true
#' @export
createReport <- function(clinChar, outputFolder = here::here(),
                         previewReport = TRUE) {

  report_txt <- build_report_txt(clinChar)
  cli::cat_bullet(
    "Create markdown report for clinCar",
    bullet = "pointer",
    bullet_col = "yellow"
  )
  report_path <- fs::path(outputFolder, "clinCharReport.md")
  readr::write_file(report_txt, file = report_path)
  cli::cat_bullet(
    glue::glue("Saving report to: {crayon::cyan(report_path)}"),
    bullet = "pointer",
    bullet_col = "yellow"
  )

  if (previewReport) {
    md_to_viewer(report_txt)
  }

  invisible(report_path)
}
