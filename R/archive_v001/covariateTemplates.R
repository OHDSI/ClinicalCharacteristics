# Cohort Covariate Templates ----------

# build simple cohort definition
simpleSkeleton <- function(domainQuery) {

  cd <- Capr::cohort(

    # Index is the domain with no prior or post observation
    # limit primary event to the first
    entry = Capr::entry(
      domainQuery,
      observationWindow = Capr::continuousObservation(),
      primaryCriteriaLimit = "First"
    ),

    # include all expression events
    attrition = Capr::attrition(
      expressionLimit = "All"
    ),

    # exit strategy is end of continuous observation
    exit = Capr::exit(
      endStrategy = Capr::observationExit()
    )

  )
  return(cd)
}


makeCohortCovariateJson <- function(conceptSet, domain, cohortFolder) {

  # build appropriate domain query
  domainQuery <- rlang::call2(.fn = domain, conceptSet = conceptSet, .ns = "Capr") |>
    eval()

  #TODO create some variations of the cohort skeleton if needed
  cd <- simpleSkeleton(domainQuery = domainQuery)

  saveName <- glue::glue("{conceptSet@Name}_covariate")

  Capr::writeCohort(cd, path = fs::path(cohortFolder, saveName, ext = "json"))

  # console messages
  console_txt1 <- glue::glue("Designed cohort {crayon::blue(saveName)}")
  cli::cat_bullet(console_txt1, bullet = "tick", bullet_col = "green")
  console_txt2 <- glue::glue("Saved to {crayon::green(cohortFolder)}")
  cli::cat_bullet(console_txt2, bullet = "tick", bullet_col = "green")

  invisible(cd)

}
