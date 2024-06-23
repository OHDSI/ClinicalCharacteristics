
.getAssertChoices <- function(category) {
  read.csv(file = system.file(package = pkgload::pkg_name(), "csv", "assertChoices.csv"),
           as.is = TRUE, stringsAsFactors = FALSE) |>
    dplyr::filter(category == !!category) |>
    dplyr::pull(choice)
}
