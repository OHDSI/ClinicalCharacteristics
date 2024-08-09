# Bind --------------
# Functions to bind files together

read_add_time <- function(file) {

  fn <- basename(tools::file_path_sans_ext(file))
  split_fn <- stringr::str_split_1(fn, pattern = "_")
  timeName <- split_fn[-1] |> paste(collapse = " - ")

  dt <- readr::read_csv(file, show_col_types = FALSE) |>
    dplyr::mutate(
      timeId = timeName,
      .before = 2
    )
  return(dt)
}

delet_indiv_file <- function(file) {

  fs::file_delete(file)

  ff <- basename(file)
  cli::cat_bullet(
    glue::glue("Deleting file: {crayon::green(ff)}"),
    bullet = "info",
    bullet_col = "blue"
  )
  invisible(ff)
}

#' Bind Covariate Files by time window
#' @param folder the folder with the clinical characteristic output
#' @param domain the domain to bind, use lead domains with multiple time windows
#' @param delete option specifying whether to delete the individual files
#' @return invisible return, saves bound files
#' @export
bindCovariateFilesByTime <- function(folder, domain, delete = TRUE) {

  # find files
  files <- fs::dir_ls(folder, regex = domain)

  # bind files as a dataframe
  dat <- purrr::map_dfr(
    files,
    ~read_add_time(file = .x)
  )

  #write out bound file
  readr::write_csv(
    x = dat,
    file = fs::path(folder, domain, ext = "csv")
  )
  cli::cat_bullet(
    glue::glue("Bind {crayon::blue(domain)} files together"),
    bullet = "tick",
    bullet_col = "green"
  )

  # remove individual files
  if (delete) {
    purrr::walk(files, ~delet_indiv_file(.x))
  }


  invisible(dat)

}
