
#' @title Read a padloc-db sys_meta.txt file.
#' @param file Path to sys_meta.txt.
#' @return A [tibble::tibble()].
#' @export
#' @examples
#' sys_meta <- read_sys_meta(padlocdev_example("padloc-db/sys_meta.txt"))
#' sys_meta
read_sys_meta <- function(file) {
  cols <- readr::cols(
    system    = readr::col_character(),
    type      = readr::col_character(),
    yaml.name = readr::col_character(),
    search    = readr::col_logical(),
    notes     = readr::col_character()
  )
  out <- readr::read_tsv(
    file,
    skip = 1,
    col_names = names(cols$cols),
    col_types = cols
  )
  out
}

#' @title Read a padloc-db `hmm_meta.txt` file.
#' @param file Path to sys_meta.txt
#' @return A [tibble::tibble()].
#' @export
#' @examples
#' sys_meta <- read_sys_meta(padlocdev_example("padloc-db/sys_meta.txt"))
read_hmm_meta <- function(file) {
  cols <- readr::cols(
    hmm.acc                    = readr::col_character(),
    hmm.name                   = readr::col_character(),
    hmm.description            = readr::col_character(),
    protein.name               = readr::col_character(),
    secondary.name             = readr::col_character(),
    author                     = readr::col_character(),
    number.seq                 = readr::col_double(),
    length.hmm                 = readr::col_double(),
    e.value.threshold          = readr::col_double(),
    hmm.coverage.threshold     = readr::col_double(),
    target.coverage.threshold  = readr::col_double(),
    system                     = readr::col_character(),
    literature.ref             = readr::col_character(),
    database.ref               = readr::col_character(),
    comments                   = readr::col_character()
  )
  x <- readr::read_tsv(
    file,
    skip = 1,
    col_names = names(cols$cols),
    col_types = cols
  )

  required_values <- c(
    "hmm.acc",
    "hmm.name",
    "protein.name",
    "e.value.threshold",
    "hmm.coverage.threshold",
    "target.coverage.threshold"
  )

  required_missing <- any(sapply(x[required_values], is.na))

  if (required_missing) {
    error <- paste0(
      "Some rows of hmm_meta.txt are missing values in required columns. ",
      "Required values include: ", paste0(required_values, collapse = ", "),
      "."
    )
    stop(error)
  }

  x

}

#' @title Read a padloc-db sys_groups.txt file.
#' @param file Path to sys_groups.txt
#' @return A [tibble::tibble()].
#' @export
#' @examples
#' sys_groups <- read_sys_groups(padlocdev_example("padloc-db/sys_groups.txt"))
#' sys_groups
read_sys_groups <- function(file) {
  cols <- readr::cols(
    yaml.name = readr::col_character(),
    group     = readr::col_character()
  )
  out <- readr::read_tsv(
    file,
    skip = 1,
    col_names = names(cols$cols),
    col_types = cols
  )
  out
}
