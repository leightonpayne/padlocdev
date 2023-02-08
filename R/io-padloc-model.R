# TODO: Write a test.
#' @title Read in a padloc model (`*.yaml`) file
#' @description Read in a padloc model (`*.yaml`) file.
#' @param file Path to padloc model in yaml format (*.yaml).
#' @return A [base::list()].
#' @export
#' @examples
#' padloc_model <- read_padloc_model(padlocdev_example("padloc-db/sys/DRT_class_I.yaml"))
read_padloc_model <- function(file) {
  input <- yaml::read_yaml(file)
  out <- padloc_model(
    maximum_separation = input$maximum_separation,
    minimum_core       = input$minimum_core,
    minimum_total      = input$minimum_total,
    core_genes         = input$core_genes,
    optional_genes     = input$optional_genes,
    prohibited_genes   = input$prohibited_genes
  )
  out
}

#' Read the filename, NAME and ACC identifiers from a profile HMM into a list.
#' @param directory Path to a directory containing profile HMMs (*.hmm)
#' @return A [base::list()].
#' @export
#' @examples
#' multi_read_hmm_header(padlocdev_example("padloc-db/sys"))
multi_read_padloc_model <- function(directory) {
  files <- list.files(directory, full.names = TRUE, pattern = "*.yaml")
  padloc_models <- lapply(X = files, FUN = read_padloc_model)
  model_names <- stringr::str_remove(basename(files), ".yaml")
  names(padloc_models) <- model_names
  padloc_models
}
