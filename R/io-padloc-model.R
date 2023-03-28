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

# TODO: Write a test.
# TODO: Write an example.
#' @title Build a list of generic padloc models from a list of loci
#' @description Build a list of generic padloc models from a list of loci.
#' @param df A [base::data.frame()] with at least one column, containing the
#' locus structures in the form `geneA__geneB__geneC`, where `__`
#' indicates separation between genes and can optionally be set to any other
#' delimiter by specifying `delim`.
#' @param loci_col The name of the column containing locus structures,
#' @param delim The delimiter that separates gene names.
#' @param name_prefix A prefix to use when generating generic names for each
#' model.
#' @return A [tibble::tibble()].
#' @export
build_generic_padloc_models <- function(df, loci_col, delim = "__", name_prefix = "SYS_") {
  df_w_names <- dplyr::mutate(df, name = paste0(name_prefix, stringr::str_pad(dplyr::row_number(), width = 5, pad = 0)))
  df_w_models <- dplyr::mutate(
    df_w_names,
    model = list(
      list(
        maximum_separation = 0,
        minimum_core = stringr::str_count({{ loci_col }}, delim) + 1,
        minimum_total = stringr::str_count({{ loci_col }}, delim) + 1,
        core_genes = unlist(stringr::str_split({{ loci_col }}, delim)),
        optional_genes = NA,
        prohibited_genes = NA
      )
    ),
    .by = name
  )
  models <- df_w_models$model
  names(models) <- df_w_names$name
  models
}

# TODO: Write a test.
# TODO: Write an example.
#' @title Convert a padloc model to a [glue::glue()] character vector
#' @description Convert a padloc model to a [glue::glue()] character vector.
#' Used for preparing a padloc model to be written to a file.
#' @param model A padloc model.
#' @return A length 1 [glue::glue()] character vector.
#' @export
padloc_model_to_chr <- function(model) {
  maximum_separation <- paste0("maximum_separation: ", model$maximum_separation)
  minimum_core <- paste0("minimum_core: ", model$minimum_core)
  minimum_total <- paste0("minimum_total: ", model$minimum_total)
  core_genes <- paste0("core_genes:\n", paste0("  - ", model$core_genes, collapse = "\n"))
  optional_genes <- paste0("optional_genes:\n", paste0("  - ", model$optional_genes, collapse = "\n"))
  prohibited_genes <- paste0("prohibited_genes:\n", paste0("  - ", model$prohibited_genes, collapse = "\n"))
  out <- glue::glue_collapse(
    c(maximum_separation, minimum_core, minimum_total, core_genes, optional_genes, prohibited_genes),
    sep = "\n"
  )
  out
}

# TODO: Write a test.
# TODO: Write an example.
#' @title Write out a padloc model (`*.yaml`) file
#' @description Write out a padloc model (`*.yaml`) file.
#' @param model A padloc model.
#' @param file Path to a file.
#' @return NULL
#' @export
write_padloc_model <- function(model, file) {
  model_chr <- padloc_model_to_chr(model)
  readr::write_lines(model_chr, file)
}

# TODO: Write a test.
# TODO: Write an example.
#' @title Write out a list of padloc model (`*.yaml`) files
#' @description Write out a list of padloc model (`*.yaml`) files.
#' @param model_list A [base::list()] of padloc models.
#' @param directory Path to a directory, where files will be written.
#' @return NULL
#' @export
multi_write_padloc_model <- function(model_list, directory) {
  purrr::iwalk(
    .x = model_list,
    .f = function(model, model_name) {
      file <- paste0(model_name, ".yaml")
      path <- fs::path_join(c(directory, file))
      write_padloc_model(model, path)
    }
  )
}
