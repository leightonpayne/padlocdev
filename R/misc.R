# TODO: Write a test? Maybe?
#' @title Compare two dataframes / tibbles
#' @description This is a helper function for comparing two dataframes / tibbles
#' @param a first dataframe
#' @param b second dataframe
#' @param join_by column in common
#' @return A [base::list()] of three [base::data.frame()]s or
#' [tibble::tibble()]s, one with entries in 'a' but not 'b' and vice-versa,
#' and one with entries in both 'a' and 'b'
a_vs_b <- function(a, b, join_by) {

  a_name = deparse(substitute(a))
  b_name = deparse(substitute(b))

  a_subset <- dplyr::select(a, join_by)
  b_subset <- dplyr::select(b, join_by)

  a_exists <- dplyr::mutate(a_subset, !!as.name(a_name) := TRUE)
  b_exists <- dplyr::mutate(b_subset, !!as.name(b_name) := TRUE)

  out <- list(
    a_wo_b <- dplyr::anti_join(a_exists, b_exists, by = join_by),
    b_wo_a <- dplyr::anti_join(b_exists, a_exists, by = join_by),
    complete <- dplyr::inner_join(a_exists, b_exists, by = join_by)
  )

  names(out) <- c(
    paste0(a_name, " without ", b_name),
    paste0(b_name, " without ", a_name),
    paste0(a_name, " with ", b_name)
  )

  out

}

# TODO: Write a test.
#' @title Expand gene groups
#' @description Expand secondary gene assignments in a specific category of
#' genes into their underlying genes.
#' @param model A padloc model.
#' @param gene_type The category of genes to expand, i.e. one of: "core_genes",
#' "optional_genes", or "prohibited_genes".
#' @param hmm_meta A padloc-db HMM metadata table.
#' @return A [base::character()] vector containing the expanded gene values.
#' @export
#' @examples
#' # Read data -----------------------------------------------------------------
#' path <- padlocdev_example("sys/SystemA_III.yaml")
#' model <- read_padloc_model(path)
#' model
#' path <- padlocdev_example("hmm_meta.txt") # Generate path for example model
#' hmm_meta <- read_hmm_meta(path)
#' hmm_meta
#' # Expand genes --------------------------------------------------------------
#' # The category "optional_genes" only contains the gene group "Sya_accessory"
#' model$optional_genes
#' # After expanding, "Sya_accessory" becomes "SyaB" and "SyaC"
#' model$optional_genes <- expand_gene_groups(model, "optional_genes", hmm_meta)
#' model$optional_genes
expand_gene_groups <- function(model, gene_type, hmm_meta) {
  gene_lookup <- dplyr::select(hmm_meta, "protein.name", "secondary.name")
  gene_lookup <- dplyr::filter(gene_lookup, !is.na(secondary.name))
  gene_lookup_distinct <- dplyr::distinct(gene_lookup, protein.name, secondary.name)
  genes <- model[[gene_type]]
  # Find which of the listed genes are actually a gene group, i.e. it is listed
  # somewhere in the secondary.name col of hmm_meta.txt
  gene_groups_index <- which(model[[gene_type]] %in% gene_lookup_distinct$secondary.name)
  if (length(gene_groups_index) != 0) {
    gene_groups <- model[[gene_type]][gene_groups_index]
    genes_to_add <- dplyr::filter(gene_lookup_distinct, secondary.name %in% gene_groups)[["protein.name"]]
    expanded <- c(model[[gene_type]][-gene_groups_index], genes_to_add)
  } else {
    expanded <- model[[gene_type]]
  }
  expanded
}

#' @title Expand all gene groups in a list of padloc models
#' @description Expand secondary gene assignments in all categories of genes
#' in a list of padloc models into their underlying genes.
#' @param models A list of padloc models.
#' @param hmm_meta A padloc-db HMM metadata table.
#' @return A [base::list()] of padloc models with expanded genes.
#' @export
#' @examples
#' model <- multi_read_padloc_model(padlocdev_example("sys"))
#' hmm_meta <- read_hmm_meta(padlocdev_example("hmm_meta.txt"))
expand_gene_groups_all <- function(models, hmm_meta) {
  gene_groups <- c("core_genes", "optional_genes", "prohibited_genes")
  models_expanded <- lapply(
    X = models,
    FUN = function(model) {
      for (gene_group in gene_groups) {
        model[[gene_group]] <- expand_gene_groups(model, gene_group, hmm_meta)
      }
      model
    }
  )
  models_expanded
}

# TODO: Write an example.
# TODO: Write a test.
#' @title Compare HMM files against those listed in hmm_meta.txt
#' @description Compare HMM files against those listed in hmm_meta.txt.
#' @param hmm_list A [base::list()] of HMMs (i.e. as read in by
#' [multi_read_hmm()] or [multi_read_hmm_header()]).
#' @param hmm_meta A padloc-db HMM metadata file (i.e. as read in by
#' [read_hmm_meta()]).
#' @return a [base::list()] of three [tibble::tibble()]s.
#' @export
compare_hmm_files_hmm_meta <- function(hmm_list, hmm_meta) {
  hmm_files <- tibble::tibble(
    # The 'NAME' field is important here, as it is what eventually ends up
    # identifying the HMM in the HMMER output, which gets tied back to the
    # metadata in hmm_meta.txt
    hmm.name = sapply(hmm_list, function(x) x[["header"]][["NAME"]])
  )
  out <- a_vs_b(hmm_files, hmm_meta, join_by = "hmm.name")
  out
}

# TODO: Write an example.
# TODO: Write a test.
#' @title Compare system model files against those listed in sys_meta.txt
#' @description Compare system model files against those listed in hmm_meta.txt.
#' @param sys_list A [base::list()] of system models (i.e. as read in by
#' [multi_read_padloc_model()].
#' @param sys_meta A padloc-db system metadata file (i.e. as read in by
#' [read_sys_meta()]).
#' @return a [base::list()] of three [tibble::tibble()]s.
#' @export
compare_sys_files_sys_meta <- function(sys_list, sys_meta) {
  sys_files <- tibble::tibble(yaml.name = names(sys_list))
  out <- a_vs_b(sys_files, sys_meta, join_by = "yaml.name")
  out
}

# TODO: Write an example.
# TODO: Write a test.
#' @title Compare system model files against those listed in sys_meta.txt
#' @description Compare system model files against those listed in hmm_meta.txt.
#' @param sys_meta A padloc-db system metadata file (i.e. as read in by
#' [read_sys_meta()]).
#' @param sys_groups A padloc-db system groups metadata file (i.e. as read in
#' by [read_sys_groups()]).
#' @return a [base::list()] of three [tibble::tibble()]s.
#' @export
compare_sys_meta_sys_groups <- function(sys_meta, sys_groups) {
  out <- a_vs_b(sys_meta, sys_groups, join_by = "yaml.name")
  out
}

# TODO: Write an example.
# TODO: Write a test.
#' @title Generate a validity report for a list of padloc models
#' @description Generate a validity report for a list of padloc models.
#' @param models A [base::list()] of system models (i.e. as read in by
#' [multi_read_padloc_model()].
#' @return A [purrr::safely()] style [base::list()] with components `result`
#' and `error`. If the model is valid, `result` is `TRUE` and `error` is `NULL`.
#' If the model is invalid, `result` is `FALSE` and `error` is an `error` object
#' that describes the reason the model is invalid.
#' @export
report_padloc_model_validity <- function(models) {
  # Create safe version of function that stores error info
  possibly_valid_padloc_model_expanded <- purrr::safely(valid_padloc_model_expanded)
  # Run across all elements
  x <- purrr::map(models, function(x) possibly_valid_padloc_model_expanded(x))
  # Turn list inside out
  x <- purrr::list_transpose(x)
  # Replace NULL result values with FALSE
  x[["result"]][sapply(x[["result"]], is.null)] <- FALSE
  x <- purrr::list_transpose(x)
  # Clean up error logging by removing unecessary info
  x <- lapply(
    X = x,
    FUN = function(i) {
      if (is.list(i[["error"]])) {
        i[["error"]][["trace"]] <- NULL
        i[["error"]][["call"]] <- NULL
        i
      } else {
        i
      }
    }
  )
  x
}

# TODO: Write an example.
# TODO: Write a test.
#' @title Determine why padloc models are invalid
#' @description Determine why padloc models are invalid.
#' @param validity_report A validity report as generated by
#' [report_padloc_model_validity()].
#' @return A named list of invalid models and the associated error.
#' @export
why_invalid <- function(validity_report) {
  x <- purrr::list_transpose(validity_report)
  Filter(Negate(is.null), x[["error"]])
}

# TODO: Write functions to separate padloc-db based on system groups

# TODO: Write an example.
# TODO: Write a test.
filter_models <- function(padloc_models, sys_groups, group_name) {
  relevant_systems <- dplyr::filter(sys_groups, group %in% group_name)
  system_names <- relevant_systems[["yaml.name"]]
  which_models <- which(names(padloc_models) %in% system_names)
  filtered_models <- padloc_models[which_models]
  filtered_models
}



# TODO: Assess amount of overlap between system groups
