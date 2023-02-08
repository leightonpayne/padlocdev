#' @title Padloc model
#' @description A list of all the parameters that make up a padloc model.
#' @param maximum_separation A [base::integer()], representing the maximum
#' number of unrelated genes that can seperate the genes of a system.
#' @param minimum_core A [base::integer()], representing the minimum number
#' of core genes that must be identified for a complete system.
#' @param minimum_total A [base::integer()], representing the minimum number
#' of total genes that must be identified for a complete system.
#' @param core_genes A [base::character()], listing the names of the core genes
#' that contribute to system completeness.
#' @param optional_genes A [base::character()], listing the names of the
#' optional genes that contribute to system completeness.
#' @param prohibited_genes A [base::character()], listing the names of the
#' prohibited genes that cannot be identified in proximity to the system.
#' @return A [base::list()].
#' @export
#' @examples
#' model <- padloc_model(
#'   maximum_separation = 3,
#'   minimum_core = 2,
#'   minimum_total = 3,
#'   core_genes = c("GenA", "GenB"),
#'   optional_genes = c("GenC", "GenD"),
#'   prohibited_genes = "NA"
#' )
padloc_model <- function(maximum_separation, minimum_core, minimum_total, core_genes, optional_genes, prohibited_genes) {

  object <- list(
    maximum_separation = maximum_separation,
    minimum_core = minimum_core,
    minimum_total = minimum_total,
    core_genes = core_genes,
    optional_genes = optional_genes,
    prohibited_genes = prohibited_genes
  )

  # valid_padloc_model_basic(object)

  object

}

#' @title Basic validity checker for a padloc model
#' @description Basic validity checker for a padloc model.
#' @param padloc_model A padloc model.
#' @return `TRUE` if the model is valid, else an `error` object.
#' @export
valid_padloc_model_basic <- function(padloc_model) {

  errors <- c()

  quorum_parameters <- c("maximum_separation", "minimum_core", "minimum_total")
  quorum_values <- padloc_model[quorum_parameters]
  gene_parameters <- c("core_genes", "optional_genes", "prohibited_genes")
  gene_values <- padloc_model[gene_parameters]

  # Check that quorum parameters are all whole numbers.
  quorum_values_typeof <- map_typeof(quorum_values)
  quorum_values_is_whole <- map_is_whole(quorum_values)
  quorum_values_length <- map_length(quorum_values)
  for (i in seq_along(quorum_parameters)) {
    if (quorum_values_is_whole[i] == FALSE) {
      errors <- c(errors, cli::format_message(c(
        "i" = "{.var {quorum_parameters[i]}} must be a whole number of type {.emph integer} or {.emph double}.",
        "x" = "Instead, got {.emph {quorum_values_typeof[i]}}: {.val {quorum_values[i]}}."
      )))
    }
  }

  # Check that quorum parameters are all length 1.
  for (i in seq_along(quorum_parameters)) {
    if (quorum_values_length[i] > 1) {
      errors <- c(errors, cli::format_message(c(
        "i" = "{.var {quorum_parameters[i]}} can only contain one value.",
        "x" = "Instead, got {.val {quorum_values_length[i]}} values: {.val {quorum_values[i]}}."
      )))
    }
  }

  # Check that gene parameters are all character vectors.
  gene_values_typeof <- map_typeof(gene_values)
  for (i in seq_along(gene_parameters)) {
    if (gene_values_typeof[i] != "character") {
      errors <- c(errors, cli::format_message(c(
        "i" = "{.var {gene_parameters[i]}} must be of type {.emph character}.",
        "x" = "Instead, got {.emph {gene_values_typeof[i]}}: {.val {gene_values[[i]]}}."
      )))
    }
  }

  # Check that core_genes does not contain NA.
  if(any(padloc_model[["core_genes"]] == "NA")) {
    errors <- c(errors, cli::format_message(c(
      "i" = "{.var core_genes} can not contain the special value {.val NA}.",
      "x" = "Got {.val {padloc_model[[\"core_genes\"]]}}."
    )))
  }

  # If NA is specified in optional or prohibited genes, check that there are no
  # additional genes listed.
  for (gene_category in c("optional_genes", "prohibited_genes")) {
    if ("NA" %in% padloc_model[[gene_category]] && length(padloc_model[[gene_category]]) > 1) {
      errors <- c(errors, cli::format_message(c(
        "i" = "{.var {gene_category}} can not contain the special value {.val NA} if additional genes are specified.",
        "x" = "Got {.val {padloc_model[[{gene_category}]]}}."
        )))
    }
  }

  # Return errors
  if (length(errors) > 0) {
    errors <- unlist(strsplit(errors, "\n"))
    cli::cli_abort(c("Invalid padloc system model:", errors))
  } else {
    return(TRUE)
  }

}

#' @title Validity checker for a padloc model with expanded genes
#' @description Validity checker for a padloc model with expanded genes.
#' @param padloc_model A padloc model.
#' @return `TRUE` if the model is valid, else an `error` object.
#' @export
valid_padloc_model_expanded <- function(padloc_model) {

  errors <- c()

  quorum_parameters <- c("maximum_separation", "minimum_core", "minimum_total")
  quorum_values <- padloc_model[quorum_parameters]
  gene_parameters <- c("core_genes", "optional_genes", "prohibited_genes")
  gene_values <- padloc_model[gene_parameters]

  # Check that quorum parameters are all whole numbers.
  quorum_values_typeof <- map_typeof(quorum_values)
  quorum_values_is_whole <- map_is_whole(quorum_values)
  quorum_values_length <- map_length(quorum_values)
  for (i in seq_along(quorum_parameters)) {
    if (quorum_values_is_whole[i] == FALSE) {
      errors <- c(errors, cli::format_message(c(
        "i" = "{.var {quorum_parameters[i]}} must be a whole number of type {.emph integer} or {.emph double}.",
        "x" = "Instead, got {.emph {quorum_values_typeof[i]}}: {.val {quorum_values[i]}}."
      )))
    }
  }

  # Check that quorum parameters are all length 1.
  for (i in seq_along(quorum_parameters)) {
    if (quorum_values_length[i] > 1) {
      errors <- c(errors, cli::format_message(c(
        "i" = "{.var {quorum_parameters[i]}} can only contain one value.",
        "x" = "Instead, got {.val {quorum_values_length[i]}} values: {.val {quorum_values[i]}}."
      )))
    }
  }

  # Check that gene parameters are all character vectors.
  gene_values_typeof <- map_typeof(gene_values)
  for (i in seq_along(gene_parameters)) {
    if (gene_values_typeof[i] != "character") {
      errors <- c(errors, cli::format_message(c(
        "i" = "{.var {gene_parameters[i]}} must be of type {.emph character}.",
        "x" = "Instead, got {.emph {gene_values_typeof[i]}}: {.val {gene_values[[i]]}}."
      )))
    }
  }

  # Check that core_genes does not contain NA.
  if(any(padloc_model[["core_genes"]] == "NA")) {
    errors <- c(errors, cli::format_message(c(
      "i" = "{.var core_genes} can not contain the special value {.val NA}.",
      "x" = "Got {.val {padloc_model[[\"core_genes\"]]}}."
    )))
  }

  # If NA is specified in optional or prohibited genes, check that there are no
  # additional genes listed.
  for (gene_category in c("optional_genes", "prohibited_genes")) {
    if ("NA" %in% padloc_model[[gene_category]] && length(padloc_model[[gene_category]]) > 1) {
      errors <- c(errors, cli::format_message(c(
        "i" = "{.var {gene_category}} can not contain the special value {.val NA} if additional genes are specified.",
        "x" = "Got {.val {padloc_model[[{gene_category}]]}}."
      )))
    }
  }

  gene_parameters <- c("core_genes", "optional_genes", "prohibited_genes")

  # Check that minimum_core is valid
  core_genes_length <- length(subset(padloc_model$core_genes, padloc_model$core_genes != "NA"))
  if (padloc_model$minimum_core > core_genes_length) {
    errors <- c(errors, cli::format_message(c(
      "i" = "{.var minimum_core} must be <= the number of {.var core_genes}",
      "x" = "{.var minimum_core} is {.val {padloc_model$minimum_core}}, but number of {.var core_genes} is {.val {length(padloc_model$core_genes)}}"
    )))
  }

  optional_genes_length <- length(subset(padloc_model$optional_genes, padloc_model$optional_genes != "NA"))
  # Check that minimum_total is valid
  if (padloc_model$minimum_total > core_genes_length + optional_genes_length) {
    errors <- c(errors, cli::format_message(c(
      "i" = "{.var minimum_total} must be <= the combined number of {.var core_genes} and {.var optional_genes}.",
      "x" = "{.var minimum_total} is {.val {padloc_model$minimum_total}}, but {.var core_genes} + {.var optional_genes} is {.val {core_genes_length + optional_genes_length}}"
    )))
  }

  # Check there are no genes duplicated within gene categories.
  duplicated_genes <- map_duplicated(padloc_model[gene_parameters])
  for (i in seq_along(gene_parameters)) {
    if (length(duplicated_genes[[i]] > 0)) {
      errors <- c(errors, cli::format_message(c(
        "i" = "{.var {gene_parameters[i]}} can not contain duplicate genes.",
        "x" = "Duplicate genes include: {.val {duplicated_genes[[i]]}}"
      )))
    }
  }

  # Check there are no overlapping genes between gene categories.
  for (i in 1:(length(gene_parameters)-1)) {
    genes_1 <- unlist(padloc_model[gene_parameters[i]])
    for (j in (i+1):length(gene_parameters)) {
      genes_2 <- unlist(padloc_model[gene_parameters[j]])
      overlap <- intersect(genes_1, genes_2)
      overlap <- overlap[overlap != "NA"]
      if (length(overlap) > 0) {
        errors <- c(errors, cli::format_message(c(
          "i" = "{.var {gene_parameters[i]}} should not overlap with {.var {gene_parameters[j]}}.",
          "x" = "Overlapping genes include: {.val {overlap}}"
        )))
      }
    }
  }

  # Return errors
  if (length(errors) > 0) {
    errors <- unlist(strsplit(errors, "\n"))
    cli::cli_abort(c("Invalid padloc system model:", errors))
  } else {
    return(TRUE)
  }

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

# # Convert validity report to tibble.
# validity_report_as_tibble <- function(validity_report) {
#   out <- tibble::tibble(
#     name = names(validity_report),
#     result = purrr::map_lgl(validity_report, "result"),
#     error = purrr::map(validity_report, "error")
#   )
#   out
# }
