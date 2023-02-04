padloc_model <- function(maximum_separation, minimum_core, minimum_total, core_genes, optional_genes, prohibited_genes) {
  object <- list(
    maximum_separation = maximum_separation,
    minimum_core = minimum_core,
    minimum_total = minimum_total,
    core_genes = core_genes,
    optional_genes = optional_genes,
    prohibited_genes = prohibited_genes
  )
  valid_padloc_model_basic(object)
  object
}

valid_padloc_model_basic <- function(object) {
  errors <- ""

  quorum_param <- c("maximum_separation", "minimum_core", "minimum_total")
  # Check that quorum parameters are all length 1
  vapply(quorum_param, function(x) length(x) == 1, logical(1))
  # Check that quorum parameters are all numeric
  vapply(quorum_param, function(x) is.numeric(object[[x]]), logical(1))

  gene_param <- c("core_genes", "optional_genes", "prohibited_genes")
  # Check that gene parameters are character
  vapply(gene_param, function(x) is.character(object[[x]]), logical(1))

  if (errors != "") {
    return(errors)
  } else {
    return(TRUE)
  }
}

# valid_padloc_model_expanded <- function(object) {
#   if (object$minimum_core > length(object$core_genes)) {
#     cli::cli_abort(c(
#       "!" = "minimum_core must be <= the length of core_genes",
#       "i" = "minimum_core is {object$minimum_core}",
#       "x" = "Number of core_genes is {length(object$core_genes)}"
#     ))
#   }
#   if (object$minimum_total > length(object$core_genes) + length(object$optional_genes)) {
#     cli::cli_abort(c(
#       "!" = "minimum_total must be <= the number of core_genes + optional_genes",
#       "i" = "minimum_total is {object$minimum_total}",
#       "x" = "Number of core_genes + optional_genes is {length(object$core_genes) + length(object$optional_genes)}"
#     ))
#   }
# }

valid_padloc_model_expanded <- function(object) {
  error_message <- c()
  if (object$minimum_core > length(object$core_genes)) {
    error_message <- c(
      error_message,
      "!" = "minimum_core must be <= the length of core_genes",
      "i" = "minimum_core is {object$minimum_core}",
      "x" = "Number of core_genes is {length(object$core_genes)}"
      )
  }
  if (object$minimum_total > length(object$core_genes) + length(object$optional_genes)) {
    error_message <- c(
      error_message,
      "!" = "minimum_total must be <= the number of core_genes + optional_genes",
      "i" = "minimum_total is {object$minimum_total}",
      "x" = "Number of core_genes + optional_genes is {length(object$core_genes) + length(object$optional_genes)}"
    )
  }
  if (length(error_message) > 0) {
    cli::cli_abort(error_message)
  } else {
    return(TRUE)
  }
}


