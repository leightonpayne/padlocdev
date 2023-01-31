padloc_model <- function(maximum_separation, minimum_core, minimum_total, core_genes, optional_genes, prohibited_genes) {
  object <- list(
    maximum_separation = maximum_separation,
    minimum_core = minimum_core,
    minimum_total = minimum_total,
    core_genes = core_genes,
    optional_genes = optional_genes,
    prohibited_genes = prohibited_genes
  )
  valid_padloc_model(object)
  object
}

valid_padloc_model <- function(object) {
  errors <- ""

  quorum_param <- c("maximum_separation", "minimum_core", "minimum_total")
  # Check that quorum parameters are all length 1
  vapply(quorum_param, function(x) length(x) == 1, logical(1))
  # Check that quorum parameters are all numeric
  vapply(quorum_param, function(x) is.numeric(object[[x]]), logical(1))

  gene_param <- c("core_genes", "optional_genes", "prohibited_genes")
  # Check that gene parameters are character
  vapply(gene_param, function(x) is.character(object[[x]]), logical(1))

  # THESE CHECKS ARE TURNED OFF UNTIL GENE GROUP ASSIGNMENTS ARE RESOLVED
  # if (object$minimum_core > length(object$core_genes)) {
  #   errors <- paste0(
  #     errors,
  #     "\nminimum_core must be <= the length of core_genes ",
  #     "(minimum_core = ", object$minimum_core, ", ",
  #     "but length(core_genes) = ", length(object$core_genes), ")"
  #   )
  # }
  #
  # if (object$minimum_total > length(object$core_genes) + length(object$optional_genes)) {
  #   errors <- paste0(
  #     errors,
  #     "\nminimum_total must be <= the length of core_genes + optional_genes ",
  #     "(minimum_total = ", object$minimum_total, ", ",
  #     "but length(core_genes) + length(optional_genes) = ", length(object$core_genes) + length(object$optional_genes), ")"
  #   )
  # }

  if (errors != "") {
    return(errors)
  } else {
    return(TRUE)
  }
}
