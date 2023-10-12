# TODO: Write a test.
#' @title Expand gene groups
#' @description Expand secondary gene assignments in a specific category of
#' genes into their underlying genes.
#' @param model A padloc model.
#' @param gene_type The category of genes to expand, i.e. one of: "core_genes",
#' "secondary_genes", or "prohibited_genes".
#' @param hmm_meta A padloc-db HMM metadata table.
#' @return A [base::character()] vector containing the expanded gene values.
#' @export
#' @examples
#' # Read data -----------------------------------------------------------------
#' path <- padlocdev_example("padloc-db/sys/DRT_class_I.yaml")
#' model <- read_padloc_model(path)
#' model
#' path <- padlocdev_example("padloc-db/hmm_meta.txt") # Generate path for example model
#' hmm_meta <- read_hmm_meta(path)
#' hmm_meta
#' # Expand genes --------------------------------------------------------------
#' # The category "secondary_genes" only contains the gene group "Sya_accessory"
#' model$secondary_genes
#' # After expanding, "Sya_accessory" becomes "SyaB" and "SyaC"
#' model$secondary_genes <- expand_gene_groups(model, "secondary_genes", hmm_meta)
#' model$secondary_genes
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
#' model <- multi_read_padloc_model(padlocdev_example("padloc-db/sys"))
#' hmm_meta <- read_hmm_meta(padlocdev_example("padloc-db/hmm_meta.txt"))
expand_gene_groups_all <- function(models, hmm_meta) {
  gene_groups <- c("core_genes", "secondary_genes", "neutral_genes", "prohibited_genes")
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
