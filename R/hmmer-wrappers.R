# proteinname_to_hmmaccession <- function(protein_names, hmm_meta) {
#
#   errors <- c()
#
#   a <- dplyr::filter(hmm_meta, protein.name %in% protein_names)
#   b <- dplyr::pull(a, hmm.acc)
#   b
#
# }
#
# models_group_2 <- filter_models(models_expanded, sys_groups, "group_2")
# genes_group_2 <- pull_genes(models_group_2)
#
#
# accessions_group_1 <- proteinname_to_hmmaccession(genes_group_2, hmm_meta)
#
# sys_meta <- read_sys_meta("~/tools/padloc-db/sys_meta.txt")
# hmm_meta <- read_hmm_meta("~/tools/padloc-db/hmm_meta.txt")
# hmm_headers <- multi_read_hmm_header("~/tools/padloc-db/hmm/")
# padloc_models <- multi_read_padloc_model("~/tools/padloc-db/sys/")
# padloc_models_expanded <- expand_gene_groups_all(padloc_models, hmm_meta)
#
#
