# system_cp_files <- function(file_list, extension, path, new_path) {
#   fs::dir_create(new_path)
#   purrr::walk(
#     .x = gene_list,
#     .f = function(x) {
#       file_path <- fs::path(path, paste0(x, extension))
#       file_new_path <- fs::path(new_path, paste0(x, extension))
#       fs::file_copy(
#         path = file_path,
#         new_path = file_new_path
#       )
#     }
#   )
# }
#
# system_cp_hmm <- function(hmm_list, path, new_path) {
#   system_cp_files(hmm_list, ".hmm", path, new_path)
# }
#
# system_cp_sys <- function(sys_list, path, new_path) {
#   system_cp_files(sys_list, ".yaml", path, new_path)
# }
#
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
