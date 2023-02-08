# TODO: Write an example.
# TODO: Write a test.
# TODO: Write documentation.
filter_models <- function(sys_list, sys_groups, group_name) {
  relevant_systems <- dplyr::filter(sys_groups, group %in% group_name)
  system_names <- relevant_systems[["yaml.name"]]
  which_models <- which(names(sys_list) %in% system_names)
  filtered_models <- sys_list[which_models]
  filtered_models
}

filter_hmm_meta <- function(sys_list, hmm_meta) {
  transposed_models <- purrr::list_transpose(sys_list)
  gene_categories <- c("core_genes", "optional_genes", "prohibited_genes")
  models_sublist <- transposed_models[gene_categories]
  raw_genes <- unlist(models_sublist, use.names = FALSE)
  unique_genes <- unique(raw_genes)
  unique_genes <- unique_genes[unique_genes != "NA"]
  relevant_meta <- dplyr::filter(hmm_meta, protein.name %in% unique_genes)
  relevant_meta
}

# This is pretty basic, and would probably make more sense to use the system
# sys groups table over a list of system models, but it's set up this way
# for consistency w/ filter_hmm_meta i.e. (models, meta)
filter_sys_meta <- function(sys_list, sys_meta) {
  relevant_systems <- names(sys_list)
  relevant_meta <- dplyr::filter(sys_meta, yaml.name %in% relevant_systems)
}

divide_database <- function(sys_expanded, sys_groups, hmm_meta_expanded, sys_meta, path, new_path) {
  groups <- unique(sys_groups[["group"]])

  cli::cli_progress_bar("Dividing database into groups", total = length(groups) * 4)

  for (group in groups) {

    # cli::cli_alert_info("Working on group {.val {group}}")

    group_dir <- fs::path_join(c(new_path, group))
    fs::dir_create(group_dir)

    relevant_sys <- filter_models(sys_list, sys_groups, group)

    relevant_sys_meta <- filter_sys_meta(relevant_sys, sys_meta)
    new_sys_meta_path <- fs::path_join(c(group_dir, "sys_meta.txt"))
    readr::write_tsv(relevant_sys_meta, new_sys_meta_path)

    cli::cli_progress_update()

    relevant_sys_files <- names(relevant_sys)

    old_sys_dir <- fs::path_join(c(path, "sys"))
    new_sys_dir <- fs::path_join(c(group_dir, "sys"))
    system_cp_sys(relevant_sys_files, old_sys_dir, new_sys_dir)

    cli::cli_progress_update()

    relevant_hmm_meta <- filter_hmm_meta(relevant_sys, hmm_meta_expanded)
    relevant_hmm_meta_collapsed <- collapse_hmm_meta(relevant_hmm_meta)
    new_hmm_meta_path <- fs::path_join(c(group_dir, "hmm_meta.txt"))
    readr::write_tsv(relevant_hmm_meta_collapsed, new_hmm_meta_path)

    cli::cli_progress_update()

    relevant_hmm_files <- unique(relevant_hmm_meta[["hmm.acc"]])

    old_hmm_dir <- fs::path_join(c(path, "hmm"))
    new_hmm_dir <- fs::path_join(c(group_dir, "hmm"))
    system_cp_hmm(relevant_hmm_files, old_hmm_dir, new_hmm_dir)

    cli::cli_progress_update()

  }
}
