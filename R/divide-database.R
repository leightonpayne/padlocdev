# TODO: Write an example.
# TODO: Write a test.
# TODO: Write documentation.
#' @export
filter_models <- function(sys_list, sys_groups, group_name) {
  relevant_systems <- dplyr::filter(sys_groups, group %in% group_name)
  system_names <- relevant_systems[["yaml.name"]]
  which_models <- which(names(sys_list) %in% system_names)
  filtered_models <- sys_list[which_models]
  filtered_models
}

#' @export
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
#' @export
filter_sys_meta <- function(sys_list, sys_meta) {
  relevant_systems <- names(sys_list)
  relevant_meta <- dplyr::filter(sys_meta, yaml.name %in% relevant_systems)
}

#' @export
group_models <- function(models, sys_groups) {
  # make sure the list of models and the groups table are in the same order by
  # using base::sort(method = "radix") and
  # dplyr::arrange(method = "radix") or dplyr::arrange(.locale = "C")
  # to enforce consistent "C"-style collation.
  # i.e. c("A", "b", "C", "d") >> c("A", "C", "b", "d")
  models_grouped <- split(
    models[sort(names(models), method = "radix")],
    dplyr::arrange(sys_groups, yaml.name, method = "radix")$group
  )
  models_grouped
}

#' @export
group_overlap <- function(sys_expanded, sys_groups, hmm_meta_expanded, sys_meta) {

  # subset for gene elements
  a <- lapply(sys_expanded, function(x) x[c("core_genes", "optional_genes", "prohibited_genes")])
  # put the models into groups
  b <- group_models(a, sys_groups)
  # unlist gene lists
  c <- lapply(b, function(x) unlist(x, use.names = FALSE))
  # remove "NA" genes
  d <- lapply(c, function(x) x[!x %in% "NA"])
  # filter for unique genes
  e <- lapply(d, function(x) unique(x))

  ncomb <- ncomb(length(e), 2)
  out <- tibble::tibble()

  for (i in 1:(length(e)-1)) {
    for (j in (i+1):length(e)) {
      g1 <- names(e[i])
      g2 <- names(e[j])
      intersect <- intersect(e[[i]], e[[j]])
      tobind <- tibble::tibble(group_1 = g1, group_2 = g2, intersect = list(intersect))
      out <- dplyr::bind_rows(out, tobind)
    }
  }

  # Filter for groups with overlap
  out <- out %>% dplyr::filter(!sapply(intersect, identical, character(0)))
  out

}

#' @export
pull_intersect <- function(group_overlap, group_1, group_2) {
  filt <- dplyr::filter(group_overlap, group_1 == group_1 & group_2 == group_2)
  out <- unlist(filt[["intersect"]])
  out
}

#' @export
divide_database <- function(sys_expanded, sys_groups, hmm_meta_expanded, sys_meta, path, new_path) {
  groups <- unique(sys_groups[["group"]])

  cli::cli_progress_bar("Dividing database into groups", total = length(groups) * 4)

  for (group in groups) {

    # cli::cli_alert_info("Working on group {.val {group}}")

    group_dir <- fs::path_join(c(new_path, group))
    fs::dir_create(group_dir)

    relevant_sys <- filter_models(sys_expanded, sys_groups, group)

    relevant_sys_meta <- filter_sys_meta(relevant_sys, sys_meta)
    new_sys_meta_path <- fs::path_join(c(group_dir, "sys_meta.txt"))
    readr::write_tsv(relevant_sys_meta, new_sys_meta_path, progress = FALSE)

    cli::cli_progress_update()

    relevant_sys_files <- names(relevant_sys)

    old_sys_dir <- fs::path_join(c(path, "sys"))
    new_sys_dir <- fs::path_join(c(group_dir, "sys"))
    system_cp_sys(relevant_sys_files, old_sys_dir, new_sys_dir)

    cli::cli_progress_update()

    relevant_hmm_meta <- filter_hmm_meta(relevant_sys, hmm_meta_expanded)
    relevant_hmm_meta_collapsed <- collapse_hmm_meta(relevant_hmm_meta)
    new_hmm_meta_path <- fs::path_join(c(group_dir, "hmm_meta.txt"))
    readr::write_tsv(relevant_hmm_meta_collapsed, new_hmm_meta_path, progress = FALSE)

    cli::cli_progress_update()

    relevant_hmm_files <- unique(relevant_hmm_meta[["hmm.acc"]])

    old_hmm_dir <- fs::path_join(c(path, "hmm"))
    new_hmm_dir <- fs::path_join(c(group_dir, "hmm"))
    system_cp_hmm(relevant_hmm_files, old_hmm_dir, new_hmm_dir)

    cli::cli_progress_update()

  }
}

#' @export
summarise_groups <- function(sys_expanded, sys_groups) {
  # group n_models n_hmms
  a <- lapply(sys_expanded, function(x) x[c("core_genes", "optional_genes", "prohibited_genes")])
  b <- group_models(a, sys_groups)

  c <- purrr::map(.x = b, .f = function(x) length(x))
  d <- purrr::map(.x = b, .f = function(x) length(unlist(x)))

  out <- tibble::tibble(
    system = names(c),
    n_models = unlist(c),
    n_hmms = unlist(d)
  )

  out

}
