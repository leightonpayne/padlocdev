# TODO: Write an example.
# TODO: Write a test.
# TODO: Write documentation.
# The name of each profile HMM in a list of HMMs read-in by `multi_read_hmms()`
# is derived from the file name. This function checks whether the file names
# match the NAME field of the HMM, as this becomes important when splitting the
# HMMs into groups.
#' @title verify_hmm_names
#' @description verify_hmm_names
#' @param hmm_list ...
#' @export
verify_hmm_names <- function(hmm_list) {
  hmm_list_transposed <- purrr::list_transpose(hmm_list)
  hmm_headers <- hmm_list_transposed[["header"]]
  headers_transposed <- purrr::list_transpose(hmm_headers)
  hmm_accessions <- unlist(headers_transposed[["ACC"]])
  if (length(hmm_accessions) != length(hmm_list)) {
    cli::cli_abort(c("Some HMMs do not have accessions.",
                     "x" = "Make sure all HMMs have an ACC field."))
  }
  x <- tibble::tibble(
    file_name = names(hmm_list),
    accession_field = hmm_accessions
  )
  x <- dplyr::mutate(x, match = ifelse(file_name == accession_field, TRUE, FALSE))
  out <- list(
    names_match = dplyr::filter(x, match == TRUE),
    names_mismatch = dplyr::filter(x, match == FALSE)
  )
  out
}

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

# TODO: Write an example.
# TODO: Write a test.
#' @title Pull the genes from a list of system models
#' @description Pull the genes from a list of system models.
#' @param sys_list A [base::list()] of system models (i.e. as read in by
#' [multi_read_padloc_model()].
#' @return a [base::character()] vector.
#' @export
pull_genes <- function(sys_list) {
  a <- purrr::list_transpose(sys_list)
  b <- a[c("core_genes", "secondary_genes", "neutral_genes", "prohibited_genes")]
  c <- unlist(b, use.names = FALSE)
  d <- unique(c)
  e <- d[d != "NA"]
  e
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
  # The 'NAME' field is important here, as it is what eventually ends up
  # identifying the HMM in the HMMER output, which gets tied back to the
  # metadata in hmm_meta.txt
  hmm_names <- sapply(hmm_list, function(x) x[["header"]][["NAME"]])
  hmm_files <- tibble::tibble(
    hmm.name = hmm_names
  )

  out <- a_vs_b(hmm_files, hmm_meta, join_by = "hmm.name")

  extra_hmms <- purrr::pluck(out, "hmm_files without hmm_meta", "hmm.name")
  if (!is.null(extra_hmms)) {
    headers <- purrr::list_transpose(hmms)[["header"]]
    headers_filtered <- Filter(function(x) x$NAME %in% extra_hmms, headers)
    transposed <- purrr::list_transpose(headers_filtered)
    tibble <- tibble::tibble(
      file.name = names(headers_filtered),
      NAME = transposed[["NAME"]],
      ACC = transposed[["ACC"]],
      hmm_files = TRUE
    )
    out[["hmm_files without hmm_meta"]] <- tibble
  }

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
#' @title Compare system model files against the hmm metadata table.
#' @description Compare system model files against those listed in hmm_meta.txt.
#' @param models_expanded A [base::list()] of padloc-db system models (i.e. as
#' read in by [multi_read_padloc_model()]).
#' @param hmm_meta_expanded A padloc-db HMM metadata file (i.e. as read in
#' by [read_hmm_meta()]).
#' @return A [base::list()] of three [tibble::tibble()]s.
#' @export
compare_sys_files_hmm_meta <- function(models_expanded, hmm_meta_expanded) {
  sys_genes <- pull_genes(models_expanded)
  sys_genes <- tibble::tibble(protein.name = sys_genes)
  hmm_meta_expanded <- dplyr::distinct(hmm_meta_expanded, protein.name)
  out <- a_vs_b(hmm_meta_expanded, sys_genes, join_by = "protein.name")
  out
}

# TODO: Write an example.
# TODO: Write a test.
#' @title Find out which system models use particular proteins
#' @description Find out which system models use particular proteins.
#' @param genes Character aray of gene names.
#' @param models_expanded List of expanded models.
#' @return A list of system models.
#' @export
which_uses <- function(genes, models_expanded) {
  # turn inside out
  a <- purrr::list_transpose(models_expanded, simplify = FALSE)
  # subset for for gene categories
  b <- a[c("core_genes", "secondary_genes", "neutral_genes", "prohibited_genes")]
  # turn back
  c <- purrr::list_transpose(b, simplify = FALSE)
  # unlist genes so there's one element per system
  d <- purrr::map(c, function(x) unlist(x, use.names = FALSE))
  # for each system, subset genes for those we're interested in (use ^...$ for exact match)
  e <- sapply(d, function(x) stringr::str_subset(x, paste0("^", genes, "$", collapse = "|")))
  # filter for systems that have those genes
  f <- Filter(function(x) length(x) != 0, e)
  # convert list to tibble
  g <- tibble::as_tibble(utils::stack(f))
  # convert factors to character
  h <- dplyr::mutate(g, ind = as.character(ind))
  # split back into a list, where gene is element name and system is values
  i <- split(h$ind, h$values)
  i
}




