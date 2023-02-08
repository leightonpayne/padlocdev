#' @title Expand HMM metadata table multi-name assignments
#' @description Expand HMM metadata table multi-name assignments.
#' @param hmm_meta An HMM metadata table (as read-in by [read_hmm_meta()]).
#' @return An HMM metadata table with expanded name assignments.
#' @export
#' @examples
#' path <- padlocdev_example("padloc-db/hmm_meta.txt")
#' hmm_meta <- read_hmm_meta(path)
#' hmm_meta_expanded <- expand_hmm_meta(hmm_meta)
#' hmm_meta_expanded
expand_hmm_meta <- function(hmm_meta) {
  # Columns that are allowed to have multiple values assigned
  multi_assigments <- c("protein.name", "secondary.name")
  # Split multi-assignments (splits at '|', including any surrounding space)
  out <- tidyr::separate_rows(
    hmm_meta,
    dplyr::all_of(multi_assigments),
    sep = "\\s*\\|\\s*"
  )
  out
}

#' @title Collapse HMM metadata table multi-name assignments
#' @description Collapse HMM metadata table multi-name assignments.
#' @param hmm_meta_expanded An expanded HMM metadata table (as read-in by
#' [read_hmm_meta()]) and expanded by [expand_hmm_meta()].
#' @return An HMM metadata table with collasped name assignments.
#' @export
#' @examples
#' path <- padlocdev_example("padloc-db/hmm_meta.txt")
#' hmm_meta <- read_hmm_meta(path)
#' hmm_meta_expanded <- expand_hmm_meta(hmm_meta)
#' hmm_meta_collapsed <- collapse_hmm_meta(hmm_meta_expanded)
#' hmm_meta_collapsed
collapse_hmm_meta <- function(hmm_meta_expanded) {
  hmm_meta_grouped <- dplyr::group_by(hmm_meta_expanded, hmm.acc)
  hmm_meta_collapsed <- dplyr::mutate(
    hmm_meta_grouped,
    secondary.name = paste0(unique(secondary.name), collapse = "|"),
    protein.name = paste0(unique(protein.name), collapse = "|")
  )
  hmm_meta_collapsed <- dplyr::ungroup(hmm_meta_collapsed)
  hmm_meta_collapsed_distinct <- dplyr::distinct(hmm_meta_collapsed)
  hmm_meta_collapsed_distinct
}
