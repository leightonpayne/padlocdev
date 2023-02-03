# Convenience wrappers for performing common padloc-db checks
# ------------------------------------------------------------------------------

# TODO: Check all hmms are listed in hmm_meta.txt and vice versa

#' @title Compare HMM files against those listed in hmm_meta.txt
#' @description Compare HMM files against those listed in hmm_meta.txt.
#' @param hmm_list A [base::list()] of HMMs (i.e. as read in by
#' [multi_read_hmm()] or [multi_read_hmm_header()]).
#' @param hmm_meta A padloc-db HMM metadata file (i.e. as read in by
#' [read_hmm_meta()]).
#' @return a [base::list()] with two [tibble::tibble()](s),
#' "HMMs without metadata" - listing HMMs that are not referenced in the
#' HMM metadata file, and "Metadata without HMMs" - listing HMMs that are
#' referenced in the HMM metadata file, but do not actually exist.
#' @export
hmm_files_vs_hmm_meta <- function(hmm_list, hmm_meta) {

  hmm_details <- tibble::tibble(
    hmm.name = sapply(hmm_list, function(x) x[["header"]][["NAME"]]),
    hmm.files = TRUE
  )

  hmm_meta_compare <- dplyr::select(hmm_meta, "hmm.name")
  hmm_meta_compare <- dplyr::mutate(hmm_meta_compare, "hmm.meta" = TRUE)

  check <- dplyr::full_join(hmm_meta_compare, hmm_details, by = "hmm.name")

  # Instances where an HMM exists, but is not referenced in hmm_meta.txt
  hmm_wo_meta <- dplyr::filter(check, is.na("hmm.meta"))
  # Instances where an HMM is referenced in hmm_meta.txt, but does not exist
  meta_wo_hmm <- dplyr::filter(check, is.na("hmm.files"))
  # Instances where referencing is complete
  hmm_w_meta <- dplyr::filter(check, "hmm.meta" == TRUE & "hmm.files" == TRUE)

  list(
    "HMMs without metadata" = hmm_wo_meta,
    "Metadata without HMMs" = meta_wo_hmm,
    "HMMs with metadata" = hmm_w_meta
    )

}

# TODO: Check all systems are listed in sys_meta.txt and vice versa



# TODO: Check all systems have been assigned a group

# TODO: Check all models are valid

# TODO: Separate padloc-db based on system groups

# TODO: Assess amount of overlap between system groups
