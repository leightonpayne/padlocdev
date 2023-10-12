#' Convert a short doi to the corresponding long doi
#' @param short_doi A short doi in the form '10/xxxx'.
#' @return A long doi.
#' @export
#' @examples
#' doi_to_long("10/gzgh")
doi_to_long <- function(short_doi) {
  cmd <- paste0("curl -si https://doi.org/", short_doi)
  curl_response <- system(cmd, intern = TRUE)
  out <- curl_response %>%
    stringr::str_extract("(?<=location: ).*") %>%
    stringr::str_flatten(na.rm = TRUE) %>%
    stringr::str_remove("https://doi.org/")
  out
}

#' Convert a long doi to the corresponding short doi
#' @param long_doi A long doi in the form '10.1093/NAR/GKT1419'.
#' @return A short doi.
#' @export
#' @examples
#' doi_to_short("10.1093/NAR/GKT1419")
doi_to_short <- function(long_doi) {
  cmd <- paste0("curl -si https://shortdoi.org/", long_doi)
  curl_response <- system(cmd, intern = TRUE)
  out <- curl_response %>%
    stringr::str_extract("(?<=, and doi:)10/\\w*") %>%
    stringr::str_flatten(na.rm = TRUE)
  out
}

#' Convert a dataframe of short dois to long dois
#' @param df A data frame containing a column with short dois
#' @param col The name of the column containing short dois
#' @return A data frame with a new column containing long dois
#' @export
doi_to_long_df <- function(df, col = doi_short) {
  out <- dplyr::mutate(
    df,
    doi_long = purrr::map_chr({{col}}, ~ doi_to_long(.x), .progress = TRUE)
  )
  out
}
