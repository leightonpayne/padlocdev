#' Get a table of crossref data.
#' @param df A data frame with a column containing long dois.
#' @param col The name of the column containing long dois.
#' @return A data frame.
#' @export
get_crossref_data <- function(df, col = doi_long) {
  doi_long <- dplyr::pull(df, {{col}})
  cr_works_list <- rcrossref::cr_works(doi_long, .progress = "time")
  cr_works_df <- cr_works_list$data
  cr_works_df_upper <- dplyr::mutate(cr_works_df, doi = stringr::str_to_upper(doi))
  out <- dplyr::left_join(df, cr_works_df_upper, by = dplyr::join_by({{col}} == doi))
}

crossref_data_clean_author <- function(crossref_data) {
  out <- crossref_data %>%
    dplyr::mutate(author = stringr::str_replace(author, "Ã©", "é"))
}

#' @export
crossref_data_pull_author <- function(crossref_data) {
  out <- dplyr::mutate(
    crossref_data,
    author = purrr::map_chr(
      author, ~ { dplyr::filter(., sequence == "first") %>% dplyr::pull(family) }
    )
  ) %>% crossref_data_clean_author()
  out
}

crossref_data_clean_date <- function(crossref_data) {
  out <- crossref_data %>%
    dplyr::mutate(
      date = dplyr::if_else(doi_short == "10/d24tcq", "1997", date)
    )
}

#' @export
crossref_data_pull_date <- function(crossref_data) {
  out <- crossref_data %>%
    dplyr::mutate(date = stringr::str_extract(issued, "[0-9]{4}")) %>%
    crossref_data_clean_date()
  out
}

crossref_data_clean_journal <- function(crossref_data) {
  out <- crossref_data %>%
    dplyr::mutate(journal = dplyr::if_else(is.na(journal), "BioRxiv", journal)) %>%
    dplyr::mutate(journal = stringr::str_replace_all(journal, "&amp;", "&"))
}

#' @export
crossref_data_pull_journal <- function(crossref_data) {
  out <- crossref_data %>%
    dplyr::mutate(journal = container.title) %>%
    crossref_data_clean_journal()
}

crossref_data_clean_title <- function(crossref_data) {
  out <- crossref_data %>%
    dplyr::mutate(
      title = title %>%
        stringr::str_remove_all("&lt;i&gt;|&lt;/i&gt;|<scp>|</scp>") %>%
        stringr::str_replace_all("<i>|</i>", " ") %>%
        stringr::str_replace_all("&amp;", "&") %>%
        stringr::str_replace_all("\\.$", "") %>%
        stringr::str_squish()
    )
}

#' @export
crossref_data_pull_title <- function(crossref_data) {
  out <- crossref_data %>% crossref_data_clean_title()
}
