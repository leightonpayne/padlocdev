#' @title Read an HMMER3 formatted profile HMM
#' @param file Path to a profile HMM generated with HMMER3
#' @return A [base::list()].
#' @export
#' @examples
#' read_hmm(padlocdev_example("padloc-db/hmm/PDLC00150.hmm"))
read_hmm <- function(file) {
  errors <- ""

  data <- readLines(file)

  # Check that the HMM is a valid formatting version
  valid_version <- grepl("^HMMER3/f", data[1])
  if (!valid_version) {
    errors <- "Only HMMER3/f format supported"
    stop(errors)
  }

  # Check that the model data is actually present
  model_exists <- any(grepl("^HMM ", data))
  if (!model_exists) {
    errors <- "HMM model not detected"
    stop(errors)
  }

  # Separate into header and model
  split <- grep("^HMM ", data)
  header <- data[1:(split - 1)]
  model <- data[split:length(data)]

  # Handle header data
  mandatory_headers <- c(
    "HMMER3/f",
    "NAME",
    "LENG",
    "ALPH"
  )

  optional_headers <- c(
    "ACC",
    "DESC",
    "MAXL",
    "RF",
    "MM",
    "CONS",
    "CS",
    "MAP",
    "DATE",
    "COM",
    "NSEQ",
    "EFFN",
    "CKSUM",
    "GA",
    "TC",
    "NC",
    "STATS LOCAL MSV",
    "STATS LOCAL VITERBI",
    "STATS LOCAL FORWARD"
  )

  all_headers <- c(mandatory_headers, optional_headers)

  # Check that mandatory header sections are present
  mandatory_exist <- all(sapply(X = mandatory_headers, FUN = function(x) any(grepl(paste0("^", x), header))))
  if (!mandatory_exist) {
    errors <- paste0(
      "Not all mandatory header sections are present.\nMandatory header sections include: ",
      paste0(mandatory_headers, collapse = ", "), ".\n",
      "Check the HMMER User's Guide for more information:\n",
      "http://eddylab.org/software/hmmer/Userguide.pdf"
    )
    stop(errors)
  }

  # Store the header values as a list
  header_values <- sapply(X = all_headers, FUN = function(x) grep(paste0("^", x), header))
  # Remove `integer(0)` values:
  # opt <- opt[lapply(opt, length) > 0]
  for (i in 1:length(header_values)) {
    key <- names(header_values[i])
    value <- header_values[[i]]
    if (length(value) != 0) {
      header_values[i] <- stringr::str_remove(header[value], paste0(key, " *"))
    }
  }

  # Handle model data
  cols_alphabet <- readr::cols(
    "A" = readr::col_character(),
    "C" = readr::col_character(),
    "D" = readr::col_character(),
    "E" = readr::col_character(),
    "F" = readr::col_character(),
    "G" = readr::col_character(),
    "H" = readr::col_character(),
    "I" = readr::col_character(),
    "K" = readr::col_character(),
    "L" = readr::col_character(),
    "M" = readr::col_character(),
    "N" = readr::col_character(),
    "P" = readr::col_character(),
    "Q" = readr::col_character(),
    "R" = readr::col_character(),
    "S" = readr::col_character(),
    "T" = readr::col_character(),
    "V" = readr::col_character(),
    "W" = readr::col_character(),
    "Y" = readr::col_character()
  )

  cols_annotations <- readr::cols(
    "MAP"  = readr::col_character(),
    "CONS" = readr::col_character(),
    "RF"   = readr::col_character(),
    "MM"   = readr::col_character(),
    "CS"   = readr::col_character()
  )

  cols_transitions <- readr::cols(
    "m_m" = readr::col_character(),
    "m_i" = readr::col_character(),
    "m_d" = readr::col_character(),
    "i_m" = readr::col_character(),
    "i_i" = readr::col_character(),
    "d_m" = readr::col_character(),
    "d_d" = readr::col_character()
  )

  cols_match <- readr::cols()
  cols_match$cols <- c(cols_alphabet$cols, cols_annotations$cols)
  cols_insert <- cols_alphabet

  # Handle optional compo line
  compo_exists <- grepl("  COMPO   ", model[3])
  if (compo_exists) {
    compo <- stringr::str_remove(model[3], "  COMPO   ")
    compo <- readr::read_table(
      paste0(compo, "\n"),
      # This is not a row of insert emissions, but it has the same columns.
      col_names = names(cols_insert$cols),
      col_types = cols_insert
    )
  } else {
    compo <- NULL
  }

  node_one <- grep("^      1", model)
  node_zero <- node_one - 2

  node_matrix <- model[node_zero:length(model)]
  node_matrix <- substr(node_matrix, 11, nchar(node_matrix))

  node_zero_insert_emissions <- node_matrix[1]
  node_zero_insert_emissions_table <- readr::read_table(
    paste0(node_zero_insert_emissions, "\n"),
    col_names = names(cols_insert$cols),
    col_types = cols_insert
  ) %>%
    dplyr::mutate("node" = 0)

  node_zero_state_transitions <- node_matrix[2]
  node_zero_state_transitions_table <- readr::read_table(
    paste0(node_zero_state_transitions, "\n"),
    col_names = names(cols_transitions$cols),
    col_types = cols_transitions
  ) %>%
    dplyr::mutate("node" = 0)

  match_emissions <- node_matrix[seq(3, length(node_matrix), by = 3)]
  insert_emissions <- node_matrix[seq(4, length(node_matrix), by = 3)]
  state_transitions <- node_matrix[seq(5, length(node_matrix), by = 3)]

  match_table <- readr::read_table(
    match_emissions,
    col_names = names(cols_match$cols),
    col_types = cols_match
  ) %>%
    dplyr::mutate("node" = dplyr::row_number()) %>%
    dplyr::select("node", dplyr::everything())

  insert_table <- readr::read_table(
    insert_emissions,
    col_names = names(cols_insert$cols),
    col_types = cols_insert
  ) %>%
    dplyr::mutate("node" = dplyr::row_number()) %>%
    dplyr::bind_rows(node_zero_insert_emissions_table) %>%
    dplyr::arrange("node") %>%
    dplyr::select("node", dplyr::everything())

  transition_table <- readr::read_table(
    state_transitions,
    col_names = names(cols_transitions$cols),
    col_types = cols_transitions
  ) %>%
    dplyr::mutate("node" = dplyr::row_number()) %>%
    dplyr::bind_rows(node_zero_state_transitions_table) %>%
    dplyr::arrange("node") %>%
    dplyr::select("node", dplyr::everything())

  model_values <- list(
    compo = compo,
    match_emissions = match_table,
    insert_emissions = insert_table,
    state_transitions = transition_table
  )

  # Handle output
  out <- list(
    header = header_values,
    model = model_values
  )

  return(out)
}

#' Read all HMMs in a directory into a list
#' @param directory Path to a directory containing profile HMMs (*.hmm)
#' @return A [base::list()] of HMMs.
#' @export
#' @examples
#' hmms <- multi_read_hmm(padlocdev_example("padloc-db/hmm"))
#' hmms[1]
multi_read_hmm <- function(directory) {
  files <- list.files(directory, full.names = TRUE, pattern = "*.hmm")
  hmm_headers <- lapply(X = cli::cli_progress_along(files), FUN = function(i) read_hmm(files[i]))
  hmm_names <- stringr::str_remove(basename(files), ".hmm")
  names(hmm_headers) <- hmm_names
  hmm_headers
}

# TODO: Write a function that prints an HMM to a file

# write_hmm <- function(hmm) {
#   header_print <- paste0(
#     "HMMER3/f ", hmm$header$`HMMER3/f`, "\n",
#     "NAME  ", hmm$header$NAME,  "\n",
#     "ACC   ", hmm$header$ACC,   "\n",
#     "DESC  ", hmm$header$DESC,  "\n",
#     "LENG  ", hmm$header$LENG,  "\n",
#     "ALPH  ", hmm$header$ALPH,  "\n",
#     "MAXL  ", hmm$header$MAXL,  "\n",
#     "RF    ", hmm$header$RF,    "\n",
#     "MM    ", hmm$header$MM,    "\n",
#     "CONS  ", hmm$header$CONS,  "\n",
#     "CS    ", hmm$header$CS,    "\n",
#     "MAP   ", hmm$header$MAP,   "\n",
#     "DATE  ", hmm$header$DATE,  "\n",
#     "COM   ", hmm$header$COM,   "\n",
#     "NSEQ  ", hmm$header$NSEQ,  "\n",
#     "EFFN  ", hmm$header$EFFN,  "\n",
#     "CKSUM ", hmm$header$CKSUM, "\n",
#     "GA    ", hmm$header$GA,    "\n",
#     "TC    ", hmm$header$TC,    "\n",
#     "NC    ", hmm$header$NC,    "\n",
#     "STATS LOCAL MSV      ", str_pad(hmm$header$`STATS LOCAL MSV`,     17, "left", pad = " "), "\n",
#     "STATS LOCAL VITERBI  ", str_pad(hmm$header$`STATS LOCAL VITERBI`, 17, "left", pad = " "), "\n",
#     "STATS LOCAL FORWARD  ", str_pad(hmm$header$`STATS LOCAL FORWARD`, 17, "left", pad = " "), "\n"
#   ) %>%
#     # Remove empty KEY:VALUE pairs
#     str_remove_all("\n[A-Z]* +(?=\n)")
#
#
# model_print <- bind_rows(
#   hmm$model$compo,
#   bind_rows(
#     hmm$model$match_emissions,
#     hmm$model$insert_emissions,
#     hmm$model$state_transitions
#   ) %>% arrange(node)
# )
# }

#' Read the header section from a profile HMM into a list.
#' @param file Path to profile HMM (*.hmm)
#' @return A [base::list()].
#' @export
#' @examples
#' read_hmm_header(padlocdev_example("padloc-db/hmm/PDLC00150.hmm"))
read_hmm_header <- function(file) {
  # Only head the first 23 lines as possible header lines (if there's more
  # header lines than this then the HMM is broken anyway)
  header <- readr::read_lines(file, n_max = 23, progress = FALSE)

  # Handle header data
  all_headers <- c(
    "HMMER3/f",
    "NAME",
    "LENG",
    "ALPH",
    "ACC",
    "DESC",
    "MAXL",
    "RF",
    "MM",
    "CONS",
    "CS",
    "MAP",
    "DATE",
    "COM",
    "NSEQ",
    "EFFN",
    "CKSUM",
    "GA",
    "TC",
    "NC",
    "STATS LOCAL MSV",
    "STATS LOCAL VITERBI",
    "STATS LOCAL FORWARD"
  )

  # Store the header values as a list
  header_values <- sapply(X = all_headers, FUN = function(x) grep(paste0("^", x), header))
  # Remove `integer(0)` values:
  # opt <- opt[lapply(opt, length) > 0]
  for (i in 1:length(header_values)) {
    key <- names(header_values[i])
    value <- header_values[[i]]
    if (length(value) != 0) {
      header_values[i] <- stringr::str_remove(header[value], paste0(key, " *"))
    }
  }

  out <- list(header = header_values)
  out
}

#' Read the headers from a profile HMM into a list.
#' @param directory Path to a directory containing profile HMMs (*.hmm)
#' @return A [base::list()].
#' @export
#' @examples
#' hmms <- multi_read_hmm_header(padlocdev_example("padloc-db/hmm"))
#' hmms[1]
multi_read_hmm_header <- function(directory) {
  files <- list.files(directory, full.names = TRUE, pattern = "*.hmm")
  hmm_headers <- lapply(X = cli::cli_progress_along(files), FUN = function(i) read_hmm_header(files[i]))
  hmm_names <- stringr::str_remove(basename(files), ".hmm")
  names(hmm_headers) <- hmm_names
  hmm_headers
}
