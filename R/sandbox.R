# TODO: Write a function for assessing the amount of overlap between system
# groups.

read_hmm_basic <- function(file) {
  data <- readLines(file)
  # Separate into header and model
  split <- grep("^HMM ", data)
  header <- data[1:(split - 1)]
  model <- data[split:length(data)]
  # Handle header data
  header_fields <- c(
    "HMMER3/f", "NAME", "ACC", "DESC", "LENG", "ALPH", "MAXL", "RF", "MM",
    "CONS", "CS", "MAP", "DATE", "COM", "NSEQ", "EFFN", "CKSUM", "GA", "TC",
    "NC", "STATS LOCAL MSV", "STATS LOCAL VITERBI", "STATS LOCAL FORWARD"
  )
  # Store the header values as a list
  header_values <- sapply(
    X = header_fields,
    FUN = function(x) grep(paste0("^", x), header)
  )
  # Remove header names from header values
  for (i in 1:length(header_values)) {
    key <- names(header_values[i])
    value <- header_values[[i]]
    if (length(value) != 0) {
      header_values[i] <- stringr::str_remove(header[value], paste0(key, " *"))
    }
  }
  # Handle output
  out <- list(header = header_values, model = model)
  return(out)
}


multi_read_hmm_basic <- function(directory) {
  files <- list.files(directory, full.names = TRUE, pattern = "*.hmm")
  hmms <- lapply(X = cli::cli_progress_along(files), FUN = function(i) read_hmm_basic(files[i]))
  hmm_names <- stringr::str_remove(basename(files), ".hmm")
  names(hmms) <- hmm_names
  hmms
}


# ---
# tmp2 <- tmp %>%
#   purrr::transpose()
#
#   do.call(cbind, .$header) %>%
#   tibble::as_tibble()
#
#
#   purrr::map(
#     .x = .$header,
#     .f = function(x) {
#       do.call(cbind, x) %>% tibble::as_tibble()
#     },
#     .progress = TRUE
#   )

# ---


write_hmm_basic <- function(hmm, path) {
  title <- c("HMMER3/f")
  stats <- c("STATS LOCAL MSV", "STATS LOCAL VITERBI", "STATS LOCAL FORWARD")
  header <- purrr::imap(
    .x = hmm$header,
    .f = ~ (
      if (length(.x) != 0) {
        if (! .y %in% c(title, stats)) {
          line <- paste0(
            stringr::str_pad(.y, width = 6, side = "right"),
            .x
          )
        } else if (.y %in% title) {
          line <- paste0(.y, " ", .x)
        } else {
          line <- paste0(
            stringr::str_pad(.y, width = 21, side = "right"),
            stringr::str_pad(.x, width = 17, side = "left")
          )
        }
        line
      }
    )
  )
  header <- Filter(Negate(is.null), header)
  readr::write_lines(c(header, hmm$model), path)
}

update_hmm_header <- function(hmm, hmm_meta) {

}
