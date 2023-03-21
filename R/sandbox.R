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
