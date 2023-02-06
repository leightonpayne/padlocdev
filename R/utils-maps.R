map_typeof <- function(x) {
  purrr::map_vec(x, function(x) typeof(x), .ptype = "")
}

map_class <- function(x) {
  purrr::map_vec(x, function(x) class(x), .ptype = "")
}

is_whole <- function(x) {
  if (is.numeric(x)) { all(x%%1 == 0) } else { FALSE }
}

map_is_whole <- function(x) {
  purrr::map_vec(x, function(x) is_whole(x), .ptype = logical(1))
}

map_length <- function(x) {
  purrr::map_int(x, function(x) length(x))
}

map_duplicated <- function(x) {
  purrr::map(x, function(x) x[duplicated(x)])
}
