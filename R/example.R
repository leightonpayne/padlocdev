#' @title Get path to padlocdev example
#' @description padlocdev comes bundled with a number of sample files in its
#' `inst/extdata` directory. This function make them easy to access.
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @return A [base::character()].
#' @export
#' @examples
#' padlocdev_example()
#' padlocdev_example("protein.hmm")
padlocdev_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "padlocdev"))
  } else {
    system.file("extdata", file, package = "padlocdev", mustWork = TRUE)
  }
}
