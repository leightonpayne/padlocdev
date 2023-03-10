% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example.R
\name{padlocdev_example}
\alias{padlocdev_example}
\title{Get path to padlocdev example}
\usage{
padlocdev_example(file = NULL)
}
\arguments{
\item{file}{Name of file. If \code{NULL}, the example files will be listed.}
}
\value{
A \code{\link[base:character]{base::character()}}.
}
\description{
padlocdev comes bundled with a number of sample files in its
\code{inst/extdata} directory. This function make them easy to access.
}
\examples{
padlocdev_example()
padlocdev_example("padloc-db/hmm/PDLC00150.hmm")
}
