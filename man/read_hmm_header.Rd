% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io-hmms.R
\name{read_hmm_header}
\alias{read_hmm_header}
\title{Read the header section from a profile HMM into a list.}
\usage{
read_hmm_header(file)
}
\arguments{
\item{file}{Path to profile HMM (*.hmm)}
}
\value{
A \code{\link[base:list]{base::list()}}.
}
\description{
Read the header section from a profile HMM into a list.
}
\examples{
read_hmm_header(padlocdev_example("padloc-db/hmm/PDLC00150.hmm"))
}
