% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expand-hmm-meta.R
\name{expand_hmm_meta}
\alias{expand_hmm_meta}
\title{Expand HMM metadata table multi-name assignments}
\usage{
expand_hmm_meta(hmm_meta)
}
\arguments{
\item{hmm_meta}{An HMM metadata table (as read-in by \code{\link[=read_hmm_meta]{read_hmm_meta()}}).}
}
\value{
An HMM metadata table with expanded name assignments.
}
\description{
Expand HMM metadata table multi-name assignments.
}
\examples{
path <- padlocdev_example("padloc-db/hmm_meta.txt")
hmm_meta <- read_hmm_meta(path)
hmm_meta_expanded <- expand_hmm_meta(hmm_meta)
hmm_meta_expanded
}
