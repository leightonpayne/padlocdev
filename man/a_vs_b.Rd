% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparison-methods.R
\name{a_vs_b}
\alias{a_vs_b}
\title{Compare two dataframes / tibbles}
\usage{
a_vs_b(a, b, join_by)
}
\arguments{
\item{a}{first dataframe}

\item{b}{second dataframe}

\item{join_by}{column in common}
}
\value{
A \code{\link[base:list]{base::list()}} of three \code{\link[base:data.frame]{base::data.frame()}}s or
\code{\link[tibble:tibble]{tibble::tibble()}}s, one with entries in 'a' but not 'b' and vice-versa,
and one with entries in both 'a' and 'b'
}
\description{
This is a helper function for comparing two dataframes / tibbles
}
