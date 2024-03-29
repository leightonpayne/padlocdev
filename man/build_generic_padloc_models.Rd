% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io-padloc-model.R
\name{build_generic_padloc_models}
\alias{build_generic_padloc_models}
\title{Build a list of generic padloc models from a list of loci}
\usage{
build_generic_padloc_models(df, loci_col, delim = "__", name_prefix = "SYS_")
}
\arguments{
\item{df}{A \code{\link[base:data.frame]{base::data.frame()}} with at least one column, containing the
locus structures in the form \code{geneA__geneB__geneC}, where \verb{__}
indicates separation between genes and can optionally be set to any other
delimiter by specifying \code{delim}.}

\item{loci_col}{The name of the column containing locus structures,}

\item{delim}{The delimiter that separates gene names.}

\item{name_prefix}{A prefix to use when generating generic names for each
model.}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}}.
}
\description{
Build a list of generic padloc models from a list of loci.
}
