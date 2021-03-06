% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/average_up.R
\name{average_up}
\alias{average_up}
\title{Aggregate FOI predictions at 1/6 degree resolutions to adim unit level}
\usage{
average_up(pxl_df, grp_flds, covariates_names)
}
\arguments{
\item{pxl_df}{dataframe of foi predictions at 1/6 degree resolution.}

\item{grp_flds}{character string of column names by which the 1/6 degree
resolution predictions are aggregated by.}

\item{covariates_names}{character vector of covariates names.}
}
\description{
The function aggregates foi predictions at 1/6 degree resolutions to
adim unit 1 level by taking the population-weighted average of the
1/6 degree resolution predictions within each admin unit 1.
}
