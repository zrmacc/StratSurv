% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MargSurv.R
\name{RateRatio}
\alias{RateRatio}
\title{Calculate Rate Ratio}
\usage{
RateRatio(alpha = 0.05, rates)
}
\arguments{
\item{alpha}{Type I error level.}

\item{rates}{Data.frame containing (strata, arm, rate, se).}
}
\value{
Per-stratum rate ratio 'rr' and log standard error 'log_se'.
}
\description{
Calculate Rate Ratio
}
