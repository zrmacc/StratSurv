% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotKM.R
\name{PlotTwoSampleKM}
\alias{PlotTwoSampleKM}
\title{Plot Two Sample Survival}
\usage{
PlotTwoSampleKM(
  data,
  alpha = 0.05,
  arm_name = "arm",
  cis = TRUE,
  color_labs = c("Ctrl", "Trt"),
  color_ctrl = "#EFC000FF",
  color_trt = "#6385B8",
  plot_surv = TRUE,
  status_name = "status",
  strata_name = "strata",
  tau = NULL,
  time_name = "time",
  title = NULL,
  x_breaks = NULL,
  x_labs = NULL,
  x_name = "Time",
  x_max = NULL,
  y_name = "Survival",
  y_lim = c(0, 1)
)
}
\arguments{
\item{data}{Data.frame containing time, status, arm, and strata.}

\item{alpha}{Type I error.}

\item{arm_name}{Name of arm column.}

\item{cis}{Include confidence intervals?}

\item{color_labs}{Color labels.}

\item{color_ctrl}{Color for control arm.}

\item{color_trt}{Color for treatment arm.}

\item{plot_surv}{Logical, TRUE for survival curves, FALSE for cumulative incidence.}

\item{status_name}{Name of status column.}

\item{strata_name}{Name of strata column.}

\item{tau}{Truncation time.}

\item{time_name}{Name of time column.}

\item{title}{Plot title.}

\item{x_breaks}{X-axis breaks.}

\item{x_labs}{X-axis labels.}

\item{x_name}{X-axis name.}

\item{x_max}{X-axis upper limit.}

\item{y_name}{Y-axis name.}

\item{y_lim}{Y-axis limits.}
}
\value{
ggplot.
}
\description{
Plot Two Sample Survival
}
