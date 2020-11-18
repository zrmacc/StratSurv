#' Sample Stratified Survival Data
#'
#' Synthetic time to event data for 600 patients divided into 3x 200 patient strata.
#' Within each stratum, there are 100 patients in the treatment arm, and 100 patients
#' in the reference arm.
#'
#' @docType data
#' @usage data(strat_data)
#' @format A data.frame containing three fields:
#' \describe{
#'   \item{arm}{Treatment arm, 0 for reference, 1 for treatment.}
#'   \item{time}{Observation time between 0 and 21.}
#'   \item{status}{Status indicator, 0 for censoring, 1 for an event.}
#'   \item{stratum}{Integer coded as 0, 1, 2.}
#' }
"strat_data"