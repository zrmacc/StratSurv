#' Generate Data per Stratum
#' 
#' @param n Sample size.
#' @param censor_rate Censoring rate.
#' @param event_rate Event rate.
#' @param strata Stratum label.
#' @param tau Truncation time.
GenDataStrat <- function(
  n,
  censor_rate,
  event_rate,
  strata,
  tau
) {
  
  # Censor time.
  if (censor_rate > 0) {
    censor <- stats::rexp(n = n, rate = censor_rate)
  } else {
    censor <- rep(tau, n)
  }
  
  # Event time.
  event <- stats::rexp(n = n, rate = event_rate)
  
  # Observation time
  time <- pmin(censor, event)
  
  # Output.
  out <- data.frame(
    time = time,
    status = 1 * (event <= time)
  )
  return(out)  
}

#' Generate Data
#' 
#' @param n Per-strata sample sizes.
#' @param censor_rates Per-strata censoring rates.
#' @param event_rates Per-strata event rates. Determines the
#'   number of strata.
#' @param tau Truncation time.
#' @importFrom dplyr "%>%"
#' @export 

GenData <- function(
  n,
  censor_rates = 0.20,
  event_rates = 1.0,
  tau = 10
) {
  
  # Strata.
  df <- data.frame(
    n = n,
    censor_rate = censor_rates,
    event_rate = event_rates,
    tau = tau
  )
  df$strata <- seq_len(nrow(df))
  
  # Data
  censor_rate <- NULL
  event_rate <- NULL
  strata <- NULL
  data <- df %>%
    dplyr::group_by(strata) %>%
    dplyr::reframe(
      GenDataStrat(
        n = n, 
        censor_rate = censor_rate, 
        event_rate = event_rate,
        strata = strata,
        tau = tau
      )
    )
  
  # Output.
  return(data)
}