#' Generate Data per Stratum
#' 
#' @param n Sample size.
#' @param censor_rate Censoring rate.
#' @param event_rate Event rate.
#' @param stratum Stratum label.
#' @param tau Truncation time.
#' @importFrom stats rexp

GenDataStratum <- function(
  n,
  censor_rate,
  event_rate,
  stratum,
  tau
) {
  
  # Censor time.
  if (censor_rate > 0) {
    censor <- rexp(n = n, rate = censor_rate)
  } else {
    censor <- rep(tau, n)
  }
  
  # Event time.
  event <- rexp(n = n, rate = event_rate)
  
  # Observation time
  time <- pmin(censor, event)
  
  # Output.
  out <- data.frame(
    time = time,
    status = 1 * (event <= time),
    stratum = stratum
  )
  return(out)  
}

#' Generate Data
#' 
#' @param n Per-stratum sample sizes.
#' @param censor_rates Per-stratum censoring rates.
#' @param event_rates Per-stratum event rates. Determines the
#'   number of strata.
#' @param tau Truncation time.
#' @importFrom dplyr "%>%" group_by summarise
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
  df$stratum <- seq_len(nrow(df))
  
  # Data
  data <- df %>%
    dplyr::group_by(stratum) %>%
    dplyr::summarise(
      GenDataStratum(
        n = n, 
        censor_rate = censor_rate, 
        event_rate = event_rate,
        stratum = stratum,
        tau = tau
      )
    )
  
  # Output.
  return(data)
}