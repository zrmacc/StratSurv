# -----------------------------------------------------------------------------
# Survival rate.
# -----------------------------------------------------------------------------

#' Kaplan-Meier Survival Rate
#' 
#' Calculate the survival rate at a given time point tau.
#' 
#' @param time Observation time.
#' @param status Event status. The event coded as 1 is assumed to be the event
#'   of interest.
#' @param tau Truncation time.
#' @param alpha Type I error.
#' @return Data.frame containing:
#' \itemize{
#'   \item Truncation time 'tau'.
#'   \item Survival 'rate'
#'   \item Standard error 'se'.
#' }
SurvRate <- function(time, status, tau, alpha = 0.05) {
  z <- stats::qnorm(p = 1 - alpha / 2)
  tab <- TabulateKM(status = status, time = time)
  km <- stats::stepfun(x = tab$time, y = c(1, tab$surv))
  se2 <- stats::stepfun(x = tab$time, y = c(0, tab$surv_var))
  out <- data.frame(
    tau = tau,
    rate = km(tau),
    se = sqrt(se2(tau))
  )
  out$lower <- out$rate - z * out$se
  out$upper <- out$rate + z * out$se
  return(out)
}


# -----------------------------------------------------------------------------
# Risk difference, ratio, odds ratio.
# -----------------------------------------------------------------------------

#' Calculate Rate Difference.
#' 
#' @param alpha Type I error level.
#' @param rates Data.frame containing (strata, arm, rate, se).
#' @importFrom dplyr "%>%"
#' @export
#' @return Per-stratum rate differences and standard error.

RateDiff <- function(alpha = 0.05, rates) {
  no_strata <- !("strata" %in% colnames(rates))
  z <- stats::qnorm(p = 1 - alpha / 2)
  arm <- NULL
  est <- NULL
  rate <- NULL
  log_se <- se <- NULL
  strata <- NULL
  
  if (no_strata) {rates$strata <- 1}
  rd <- rates %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(
      stat = "rd",
      est = rate[arm == 1] - rate[arm == 0],
      se = sqrt(se[arm == 1]^2 + se[arm == 0]^2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      lower = est - z * se,
      upper = est + z * se,
      p = 2 * stats::pnorm(q = abs(est) / se, lower.tail = FALSE)
    )
  return(rd)
}


#' Calculate Rate Ratio
#' 
#' @param alpha Type I error level.
#' @param rates Data.frame containing (strata, arm, rate, se).
#' @importFrom dplyr "%>%"
#' @export
#' @return Per-stratum rate ratio 'rr' and log standard error 'log_se'.

RateRatio <- function(alpha = 0.05, rates) {
  z <- stats::qnorm(p = 1 - alpha / 2)
  no_strata <- !("strata" %in% colnames(rates))
  arm <- NULL
  est <- NULL
  rate <- NULL
  log_se <- se <- NULL
  strata <- NULL
  
  if (no_strata) {rates$strata <- 1}
  rr <- rates %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(
      stat = "rr",
      est = rate[arm == 1] / rate[arm == 0],
      log_se = sqrt(se[arm == 1]^2 / rate[arm == 1]^2 + se[arm == 0]^2 / rate[arm == 0]^2),
      .groups = "drop"
    ) %>% 
    dplyr::mutate(
      lower = est * exp(-z * log_se),
      upper = est * exp(+z * log_se),
      se = est * log_se,
      p = 2 * stats::pnorm(q = abs(log(est)) / log_se, lower.tail = FALSE)
    ) %>% 
    dplyr::select(
      - log_se
    )
  return(rr)
}


#' Calculate Rate Odds Ratio
#' 
#' @param alpha Type I error.
#' @param rates Data.frame containing (strata, arm, rate, se).
#' @importFrom dplyr "%>%"
#' @export
#' @return Per-stratum rate odds ratio 'or' and log standard error 'log_se'.

OddsRatio <- function(alpha = 0.05, rates) {
  z <- stats::qnorm(p = 1 - alpha / 2)
  no_strata <- !("strata" %in% colnames(rates))
  arm <- NULL
  est <- NULL
  rate <- NULL
  log_se <- se <- NULL
  strata <- NULL
  
  if (no_strata) {rates$strata <- 1}
  or <- rates %>% 
    dplyr::group_by(strata) %>%
    dplyr::summarise(
      stat = "or",
      est = rate[arm == 1] * (1 - rate[arm == 0]) / rate[arm == 0] / (1 - rate[arm == 1]),
      log_se = sqrt(
        se[arm == 1]^2 / (rate[arm == 1]^2 * (1 - rate[arm == 1])^2) +
          se[arm == 0]^2 / (rate[arm == 0]^2 * (1 - rate[arm == 0])^2)
      ),
      .groups = "drop"
    ) %>% 
    dplyr::mutate(
      lower = est * exp(-z * log_se),
      upper = est * exp(+z * log_se),
      se = est * log_se,
      p = 2 * stats::pnorm(q = abs(log(est)) / log_se, lower.tail = FALSE)
    ) %>% 
    dplyr::select(
      - log_se
    )
  return(or)
}


# -----------------------------------------------------------------------------

#' Stratified RMST Analysis
#' 
#' Calculates the difference and ratio of marginal restricted mean survival 
#' times (RMSTs) and restricted mean times lost (RMTLs). The marginal 
#' statistics are obtained by taking a weighted combination of the stratum-
#' specific statistics, where the weights are proportional to the stratum
#' sizes.
#' 
#' @param time Observation time.
#' @param status Event status. The event coded as 1 is assumed to be the event
#'   of interest.
#' @param arm Arm, assumed to have two levels, coded 0/1.
#' @param strata Stratification factor. 
#' @param tau Truncation time.
#' @param alpha Type I error.
#' @param weights Per-stratum weights, for sorted strata.
#' @importFrom dplyr "%>%"
#' @export
#' @examples 
#' # Arm 1.
#' data1 <- GenData(
#'   n = 100,
#'   event_rates = c(0.5, 1),
#'   tau = 5
#' )
#' data1$arm <- 1
#' 
#' # Arm 0.
#' data0 <- GenData(
#'   n = 100,
#'   event_rates = c(1, 1),
#'   tau = 5
#' )
#' data0$arm <- 0
#' 
#' # Overall data set.
#' data <- rbind(data1, data0)
#' 
#' StratRate(
#'   time = data$time,
#'   status = data$status,
#'   arm = data$arm,
#'   strata = data$strata,
#'   tau = 2
#' )

StratRate <- function(
  time,
  status,
  arm,
  strata = NULL,
  tau,
  alpha = 0.05,
  weights = NULL
) {
  
  # Create single stratum if no strata are provided. 
  if (is.null(strata)){
    strata <- rep(1, length(time))
  }
  
  # Data.
  data <- data.frame(time, status, arm, strata)
  
  # Prepare stratum weights.
  weights <- PrepWeights(data = data, weight = weights)
  
  # Per-stratum event rates.
  strat_rates <- data %>%
    dplyr::group_by(strata, arm) %>%
    dplyr::reframe(
      SurvRate(alpha = alpha, time = time, status = status, tau = tau)
    ) %>% 
    dplyr::inner_join(
      weights,
      by = "strata"
    )
  
  # Marginal rates.
  rate <- NULL
  se <- NULL
  weight <- NULL
  z <- stats::qnorm(p = 1 - alpha / 2)
  marg_rates <- strat_rates %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(
      tau = max(tau),
      rate = sum(weight * rate),
      se = sqrt(sum(weight^2 * se^2)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      lower = rate - z * se,
      upper = rate + z * se
    )
  
  # Per-stratum contrasts.
  # strat_rd <- RateDiff(alpha = alpha, rates = strat_rates)
  # strat_rr <- RateRatio(alpha = alpha, rates = strat_rates)
  # strat_or <- OddsRatio(alpha = alpha, rates = strat_rates)
  # strat_contrasts <- rbind(strat_rd, strat_rr, strat_or)
  
  # Marginal contrasts.
  marg_rd <- RateDiff(alpha = alpha, rates = marg_rates)
  marg_rr <- RateRatio(alpha = alpha, rates = marg_rates)
  marg_or <- OddsRatio(alpha = alpha, rates = marg_rates)
  marg_contrasts <- rbind(marg_rd, marg_rr, marg_or)
  
  # Weights data.frame.
  counts <- PrepCounts(data = data, weights = weights)
  
  # Output.
  out <- methods::new(
    Class = "stratSurv",
    Stratified = strat_rates,
    Marginal = marg_rates,
    Contrasts = marg_contrasts,
    Weights = counts
  )
  return(out)
}