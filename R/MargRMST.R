# -----------------------------------------------------------------------------

#' Calculate Per-Stratum Statistics
#' 
#' Calculate the per-stratum and per-arm RMST and RMTL.
#' 
#' @param time Observation time.
#' @param status Event status. The event coded as 1 is assumed to be the event
#'   of interest.
#' @param arm Arm, assumed to have two levels, coded 0/1.
#' @param tau Truncation time.
#' @importFrom survRM2 rmst2
#' @return Data.frame containing:
#' \itemize{
#'   \item 'Stratum' and 'Arm'.
#'   \item Truncation time 'Tau'.
#'   \item Statistic 'Stat' and estimate 'Est'.
#'   \item Standard error 'SE'.
#'   \item Lower 'L' and upper 'U'.
#' }

StratumRMST <- function(
  time,
  status,
  arm,
  tau
) {
  
  fit <- survRM2::rmst2(time = time, status = status, arm = arm, tau = tau)
  out <- data.frame(
    rbind(
      fit$RMST.arm0$result,
      fit$RMST.arm1$result
    )
  )
  colnames(out) <- c("est", "se", "lower", "upper")
  out <- cbind(
    "arm" = c(0, 0, 1, 1),
    "tau" = rep(tau, 4),
    "stat" = rep(c("RMST", "RMTL"), times = 2),
    out
  )
  rownames(out) <- NULL
  return(out)
}


# -----------------------------------------------------------------------------

#' Calculate Marginal Statistics
#' 
#' Calculate the marginalized RMST and RMTL by taking a weighted combination
#' of the per-stratum estimates.
#' 
#' @param est Statistic value.
#' @param se Standard error.
#' @param weight Statistic weight.
#' @param alpha Type I error.
#' @importFrom stats qnorm
#' @return Data.frame containing:
#' \itemize{
#'   \item Marginalized estimate 'Est' and standard error 'SE'.
#'   \item Lower 'L' and upper 'U' confidence bounds.
#' }

MargStats <- function(
  est,
  se,
  weight,
  alpha
) {
  crit <- qnorm(p = 1 - alpha / 2)
  out <- data.frame(
    "est" = sum(weight * est),
    "se" = sqrt(sum(weight^2 * se^2))
  )
  out$lower <- out$est - crit * out$se
  out$upper <- out$est + crit * out$se
  return(out)
}


# -----------------------------------------------------------------------------

#' Contrast Summary Statistics
#' 
#' Finds the difference and ratio of summary statistics, comparing two arms.
#' 
#' @param est1 Statistic for arm 1.
#' @param est0 Statistic for arm 0.
#' @param se1 Standard error for arm 1.
#' @param se0 Standard error for arm 0.
#' @param alpha Type I error.
#' @importFrom stats pnorm qnorm
#' @return Data.frame containing:
#' \itemize{
#'   \item 'Contrast' and estimate 'Est'.
#'   \item Lower 'L' and upper 'U' confidence bounds.
#'   \item 'P' value.
#' }

Contrasts <- function(
  est1,
  est0,
  se1,
  se0,
  alpha 
) {
  crit <- qnorm(p = 1 - alpha / 2)
  
  # Difference.
  delta <- est1 - est0
  se_diff <- sqrt(se1^2 + se0^2)
  delta_lower <- delta - crit * se_diff
  delta_upper <- delta + crit * se_diff
  delta_p <- 2 * pnorm(q = abs(delta) / se_diff, lower.tail = FALSE)
  
  # Ratio.
  rho <- est1 / est0 
  se_rho_log <- sqrt(se1^2 / est1^2 + se0^2 / est0^2)
  rho_lower <- rho * exp(- crit * se_rho_log)
  rho_upper <- rho * exp(+ crit * se_rho_log)
  rho_p <- 2 * pnorm(q = abs(log(rho)) / se_rho_log, lower.tail = FALSE)
  
  # Output.
  out <- data.frame(
    "contrast" = c("A1-A0", "A1/A0"),
    "est" = c(delta, rho),
    "lower" = c(delta_lower, rho_lower),
    "upper" = c(delta_upper, rho_upper),
    "p" = c(delta_p, rho_p)
  )
  return(out)
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
#' @importFrom methods new
#' @importFrom dplyr "%>%" group_by inner_join n select summarise
#' @importFrom tidyr pivot_wider
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
#' StratRMST(
#'   time = data$time,
#'   status = data$status,
#'   arm = data$arm,
#'   strata = data$strata,
#'   tau = 2
#' )

StratRMST <- function(
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
  
  # Per-stratum RMSTs.
  rmst <- data %>%
    dplyr::group_by(strata) %>%
    dplyr::summarise(
      StratumRMST(
        time = time,
        status = status,
        arm = arm,
        tau = tau
      ),
      .groups = "drop"
    ) %>%
    dplyr::inner_join(weights, by = "strata")

  # Marginalize.
  marg <- rmst %>%
    dplyr::group_by(arm, stat) %>%
    dplyr::summarise(
      MargStats(
        est = est,
        se = se,
        weight = weight,
        alpha = alpha
      ),
      .groups = "drop"
    )
  
  # Contrasts.
  arm <- NULL
  est <- est0 <- est1 <- NULL
  stat <- NULL
  se <- se0 <- se1 <- NULL
  weight <- NULL
  
  contrasts <- marg %>%
    dplyr::select(arm, stat, est, se) %>%
    tidyr::pivot_wider(
      names_from = arm, 
      values_from = c(est, se), 
      names_sep = ""
      ) %>%
    dplyr::group_by(stat) %>%
    dplyr::summarise(
      Contrasts(est1 = est1, est0 = est0, se1 = se1, se0 = se0, alpha = alpha),
      .groups = "drop"
    )
  
  # Weights data.frame.
  counts <- PrepCounts(data = data, weights = weights)
  
  # Output.
  out <- new(
    Class = "stratSurv",
    Stratified = rmst,
    Marginal = marg,
    Contrasts = contrasts,
    Weights = counts
  )
  return(out)
}