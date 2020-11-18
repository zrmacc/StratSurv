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
  colnames(out) <- c("Est", "SE", "L", "U")
  out <- cbind(
    "Arm" = c(0, 0, 1, 1),
    "Tau" = rep(tau, 4),
    "Stat" = rep(c("RMST", "RMTL"), times = 2),
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
    "Est" = sum(weight * est),
    "SE" = sqrt(sum(weight^2 * se^2))
  )
  out$L <- out$Est - crit * out$SE
  out$U <- out$Est + crit * out$SE
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
    "Contrast" = c("A1-A0", "A1/A0"),
    "Est" = c(delta, rho),
    "L" = c(delta_lower, rho_lower),
    "U" = c(delta_upper, rho_upper),
    "P" = c(delta_p, rho_p)
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
#' @importFrom methods new
#' @export
#' @examples 
#' data(strat_data)
#' StratRMST(
#'   time = strat_data$time,
#'   status = strat_data$status,
#'   arm = strat_data$arm,
#'   strata = strat_data$stratum,
#'   tau = 18
#' )

StratRMST <- function(
  time,
  status,
  arm,
  strata = NULL,
  tau,
  alpha = 0.05
) {
  
  # Create single stratum if no strata are provided. 
  if (is.null(strata)){
    strata <- rep(1, length(time))
  }
  
  # Data.
  data <- data.frame(time, status, arm, strata)
  
  # Split data by strata.
  split_data <- split(x = data, f = data$strata)
  
  # Weights proportional to stratum sizes.
  strata_tab <- table(data$strata)
  weights <- strata_tab / nrow(data)
  
  # Calculate per-stratum statistics.
  aux <- function(df) {
    out <- StratumRMST(
      time = df$time,
      status = df$status,
      arm = df$arm,
      tau = tau
    )
    out <- cbind(
      "Stratum" = unique(df$strata),
      out
    )
    return(out)
  }
  per_stratum_stats <- lapply(split_data, aux)
  per_stratum_stats <- do.call(rbind, per_stratum_stats)
  rownames(per_stratum_stats) <- NULL
  per_stratum_rmst <- per_stratum_stats[per_stratum_stats$Stat == "RMST", ]
  per_stratum_rmtl <- per_stratum_stats[per_stratum_stats$Stat == "RMTL", ]
  
  # Split by arm.
  per_stratum_rmst_split <- split(x = per_stratum_rmst, f = per_stratum_rmst$Arm)
  per_stratum_rmtl_split <- split(x = per_stratum_rmtl, f = per_stratum_rmst$Arm)
  
  # Marginal statistics.
  aux <- function(df) {
    out <- MargStats(est = df$Est, se = df$SE, weight = weights, alpha = alpha)
    out <- cbind(
      "Arm" = unique(df$Arm),
      "Tau" = unique(df$Tau),
      "Stat" = unique(df$Stat),
      out
    )
    return(out)
  }
  marg_stats <- c(lapply(per_stratum_rmst_split, aux), lapply(per_stratum_rmtl_split, aux))
  marg_stats <- do.call(rbind, marg_stats)
  rownames(marg_stats) <- NULL
  marg_rmst <- marg_stats[marg_stats$Stat == "RMST", ]
  marg_rmtl <- marg_stats[marg_stats$Stat == "RMTL", ]
  
  # Contrast RMSTs.
  contrast_rmst <- Contrasts(
    est1 = marg_rmst$Est[marg_rmst$Arm == 1],
    est0 = marg_rmst$Est[marg_rmst$Arm == 0],
    se1 = marg_rmst$SE[marg_rmst$Arm == 1],
    se0 = marg_rmst$SE[marg_rmst$Arm == 0],
    alpha = alpha
  )
  contrast_rmst <- cbind(
    "Stat" = "RMST",
    contrast_rmst
  )
  
  # Contrast RMTLs.
  contrast_rmtl <- Contrasts(
    est1 = marg_rmtl$Est[marg_rmtl$Arm == 1],
    est0 = marg_rmtl$Est[marg_rmtl$Arm == 0],
    se1 = marg_rmtl$SE[marg_rmtl$Arm == 1],
    se0 = marg_rmtl$SE[marg_rmtl$Arm == 0],
    alpha = alpha
  )
  contrast_rmtl <- cbind(
    "Stat" = "RMTL",
    contrast_rmtl
  )
  
  contrasts <- rbind(
    contrast_rmst,
    contrast_rmtl
  )
  
  # Weights data.frame.
  weights_df <- data.frame(
    "Stratum" = names(strata_tab),
    "Weight" = as.numeric(weights),
    "N" = as.numeric(strata_tab),
    "N0" = as.numeric(table(data$strata[data$arm == 0])),
    "N1" = as.numeric(table(data$strata[data$arm == 1]))
  )
  
  # Output.
  out <- new(
    Class = "stratRMST",
    Stratified = per_stratum_stats,
    Marginal = marg_stats,
    Contrasts = contrasts,
    Weights = weights_df
  )
  return(out)
}