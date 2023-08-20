# Purpose: Construct marginal KM curves.
# Updated: 2023-08-20


#' Tabulate Kaplan-Meier 
#' 
#' @param status Event status.
#' @param time Event time.
#' @return Data.frame.
#' @importFrom dplyr "%>%"
#' @export
TabulateKM <- function(
  status,
  time
) {
  
  # Events table.
  data <- data.frame("status" = status, "time" = time)
  out <- data %>%
    dplyr::arrange(time) %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(
      censor = sum(status == 0),
      events = sum(status == 1)
    ) %>% dplyr::ungroup()
  
  # Add initial row.
  out <- rbind(c(0, 0, 0), out)
  
  # Number at risk.
  n <- nrow(data)
  n_removed <- cumsum(out$censor + out$events)
  out$nar <- n - c(0, n_removed[1:(nrow(out) - 1)])
  
  # Hazard.
  out$haz <- out$events / out$nar
  out$cum_haz <- cumsum(out$haz)
  out$cum_haz_var <- cumsum(out$events / out$nar^2)
  
  # Survival.
  out$surv <- cumprod(1 - out$haz)
  out$surv_var <- (out$surv^2) * out$cum_haz_var
  return(out)
}


#' Marginal Kaplan-Meier Curve
#' 
#' @param status Event status.
#' @param strata Strata.
#' @param time Event time.
#' @returns Tabulate marginal KM curve.
#' @export
MargKM <- function(status, strata, time) {

  unique_levels <- sort(unique(strata))
  unique_times <- sort(unique(c(0, time)))
  n <- length(strata)
  
  # Prepare stratum-specific KM curves.
  results <- lapply(unique_levels, function(s) {
    
    # Subset to stratum.
    key <- (strata == s)
    n_s <- sum(key)
    w_s <- n_s / n
    status_s <- status[key]
    time_s <- time[key]
    
    # Construct KM curves.
    tab <- TabulateKM(status = status_s, time = time_s)
    km_fn <- stats::stepfun(x = tab$time, y = c(1, tab$surv))
    nar_fn <- stats::stepfun(x = tab$time, y = c(n_s, tab$nar))
    se2_fn <- stats::stepfun(x = tab$time, y = c(0, tab$surv_var))
    out <- data.frame(
      weight = w_s,
      time = unique_times,
      nar = nar_fn(unique_times),
      surv = km_fn(unique_times),
      se2 = se2_fn(unique_times)
    )
    
    stratum <- NULL
    out$stratum <- s
    out <- out %>% dplyr::relocate(stratum)
    return(out)
  })

  # Calculate marginal KM curve.
  km_marg <- data.frame(
    time = unique_times,
    nar = 0,
    surv = 0,
    se2 = 0
  )
  
  for (i in 1:length(results)) {
    km_s <- results[[i]]
    km_marg$nar <- km_marg$nar + km_s$nar
    km_marg$surv <- km_marg$surv + km_s$weight * km_s$surv
    km_marg$se2 <- km_marg$se2 + (km_s$weight^2 * km_s$se2)
  }
  km_marg$se <- sqrt(km_marg$se2)
  km_marg$se2 <- NULL
  return(km_marg)
}


