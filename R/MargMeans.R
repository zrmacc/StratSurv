# Purpose: Stratified analysis of a mean.
# Updated: 2023-08-30


# -----------------------------------------------------------------------------

#' Stratified Mean Analysis
#' 
#' Calculates the difference and ratio of means. The marginal 
#' statistics are obtained by taking a weighted combination of the stratum-
#' specific statistics, where the weights are proportional to the stratum
#' sizes.
#' 
#' @param arm Arm, assumed to have two levels, coded 0/1.
#' @param y Continuous outcome.
#' @param strata Stratification factor. 
#' @param alpha Type I error.
#' @param weights Per-stratum weights, for sorted strata.
#' @importFrom dplyr "%>%"
#' @export
StratMeans <- function(
  arm,
  y,
  strata = NULL,
  alpha = 0.05,
  weights = NULL
) {
  
  # Create single stratum if no strata are provided. 
  if (is.null(strata)){
    strata <- rep(1, length(y))
  }
  
  # Data.
  data <- data.frame(arm, strata, y)
  
  # Prepare stratum weights.
  weights <- PrepWeights(data = data, weight = weights)
  
  # Per-stratum means.
  means <- data %>%
    dplyr::group_by(arm, strata) %>%
    dplyr::summarise(
      est = mean(y),
      se = sqrt(stats::var(y) / dplyr::n()),
      .groups = "drop"
    ) %>%
    dplyr::inner_join(weights, by = "strata")

  # Marginalize.
  marg <- means %>%
    dplyr::group_by(arm) %>%
    dplyr::reframe(
      MargStats(
        est = est,
        se = se,
        weight = weight,
        alpha = alpha
      )
    )
  
  # Contrasts.
  arm <- NULL
  est <- est0 <- est1 <- NULL
  stat <- NULL
  se <- se0 <- se1 <- NULL
  weight <- NULL
  
  contrasts <- marg %>%
    dplyr::select(arm, est, se) %>%
    tidyr::pivot_wider(
      names_from = arm, 
      values_from = c(est, se), 
      names_sep = ""
      ) %>%
    dplyr::reframe(
      Contrasts(est1 = est1, est0 = est0, se1 = se1, se0 = se0, alpha = alpha)
    )
  
  # Weights data.frame.
  counts <- PrepCounts(data = data, weights = weights)
  
  # Output.
  out <- methods::new(
    Class = "stratSurv",
    Stratified = means,
    Marginal = marg,
    Contrasts = contrasts,
    Weights = counts
  )
  return(out)
}