#' Stratum Counts
#' 
#' @param data Containing (arm, strata).
#' @param weights Weights data.frame as returned by \code{\link{PrepWeights}}.
#' @return Data.frame containing per-stratum counts in each arm.
PrepCounts <- function(data, weights) {
  arm <- n <- strata <- NULL
  counts <- data %>%
    dplyr::group_by(arm, strata) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = arm,
      names_sep = "",
      values_from = n
    ) %>%
    dplyr::rename(
      n0 = "0",
      n1 = "1"
    ) %>%
    dplyr::inner_join(
      weights,
      by = "strata"
    )
  return(counts)
}


#' Calculate Stratum Weights
#' 
#' @param data Containing (time, status, arm, strata).
#' @param weight Input weights.
#' @importFrom dplyr "%>%"
#' @return Data.frame containing 'strata' and 'weight'.
PrepWeights <- function(data, weight) {
  strata <- NULL
  if (is.null(weight)) {
    weights <- data %>%
      dplyr::group_by(strata) %>%
      dplyr::summarise(weight = dplyr::n(), .groups = "drop")
  } else {
    weights <- data.frame(
      strata = sort(unique(data$strata)),
      weight = weight
    )
  }
  weights$weight <- weights$weight / sum(weights$weight)
  return(weights)
}
