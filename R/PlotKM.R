#' One Sample Survival Plotting Frame
#' 
#' @param data Data.frame
#' @param alpha Type I error.
#' @param eval_points Number of points at which to evaluate the curve.
#' @param return_surv Logical, TRUE for survival, FALSE for cumulative incidence.
#' @param status_name Name of status column.
#' @param strata_name Name of strata column.
#' @param tau Trunction time.
#' @param time_name Name of time column.
#' @return Data.frame.
#' @importFrom dplyr "%>%"
#' @noRd
OneSampleSurvFrame <- function(
    data,
    alpha = 0.05,
    eval_points = 1000,
    return_surv = TRUE, 
    status_name = "status",
    strata_name = "strata",
    tau = NULL,
    time_name = "time"
) {
  
  # Prepare data.
  df <- data %>%
    dplyr::rename(
      status = {{status_name}},
      strata = {{strata_name}},
      time = {{time_name}}
    )
  
  # Tabulate marginal KM curve.
  z <- stats::qnorm(1 - alpha / 2)
  km <- MargKM(status = df$status, strata = df$strata, time = df$time)
  km$lower <- pmax(km$surv - z * km$se, 0)
  km$upper <- pmin(km$surv + z * km$se, 1)
  
  # Step functions.
  surv_fn <- stats::stepfun(x = km$time, y = c(1, km$surv))
  lower_fn <- stats::stepfun(x = km$time, y = c(1, km$lower))
  upper_fn <- stats::stepfun(x = km$time, y = c(1, km$upper))
  
  # Time grid.
  if (is.null(tau)) {
    tau <- max(df$time)
  }
  times <- seq(from = 0, to = tau, length.out = eval_points)
  out <- data.frame(
    time = times,
    prob = surv_fn(times),
    lower = lower_fn(times),
    upper = upper_fn(times)
  )
  if (!return_surv) {
    out$prob <- 1 - out$prob
    lower <- pmax(1 - out$upper, 0)
    upper <- pmin(1 - out$lower, 1)
    out$lower <- lower
    out$upper <- upper
  }
  return(out)
}


#' Two Sample Survival Plotting Frame
#' 
#' @param data Data.frame.
#' @param alpha Type I error.
#' @param arm_name Name of arm column.
#' @param eval_points Number of points at which to evaluate the curve.
#' @param return_surv Logical, TRUE for survival, FALSE for cumulative incidence.
#' @param status_name Name of status column.
#' @param strata_name Name of strata column.
#' @param tau Trunction time.
#' @param time_name Name of time column.
#' @return Data.frame.
#' @importFrom dplyr "%>%"
#' @noRd
TwoSampleSurvFrame <- function(
    data,
    tau,
    alpha = 0.05,
    arm_name = "arm",
    eval_points = 1000,
    return_surv = TRUE, 
    status_name = "status", 
    strata_name = "strata",
    time_name = "time"
) {
  
  # Data.frame.
  data <- data %>%
    dplyr::rename(
      arm = {{arm_name}},
      status = {{status_name}},
      strata = {{strata_name}},
      time = {{time_name}}
    )
  
  # Prepare data.
  arm <- NULL
  df0 <- data %>%
    dplyr::filter(arm == 0) %>%
    OneSampleSurvFrame(
      eval_points = eval_points,
      return_surv = return_surv,
      tau = tau
    ) %>%
    dplyr::mutate(arm = 0)
  
  df1 <- data %>%
    dplyr::filter(arm == 1) %>%
    OneSampleSurvFrame(
      eval_points = eval_points,
      return_surv = return_surv,
      tau = tau
    ) %>%
    dplyr::mutate(arm = 1)
  
  out <- rbind(df0, df1)
  out$arm <- factor(out$arm, levels = c(0, 1), ordered = TRUE)
  return(out)
}



#' Plot Two Sample Survival
#'
#' @param data Data.frame containing time, status, arm, and strata.
#' @param alpha Type I error.
#' @param arm_name Name of arm column.
#' @param cis Include confidence intervals?
#' @param color_labs Color labels.
#' @param color_ctrl Color for control arm.
#' @param color_trt Color for treatment arm.
#' @param plot_surv Logical, TRUE for survival curves, FALSE for cumulative incidence.
#' @param status_name Name of status column.
#' @param strata_name Name of strata column.
#' @param tau Truncation time.
#' @param time_name Name of time column.
#' @param title Plot title.
#' @param x_breaks X-axis breaks.
#' @param x_labs X-axis labels.
#' @param x_name X-axis name.
#' @param x_max X-axis upper limit.
#' @param y_name Y-axis name.
#' @param y_lim Y-axis limits.
#' @return ggplot.
#' @importFrom dplyr "%>%"
#' @export
PlotTwoSampleKM <- function(
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
) {
  
  # Defaults.
  if (is.null(x_max)) {
    x_max <- max(data %>% dplyr::select(dplyr::all_of(time_name)))
  }
  if (is.null(tau)) {
    tau <- x_max
  }
  if (is.null(x_breaks)) {
    x_breaks <- round(seq(from = 0.0, to = x_max, length.out = 10))
  }
  if (is.null(x_labs)) {
    x_labs <- x_breaks
  }
  
  # Prepare data.
  df_km <- data %>% TwoSampleSurvFrame(
    tau = tau,
    arm_name = arm_name,
    return_surv = plot_surv,
    status_name = status_name,
    strata_name = strata_name,
    time_name = time_name
  )
  
  # Plotting.
  arm <- lower <- prob <- time <- upper <- NULL
  q <- ggplot2::ggplot() +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top"
    ) 
  
  # Confidence interval.
  if (cis) {
    q <- q + ggplot2::geom_ribbon(
      data = df_km,
      ggplot2::aes(x = time, ymin = lower, ymax = upper, fill = arm),
      alpha = 0.2
    )
  }
  
  # Step function.
  q <- q + 
    ggplot2::geom_step(
      data = df_km, 
      ggplot2::aes(x = time, y = prob, color = arm),
      linewidth = 1
    )
  
  # Plot adjustments.
  q <- q + 
    ggplot2::scale_color_manual(
      name = NULL, 
      labels = color_labs, 
      values = c(color_ctrl, color_trt)
    ) + ggplot2::scale_fill_manual(
      name = NULL, 
      labels = color_labs, 
      values = c(color_ctrl, color_trt)
    ) +
    ggplot2::scale_x_continuous(
      name = x_name,
      breaks = x_breaks,
      labels = x_labs,
      limits = c(0, x_max)
    ) +
    ggplot2::scale_y_continuous(
      name = y_name,
      limits = y_lim
    ) + 
    ggplot2::ggtitle(
      label = title
    )
  return(q)
}
