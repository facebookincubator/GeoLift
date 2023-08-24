# Copyright (c) Meta Platforms, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function GeoPlot, plot.GeoLiftPower, plot.GeoLift, Lift.plot,
# absolute_value.plot, cumulative_value.plot, plot.MultiCellMarketSelection
# plot.MultiCellPower, plot.GeoLiftMultiCell


#' Plotting function for Exploratory Analysis.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `GeoPlot` takes a data frame used for GeoLift to generate
#' a plot of each location's time series.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param Y_id Name of the outcome variable (String).
#' @param time_id Name of the time variable (String).
#' @param location_id Name of the location variable (String).
#' @param treatment_start Treatment start (Default = 0).
#' @param KPI_id Outcome variable.
#' @param notes Additional notes.
#'
#' @return
#' A plot of each location's time series.
#'
#' @export
GeoPlot <- function(data,
                    Y_id = "Y",
                    time_id = "time",
                    location_id = "location",
                    treatment_start = 0,
                    KPI_id = "",
                    notes = "") {
  if (treatment_start == 0) {
    size_vline <- 0
  } else {
    size_vline <- 0.8
  }

  p <- ggplot(data, aes(y = !!sym(Y_id), x = !!sym(time_id), colour = !!sym(location_id), label = !!sym(location_id))) +
    geom_line(show.legend = FALSE) +
    geom_vline(xintercept = treatment_start, linetype = "dashed", size = size_vline, color = "grey35") +
    geom_dl(aes(label = !!sym(location_id)), method = list(dl.combine("last.points"), cex = 0.8)) +
    xlim(0, 1.15 * (max(data[[time_id]]))) +
    labs(y = KPI_id, caption = notes) +
    theme_minimal()

  print(p)
}


#' @param x \code{GeoLiftPower()} output.
#' @param actual_values Logic flag indicating whether to include in the plot
#' the actual values. TRUE by default.
#' @param smoothed_values Logic flag indicating whether to include in the plot
#' the smoothed values. TRUE by default.
#' @param show_mde Logic flag indicating whether to include in the plot
#' the positive and negative MDEs. FALSE by default.
#' @param breaks_x_axis Numeric value indicating the number of breaks in the
#' x-axis of the power plot. You may get slightly more or fewer breaks that
#' requested based on `breaks_pretty()`. Set to 10 by default.
#' @param notes String with additional notes to add as caption.
#' @param ... additional arguments
#'
#' @rdname GeoLiftPower
#' @order 2
#' @export
#' @importFrom graphics plot
plot.GeoLiftPower <- function(x,
                              actual_values = TRUE,
                              smoothed_values = TRUE,
                              show_mde = FALSE,
                              breaks_x_axis = 10,
                              notes = "",
                              ...) {
  final_legend <- c()
  if (!inherits(x, "GeoLiftPower")) {
    stop("object must be class GeoLiftPower")
  }

  if (length(unique(x$duration)) > 1) {
    stop("More than 1 test durations detected.  Please only use one.")
  }

  treatment_periods <- unique(x$duration)
  EffectSize <- unique(x$EffectSize)

  PowerPlot_data <- x %>%
    dplyr::group_by(EffectSize) %>%
    dplyr::summarise(power = mean(power), investment = mean(Investment)) %>%
    dplyr::mutate(AvgCost = investment / EffectSize)

  PowerPlot_graph <- ggplot(PowerPlot_data, aes(x = EffectSize, y = power)) +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey") +
    labs(
      title = "GeoLift Power Curve",
      subtitle = paste0("Treatment Periods: ", unique(x$duration)),
      x = "Effect Size",
      y = "Power",
      caption = notes
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_cartesian(ylim = c(0, 1))

  if (sum(PowerPlot_data$investment != 0)) {
    CostPerLift <- as.numeric(
      x %>%
        dplyr::filter(EffectSize != 0) %>%
        dplyr::mutate(AvgCost = abs(Investment / EffectSize)) %>%
        dplyr::summarise(mean(AvgCost))
    )
    PowerPlot_graph <- PowerPlot_graph +
      scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        sec.axis = sec_axis(
          ~ . * CostPerLift,
          breaks = scales::pretty_breaks(n = breaks_x_axis),
          name = "Estimated Investment"
        ),
        breaks = scales::pretty_breaks(n = breaks_x_axis)
      )
  } else {
    PowerPlot_graph <- PowerPlot_graph +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1))
  }

  if (smoothed_values == TRUE) {
    final_legend <- c(final_legend, c("Smoothed Values" = "#52854C"))
    PowerPlot_graph <- PowerPlot_graph +
      geom_smooth(
        formula = y ~ x,
        method = "loess",
        se = FALSE,
        alpha = 0.8,
        linetype = "dashed",
        aes(color = "Smoothed Values")
      )
  }

  if (actual_values == TRUE) {
    final_legend <- c(final_legend, c("Actual Values" = "#4B4196"))
    PowerPlot_graph <- PowerPlot_graph +
      geom_line(size = 0.62, alpha = 0.5, aes(colour = "Actual Values"))
  }

  if (show_mde == TRUE) {
    final_legend <- c(final_legend, c("MDE Values" = "salmon"))
    if (min(EffectSize) < 0) {
      negative_df <- PowerPlot_data %>%
        dplyr::filter(EffectSize < 0 & power > 0.8)

      PowerPlot_graph <- PowerPlot_graph +
        geom_vline(aes(xintercept = max(negative_df[, "EffectSize"]), color = "MDE Values"), alpha = 0.4, linetype = "dashed")
    }

    if (max(EffectSize) > 0) {
      positive_df <- PowerPlot_data %>%
        dplyr::filter(EffectSize > 0 & power > 0.8)

      PowerPlot_graph <- PowerPlot_graph +
        geom_vline(aes(xintercept = min(positive_df[, "EffectSize"]), color = "MDE Values"), alpha = 0.4, linetype = "dashed")
    }
  }

  PowerPlot_graph <- PowerPlot_graph + scale_colour_manual(
    name = "Power Values",
    values = final_legend
  )

  return(PowerPlot_graph)
}


#' @param x \code{GeoLift()}.
#' @param type Type of plot. By default "Lift" which plots the
#' incrementality on the outcome variable. If type is set to "ATT",
#' the average ATT is plotted. If type is set to "Incrementality",
#' daily incremental values are plotted.
#' @param treatment_end_date Character that represents a date in year-month=day format.
#' @param frequency Character that represents periodicity of time stamps. Can be either
#' weekly or daily. Defaults to daily.
#' @param title String for the title of the plot. Empty by default.
#' @param plot_start_date Character that represents initial date of plot in year-month-day format.
#' @param subtitle String for the subtitle of the plot. Empty by default.
#' @param notes String to add notes to the plot. Empty by default.
#' @param post_treatment_periods Number of post-treatment periods. Zero by default.
#' @param ... additional arguments
#'
#' @rdname GeoLift
#' @order 5
#'
#' @export
#' @importFrom graphics plot
plot.GeoLift <- function(x,
                         type = "Lift",
                         treatment_end_date = NULL,
                         frequency = "daily",
                         plot_start_date = NULL,
                         title = "",
                         subtitle = "",
                         notes = "",
                         post_treatment_periods = 0,
                         ...) {
  if (!inherits(x, "GeoLift")) {
    stop("object must be class GeoLift")
  }

  if (type == "TreatmentSchedule") {
    panelView::panelview(Y ~ D, data = x$data, index = c("location", "time"), pre.post = TRUE)
  } else if (tolower(type) %in% c("att", "incrementality")) {
    absolute_value.plot(
      GeoLift = x,
      treatment_end_date = treatment_end_date,
      frequency = frequency,
      plot_start_date = plot_start_date,
      plot_type = type,
      title = title,
      subtitle = subtitle,
      notes = notes,
      post_treatment_periods = post_treatment_periods,
      ...
    )
  } else if (tolower(type) == "lift") {
    Lift.plot(
      GeoLift = x,
      treatment_end_date = treatment_end_date,
      frequency = frequency,
      plot_start_date = plot_start_date,
      title = title,
      subtitle = subtitle,
      notes = notes,
      post_treatment_periods = post_treatment_periods,
      ...
    )
  } else {
    message("Error: Please select a correct plot type: TreatmentSchedule/Lift/ATT/Incrementality")
  }
}


#' Aggregate Lift plot function for GeoLift.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Aggregate Lift plot function for GeoLift.
#'
#' @param GeoLift GeoLift object.
#' @param treatment_end_date Character that represents a date in year-month=day format.
#' @param frequency Character that represents periodicity of time stamps. Can be either
#' weekly or daily. Defaults to daily.
#' @param plot_start_date Character that represents initial date of plot in year-month-day format.
#' @param title String for the title of the plot. Empty by default.
#' @param subtitle String for the subtitle of the plot. Empty by default.
#' @param notes String to add notes to the plot. Empty by default.
#' @param post_treatment_periods Number of post-treatment periods. Zero by default.
#' @param ... additional arguments
#'
#' @return
#' Aggregate Lift plot.
#'
#' @export
Lift.plot <- function(GeoLift,
                      treatment_end_date = NULL,
                      frequency = "daily",
                      plot_start_date = NULL,
                      title = "",
                      subtitle = "",
                      notes = "",
                      post_treatment_periods = 0,
                      ...) {
  treatment_obs <- as.data.frame(
    colMeans(
      matrix(
        GeoLift$y_obs,
        nrow = nrow(GeoLift$test_id),
        ncol = GeoLift$TreatmentEnd
      )
    )
  ) * nrow(GeoLift$test_id)
  colnames(treatment_obs) <- c("t_obs")

  q_treatment_locations <- length(GeoLift$test_id$name)
  df <- data.frame(
    t_obs = treatment_obs$t_obs,
    c_obs = GeoLift$y_hat * q_treatment_locations,
    c_obs_lower_bound = (GeoLift$y_hat - (GeoLift$summary$att$Estimate - GeoLift$summary$att$lower_bound)) * q_treatment_locations,
    c_obs_upper_bound = (GeoLift$y_hat + (GeoLift$summary$att$upper_bound - GeoLift$summary$att$Estimate)) * q_treatment_locations,
    Time = 1:length(treatment_obs$t_obs)
  )

  if (!is.null(treatment_end_date)) {
    plot_dates <- get_date_from_test_periods(GeoLift,
      treatment_end_date,
      post_treatment_periods = post_treatment_periods,
      frequency = frequency
    )
    df$Time <- plot_dates$date_vector
  } else {
    message(
      "You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter."
    )
    plot_dates <- list(
      treatment_start = GeoLift$TreatmentStart,
      treatment_end = GeoLift$TreatmentEnd
    )
  }

  if (post_treatment_periods < 0) {
    post_treatment_periods <- abs(post_treatment_periods)
  }

  if (!is.null(plot_start_date)) {
    if (is.null(treatment_end_date)) {
      stop("If you want to filter your dataset on a date, please specify treatment_end_date param so periods are converted to dates.")
    } else {
      df <- df[df$Time >= plot_start_date, ]
    }
  }

  if (nchar(title) == 0) {
    title <- "Observations per Timestamp and Test Group"
  }

  colors <- c("Treatment" = "#52854C", "Control" = "#7030A0")

  # Post Treatment Periods
  df$post_treatment <- "Treatment Period"
  if (post_treatment_periods > 0) {
    post_treatment_linetype <- "dashed"
    df$post_treatment[(nrow(df) - post_treatment_periods + 1):nrow(df)] <- "Post-treatment Period"
    df <- rbind(df, df[(nrow(df) - post_treatment_periods + 1), ])
    df$post_treatment[nrow(df)] <- "Treatment Period"
  } else {
    post_treatment_linetype <- "blank"
  }

  if (!is.null(treatment_end_date)) {
    plot_dates$treatment_end <- plot_dates$treatment_end + post_treatment_periods
  }

  ggplot(df, aes(x = Time, fill = post_treatment)) +
    geom_line(
      aes(y = c_obs, color = "Control"),
      linetype = "dashed", alpha = 1.5
    ) +
    geom_ribbon(aes(ymin = c_obs_lower_bound, ymax = c_obs_upper_bound), alpha = 0.2) +
    geom_line(
      aes(y = t_obs, color = "Treatment")
    ) +
    theme_minimal() +
    labs(
      y = "Actual values",
      x = "Periods",
      title = title,
      subtitle = subtitle,
      caption = notes,
      color = "Test group"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      title = element_blank()
    ) +
    geom_vline(xintercept = plot_dates$treatment_start, linetype = "dashed", alpha = 0.3) +
    geom_vline(
      xintercept = (plot_dates$treatment_end - abs(post_treatment_periods)),
      linetype = post_treatment_linetype, alpha = 0.3
    ) +
    scale_color_manual(values = colors) +
    scale_fill_manual(
      breaks = c("Post-treatment Period", "Treatment Period"),
      values = c("gray44", "#4B4196"),
      name = "fill"
    ) +
    guides(shape = guide_legend(order = 2), col = guide_legend(order = 1))
}


#' Daily Incrementality or ATT plot function for GeoLift output.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `absolute_value.plot` returns chart for daily absolute values using GeoLift output.
#'
#' @param GeoLift GeoLift object.
#' @param plot_type Can be either ATT or Incrementality.  Defaults to ATT.
#' @param treatment_end_date Character that represents a date in year-month-day format.
#' @param frequency Character that represents periodicity of time stamps. Can be either
#' weekly or daily. Defaults to daily.
#' @param plot_start_date Character that represents initial date of plot in year-month-day format.
#' @param title Character for the title of the plot. NULL by default.
#' @param subtitle Character for the subtitle of the plot. NULL by default.
#' @param notes String to add notes to the plot. Empty by default.
#' @param ROPE  A logic flag indicating whether to plot the region
#' of practical equivalence (ROPE) at a 90% percentile. FALSE by default.
#' @param post_treatment_periods Number of post-treatment periods. Zero by default.
#' @param ... additional arguments
#'
#' @return
#' Daily Incremental or ATT plot.
#'
#' @export
absolute_value.plot <- function(GeoLift,
                                plot_type = "ATT",
                                treatment_end_date = NULL,
                                frequency = "daily",
                                plot_start_date = NULL,
                                title = "",
                                subtitle = "",
                                notes = "",
                                ROPE = FALSE,
                                post_treatment_periods = 0,
                                ...) {
  df <- GeoLift$summary$att
  df <- df[, c("Time", "Estimate", "lower_bound", "upper_bound")]

  if (tolower(plot_type) == "incrementality") {
    q_treatment_locations <- length(GeoLift$test_id$name)
    df$Estimate <- df$Estimate * q_treatment_locations
    df$lower_bound <- df$lower_bound * q_treatment_locations
    df$upper_bound <- df$upper_bound * q_treatment_locations
    ylab <- "Incremental values"
    if (nchar(title) == 0) {
      title <- "Incremental Value per Timestamp"
    }
    if (nchar(subtitle) == 0) {
      subtitle <- "GeoLift Analysis"
    }
  } else if (tolower(plot_type) == "att") {
    ylab <- "Average ATT"
    if (nchar(title) == 0) {
      title <- "Average Effect on the Treated"
    }
    if (nchar(subtitle) == 0) {
      subtitle <- "Average Effect per Timestamp per Location in Treatment"
    }
  } else {
    stop("Please specify which plot type you would like: ATT or Incrementality.")
  }

  if (post_treatment_periods < 0) {
    post_treatment_periods <- abs(post_treatment_periods)
  }

  if (!is.null(treatment_end_date)) {
    plot_dates <- get_date_from_test_periods(GeoLift,
      treatment_end_date,
      post_treatment_periods = post_treatment_periods,
      frequency = frequency
    )
    df$Time <- plot_dates$date_vector
  } else {
    message(
      "You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter."
    )
    plot_dates <- list(
      treatment_start = GeoLift$TreatmentStart,
      treatment_end = GeoLift$TreatmentEnd
    )
  }
  if (!is.null(plot_start_date)) {
    if (is.null(treatment_end_date)) {
      stop("If you want to filter your dataset on a date, please specify treatment_end_date param so periods are converted to dates.")
    } else {
      df <- df[df$Time >= plot_start_date, ]
    }
  }

  # Compute ROPE
  if (is.numeric(plot_dates$treatment_start)) {
    rope_quantiles <- quantile(df[1:plot_dates$treatment_start - 1, 2], c(0.1, 0.9))
  } else {
    rope_quantiles <- quantile(df[1:(nrow(df) -
      as.numeric(plot_dates$treatment_end -
        plot_dates$treatment_start) -
      post_treatment_periods), 2], c(0.1, 0.9))
  }
  if (ROPE == TRUE) {
    rope_linetype <- "dashed"
  } else {
    rope_linetype <- "blank"
  }

  # Post Treatment Periods
  df$post_treatment <- "Treatment Period"
  if (post_treatment_periods > 0) {
    post_treatment_linetype <- "dashed"
    df$post_treatment[(nrow(df) - post_treatment_periods + 1):nrow(df)] <- "Post-treatment Period"
    df <- rbind(df, df[(nrow(df) - post_treatment_periods + 1), ])
    df$post_treatment[nrow(df)] <- "Treatment Period"
  } else {
    post_treatment_linetype <- "blank"
  }

  if (!is.null(treatment_end_date)) {
    plot_dates$treatment_end <- plot_dates$treatment_end + post_treatment_periods
  }


  ggplot(df, aes(x = Time, y = Estimate, fill = post_treatment)) +
    geom_line(linetype = "dashed", color = "#373472", size = 0.75) +
    geom_vline(xintercept = plot_dates$treatment_start, linetype = "dashed", alpha = 0.3) +
    geom_vline(
      xintercept = (plot_dates$treatment_end - post_treatment_periods),
      linetype = post_treatment_linetype, alpha = 0.3
    ) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    geom_segment(
      aes(
        y = rope_quantiles[[1]], yend = rope_quantiles[[1]],
        x = plot_dates$treatment_start, xend = plot_dates$treatment_end
      ),
      linetype = rope_linetype, colour = "darkgrey"
    ) +
    geom_segment(
      aes(
        y = rope_quantiles[[2]], yend = rope_quantiles[[2]],
        x = plot_dates$treatment_start, xend = plot_dates$treatment_end
      ),
      linetype = rope_linetype, colour = "darkgrey"
    ) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.2) + # 4B4196
    scale_fill_manual(
      breaks = c("Post-treatment Period", "Treatment Period"),
      values = c("gray44", "#4B4196"),
      name = "fill"
    ) +
    theme_minimal() +
    labs(
      y = ylab,
      x = "Periods",
      title = title,
      subtitle = subtitle,
      caption = notes
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      title = element_blank()
    )
}


#' Plot the accumulated lift effect.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Plot the accumulated lift effect.
#'
#' @inheritParams GeoLift
#' @param treatment_end_date Character that represents a date in year-month-day format.
#' @param frequency Character that represents periodicity of time stamps. Can be either
#' weekly or daily. Defaults to daily.
#' @param plot_start_date Character that represents initial date of plot in year-month-day format.
#' @param title Character for the title of the plot. NULL by default.
#' @param subtitle Character for the subtitle of the plot. NULL by default.
#' @param notes String to add notes to the plot. Empty by default.
#' @param ... additional arguments
#'
#' @return
#' A ggplot object that shows the accumulated lift per time period.
#'
#' @export
cumulative_value.plot <- function(data,
                                  locations,
                                  treatment_start_time,
                                  treatment_end_time,
                                  location_id = "location",
                                  time_id = "time",
                                  Y_id = "Y",
                                  X = c(), 
                                  alpha = 0.1, 
                                  model = "none", 
                                  fixed_effects = TRUE, 
                                  method = "conformal", 
                                  grid_size = 250, 
                                  stat_test = "Total",
                                  conformal_type = "iid", 
                                  ns = 1000, 
                                  treatment_end_date = NULL,
                                  frequency = "daily",
                                  plot_start_date = NULL,
                                  title = "",
                                  subtitle = "",
                                  notes = "",
                                  ...) {
  cumulative_lift_df <- cumulative_lift(
    data = data,
    locations = locations,
    treatment_start_time = treatment_start_time,
    treatment_end_time = treatment_end_time,
    location_id = location_id,
    time_id = time_id,
    Y_id = Y_id,
    X = X, 
    alpha = alpha, 
    model = model, 
    fixed_effects = fixed_effects, 
    method = method, 
    grid_size = grid_size, 
    stat_test = stat_test, 
    conformal_type = conformal_type, 
    ns = ns 
  )

  if (nchar(title) == 0) {
    title <- "Accumulated Incremental Value"
  }
  GeoLift <- list(TreatmentEnd = treatment_end_time, TreatmentStart = treatment_start_time)
  if (!is.null(treatment_end_date)) {
    plot_dates <- get_date_from_test_periods(GeoLift, treatment_end_date, frequency = frequency)
    cumulative_lift_df$Time <- plot_dates$date_vector
  } else {
    message(
      "You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter."
    )
    plot_dates <- list(
      treatment_start = GeoLift$TreatmentStart,
      treatment_end = GeoLift$TreatmentEnd
    )
  }
  if (!is.null(plot_start_date)) {
    if (is.null(treatment_end_date)) {
      stop("If you want to filter your dataset on a date, please specify treatment_end_date param so periods are converted to dates.")
    } else {
      cumulative_lift_df <- cumulative_lift_df[cumulative_lift_df$Time >= plot_start_date, ]
    }
  }
  ggplot(
    cumulative_lift_df,
    aes(x = Time, y = incremental, group = 1)
  ) +
    geom_line(linetype = "dashed", color = "#373472") +
    geom_ribbon(aes(ymin = incremental_lb, ymax = incremental_ub), alpha = 0.2, fill = "#4B4196") +
    theme_minimal() +
    labs(
      y = "Incremental Values",
      x = "Date",
      title = title,
      subtitle = subtitle,
      caption = notes
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_vline(xintercept = plot_dates$treatment_start, linetype = "dashed", alpha = 0.3)
}


#' @param x \code{GeoLiftMarketSelection()}
#' @param market_ID Numeric value indicating the market to be plotted. This
#' value should reflect a valid ID from the BestMarkets data frame of the
#' `GeoLiftMarketSelection` output.
#' @param print_summary Logic flag indicating whether to print model metrics
#' from the latest possible test. Set to TRUE by default.
#' @param actual_values Logic flag indicating whether to include in the plot
#' the actual values. TRUE by default.
#' @param smoothed_values Logic flag indicating whether to include in the plot
#' the smoothed values. TRUE by default.
#' @param show_mde Logic flag indicating whether to include in the plot
#' the positive and negative MDEs. FALSE by default.
#' @param breaks_x_axis Numeric value indicating the number of breaks in the
#' x-axis of the power plot. You may get slightly more or fewer breaks that
#' requested based on `breaks_pretty()`. Set to 10 by default.
#' @param ... additional arguments
#'
#' @rdname GeoLiftMarketSelection
#' @order 3
#'
#' @export
#' @importFrom graphics plot
plot.GeoLiftMarketSelection <- function(x,
                                        market_ID = 0,
                                        print_summary = TRUE,
                                        actual_values = TRUE,
                                        smoothed_values = TRUE,
                                        show_mde = FALSE,
                                        breaks_x_axis = 10,
                                        ...) {
  if (!inherits(x, "GeoLiftMarketSelection")) {
    stop("object must be class GeoLiftMarketSelection")
  }

  # Will plot the lift plot with the MDE and the power curve
  if (!(market_ID %in% x$BestMarkets$ID)) {
    stop("Please enter a valid ID.")
  }

  Market <- x$BestMarkets %>% dplyr::filter(ID == market_ID)

  locs_aux <- unlist(strsplit(stringr::str_replace_all(Market$location, ", ", ","), split = ","))
  max_time <- max(x$parameters$data$time)

  data_lifted <- x$parameters$data
  data_lifted$Y[data_lifted$location %in% locs_aux &
    data_lifted$time >= max_time - Market$duration + 1] <-
    data_lifted$Y[data_lifted$location %in% locs_aux &
      data_lifted$time >= max_time - Market$duration + 1] * (1 + Market$EffectSize)

  if (tolower(x$parameters$side_of_test) == "two_sided") {
    stat_test <- "Total"
  } else {
    if (Market$EffectSize < 0) {
      stat_test <- "Negative"
    } else if (Market$EffectSize > 0) {
      stat_test <- "Positive"
    }
  }

  lifted <- suppressMessages(GeoLift::GeoLift(
    Y_id = "Y",
    time_id = "time",
    location_id = "location",
    data = data_lifted,
    locations = locs_aux,
    treatment_start_time = max_time - Market$duration + 1,
    treatment_end_time = max_time,
    model = x$parameters$model,
    fixed_effects = x$parameters$fixed_effects,
    stat_test = stat_test
  ))

  if (print_summary) {
    message(paste0(
      "##################################",
      "\n#####   GeoLift Simulation   #####\n",
      "####  Simulating: ",
      100 * round(Market$EffectSize, 2),
      "% Lift  ####\n",
      "##################################"
    ))
    print(summary(lifted))
  }

  PowerPlot_data <- as.data.frame(x$PowerCurves %>% dplyr::filter(
    duration == Market$duration,
    location == Market$location
  ))

  class(PowerPlot_data) <- c("GeoLiftPower", class(PowerPlot_data))
  PowerPlot_graph <- plot(
    PowerPlot_data,
    actual_values = actual_values,
    smoothed_values = smoothed_values,
    show_mde = show_mde,
    breaks_x_axis = breaks_x_axis
  )

  suppressMessages(gridExtra::grid.arrange(
    plot(lifted),
    PowerPlot_graph,
    ncol = 1,
    nrow = 2,
    bottom = paste(
      "Rank:", Market$rank,
      "\n Locations:", Market$location,
      "\n Treatment Periods:", Market$duration,
      "\n Effect Size: ", Market$EffectSize
    )
  ))
}


#' Plotting function for historical correlations between the test market's
#' and total markets KPI.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `plotCorrels` takes a data frame used for GeoLift to generate
#' a plot that show historical similarities in KPI levels between the
#' test markets and the aggregation of all locations (total).
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param locs List of markets to use in the calculation of the correlations.
#' @param scaled A logic flag indicating whether to plot the scaled values of
#' the KPI metric (standardized by the largest historical value). Set to FALSE
#' by default to plot the observed KPI levels.
#' @param KPI_id Outcome variable.
#' @param dtw Emphasis on Dynamic Time Warping (DTW), dtw = 1 focuses exclusively
#' on this metric while dtw = 0 (default) relies on correlations only.
#'
#' @return
#' A plot of the historical values of the test market and the aggregation of
#' all control markets.
#'
#' @export
plotCorrels <- function(data,
                        locs = c(),
                        scaled = TRUE,
                        KPI_id = "",
                        dtw = 0) {
  data_aux <- AppendAgg(data, locs = locs)
  correl <- CorrelationCoefficient(data, locs = locs)

  if (scaled == TRUE) {
    data_aux$Yscaled <- 0
    data_aux$Yscaled[data_aux$location == "control_markets"] <- data_aux$Y[data_aux$location == "control_markets"] / max(data_aux[data_aux$location == "control_markets", ]$Y)
    data_aux$Yscaled[data_aux$location == "test_markets"] <- data_aux$Y[data_aux$location == "test_markets"] / max(data_aux[data_aux$location == "test_markets", ]$Y)
    Y_id <- "Yscaled"
  } else {
    Y_id <- "Y"
  }

  GeoPlot(data_aux,
    Y_id = Y_id,
    time_id = "time",
    location_id = "location",
    KPI_id = KPI_id,
    notes = paste0("Correlation: ", round(correl, 4))
  )
}


#' @param x \code{MultiCellMarketSelection()}
#' @param test_markets List of market IDs per cell. The list must contain exactly
#' k numeric values corresponding to the power analysis. The recommended layout is
#' `list(cell_1 = 1, cell2 = 1, cell3 = 1,...)`.
#' @param type Type of plot. By default "Lift" which plots the
#' incrementality on the outcome variable. If type is set to "ATT",
#' the average ATT is plotted. If type is set to "Incrementality",
#' daily incremental values are plotted.
#' @param treatment_end_date Character that represents a date in year-month=day format.
#' @param frequency Character that represents periodicity of time stamps. Can be either
#' weekly or daily. Defaults to daily.
#' @param plot_start_date Character that represents initial date of plot in year-month-day format.
#' @param post_treatment_periods Number of post-treatment periods. Zero by default.
#' @param title String for the title of the plot. Empty by default.
#' @param stacked Logic flag indicating whether to stack all the Multi-Cell plots
#' together vertically or to output each one of them separately. Set to TRUE by
#' default.
#' @param ... additional arguments
#'
#' @rdname MultiCellMarketSelection
#' @order 3
#' @export
#' @importFrom graphics plot
plot.MultiCellMarketSelection <- function(x,
                                          test_markets = list(),
                                          type = "Lift",
                                          treatment_end_date = NULL,
                                          frequency = "daily",
                                          plot_start_date = NULL,
                                          post_treatment_periods = 0,
                                          title = "",
                                          stacked = TRUE,
                                          ...) {
  if (!inherits(x, "MultiCellMarketSelection")) {
    stop("object must be class MultiCellMarketSelection")
  }

  if (length(test_markets) != length(x$Models)) {
    stop("\nMake sure an ID is provided for each cell in the analysis.")
  }

  if (!(all(sapply(test_markets, is.numeric)))) {
    stop("\nMake sure all input IDs in test_markets are numeric.")
  }

  if (!(tolower(type) %in% c("lift", "att", "treatmentschedule"))) {
    stop("\nPlease specify a valid geolift_type test ('Lift', 'ATT', 'TreatmentSchedule').")
  }

  plots <- list()

  for (cell in 1:length(x$Models)) {
    Market <- x$Models[[cell]]$BestMarkets %>% dplyr::filter(ID == test_markets[cell])
    locs_aux <- unlist(strsplit(stringr::str_replace_all(Market$location, ", ", ","), split = ","))
    max_time <- max(x$Models[[cell]]$parameters$data$time)

    data_lifted <- x$Models[[cell]]$parameters$data
    data_lifted$Y[data_lifted$location %in% locs_aux &
      data_lifted$time >= max_time - Market$duration + 1] <-
      data_lifted$Y[data_lifted$location %in% locs_aux &
        data_lifted$time >= max_time - Market$duration + 1] * (1 + Market$EffectSize)

    if (tolower(x$Models[[cell]]$parameters$side_of_test) == "two_sided") {
      stat_test <- "Total"
    } else {
      if (Market$EffectSize < 0) {
        stat_test <- "Negative"
      } else if (Market$EffectSize > 0) {
        stat_test <- "Positive"
      }
    }

    lifted <- suppressMessages(GeoLift::GeoLift(
      Y_id = "Y",
      time_id = "time",
      location_id = "location",
      data = data_lifted,
      locations = locs_aux,
      treatment_start_time = max_time - Market$duration + 1,
      treatment_end_time = max_time,
      model = x$Models[[cell]]$parameters$model,
      fixed_effects = x$Models[[cell]]$parameters$fixed_effects,
      stat_test = stat_test
    ))

    plots <- append(plots, list(lifted))
  }

  if (stacked) {
    aux <- lapply(plots, function(x) {
      plot(x,
        type = type,
        treatment_end_date = treatment_end_date,
        frequency = frequency,
        plot_start_date = plot_start_date,
        title = title,
        subtitle = paste0("Cell: ", paste(x$test_id$name, collapse = ", ")),
        post_treatment_periods = post_treatment_periods
      )
    })

    suppressMessages(gridExtra::grid.arrange(
      grobs = aux,
      ncol = 1,
      nrow = length(x$Models),
      bottom = paste("Note: Plots with applied MDE applied in the treatment period.")
    ))
  } else {
    for (cell in 1:length(x$Models)) {
      print(plot(plots[[cell]],
        type = type,
        treatment_end_date = treatment_end_date,
        frequency = frequency,
        plot_start_date = plot_start_date,
        title = title,
        subtitle = paste0(
          "Cell ", cell, ":\n", paste(plots[[cell]]$test_id$name, collapse = ", "),
          ":\nPlots with applied MDE in the Treatment period."
        ),
        post_treatment_periods = post_treatment_periods
      ))
    }
  }
}


#' @param x \code{MultiCellMarketSelection()}
#' @param actual_values Logic flag indicating whether to include in the plot
#' the actual values. TRUE by default.
#' @param smoothed_values Logic flag indicating whether to include in the plot
#' the smoothed values. TRUE by default.
#' @param show_mde Logic flag indicating whether to include in the plot
#' the positive and negative MDEs. FALSE by default.
#' @param breaks_x_axis Numeric value indicating the number of breaks in the
#' x-axis of the power plot. You may get slightly more or fewer breaks that
#' requested based on `breaks_pretty()`. Set to 10 by default.
#' @param stacked Logic flag indicating whether to stack all the Multi-Cell plots
#' together vertically or to output each one of them separately. Set to TRUE by
#' default.
#' @param ... additional arguments
#'
#' @rdname MultiCellPower
#' @order 3
#' @export
#' @importFrom graphics plot
plot.MultiCellPower <- function(x,
                                actual_values = TRUE,
                                smoothed_values = FALSE,
                                show_mde = TRUE,
                                breaks_x_axis = 10,
                                stacked = TRUE,
                                ...) {
  if (!inherits(x, "MultiCellPower")) {
    stop("object must be class MultiCellPower")
  }

  if (stacked) {
    aux <- lapply(x$PowerCurves, function(x) {
      plot(x,
        actual_values = actual_values,
        smoothed_values = smoothed_values,
        show_mde = show_mde,
        breaks_x_axis = breaks_x_axis,
        notes = paste0("Cell : ", x[1, 1])
      )
    })

    suppressMessages(gridExtra::grid.arrange(
      grobs = aux,
      ncol = 1,
      nrow = length(x$PowerCurves)
    ))
  } else {
    for (cell in 1:length(x$PowerCurves)) {
      print(plot(x$PowerCurves[[cell]],
        actual_values = actual_values,
        smoothed_values = smoothed_values,
        show_mde = show_mde,
        breaks_x_axis = breaks_x_axis,
        notes = paste0("Cell ", cell, ": ", x$PowerCurves[[cell]][1, 1])
      ))
    }
  }
}


#' @param x \code{GeoLiftMultiCell()}
#' @param type Type of plot. By default "Lift" which plots the
#' incrementality on the outcome variable. If type is set to "ATT",
#' the average ATT is plotted. If type is set to "Incrementality",
#' daily incremental values are plotted.
#' @param treatment_end_date Character that represents a date in year-month=day format.
#' @param frequency Character that represents periodicity of time stamps. Can be either
#' weekly or daily. Defaults to daily.
#' @param title String for the title of the plot. Empty by default.
#' @param plot_start_date Character that represents initial date of plot in year-month-day format.
#' @param post_treatment_periods Number of post-treatment periods. Zero by default.
#' @param stacked Logic flag indicating whether to stack all the Multi-Cell plots
#' together vertically or to output each one of them separately. Set to TRUE by
#' default.
#' @param ... additional arguments
#'
#' @rdname GeoLiftMultiCell
#' @order 4
#' @export
#' @importFrom graphics plot
plot.GeoLiftMultiCell <- function(x,
                                  type = "Lift",
                                  treatment_end_date = NULL,
                                  frequency = "daily",
                                  plot_start_date = NULL,
                                  title = "",
                                  post_treatment_periods = 0,
                                  stacked = TRUE,
                                  ...) {
  if (!inherits(x, "GeoLiftMultiCell")) {
    stop("object must be class GeoLiftMultiCell")
  }

  if (!(tolower(type) %in% c("lift", "att", "treatmentschedule", "incrementality"))) {
    stop("\nPlease specify a valid geolift_type test ('Lift', 'ATT', 'TreatmentSchedule', 'Incrementality').")
  }

  if (stacked) {
    aux <- lapply(x$results, function(x) {
      plot(x,
        type = type,
        treatment_end_date = treatment_end_date,
        frequency = frequency,
        plot_start_date = plot_start_date,
        title = title,
        subtitle = paste0("Cell: ", paste(x$test_id$name, collapse = ", ")),
        post_treatment_periods = post_treatment_periods
      )
    })

    suppressMessages(gridExtra::grid.arrange(
      grobs = aux,
      ncol = 1,
      nrow = length(x$results)
    ))
  } else {
    for (cell in 1:length(x$results)) {
      print(plot(x$results[[cell]],
        type = type,
        treatment_end_date = treatment_end_date,
        frequency = frequency,
        plot_start_date = plot_start_date,
        title = title,
        subtitle = paste0("Cell ", cell, ":\n", paste(x$results[[cell]]$test_id$name, collapse = ", ")),
        post_treatment_periods = post_treatment_periods
      ))
    }
  }
}
