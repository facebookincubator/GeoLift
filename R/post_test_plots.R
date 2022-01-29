# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function plot.GeoLift, Lift.plot, absolute_value.plot,
# cumulative_value.plot.


#' Plot for GeoLift.
#'
#' @description
#'
#' Plot for GeoLift objects.
#'
#' @param x GeoLift object.
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
#' @param ... additional arguments
#'
#' @return
#' GeoLift plot.
#'
#' @export
plot.GeoLift <- function(x,
                         type = "Lift",
                         treatment_end_date = NULL,
                         frequency = "daily",
                         plot_start_date = NULL,
                         title = "",
                         subtitle = "",
                         notes = "",
                         ...) {
  if (!inherits(x, "GeoLift")) {
    stop("object must be class GeoLift")
  }

  if (type == "TreatmentSchedule") {
    panelView(Y ~ D, data = x$data, index = c("location", "time"), pre.post = TRUE)
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
      ...
    )
  } else {
    message("Error: Please select a correct plot type: TreatmentSchedule/Lift/ATT/Incrementality")
  }
}


#' Aggregate Lift plot function for GeoLift.
#'
#' @description
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
    c_obs_lower_bound = treatment_obs$t_obs - GeoLift$summary$att$upper_bound * q_treatment_locations,
    c_obs_upper_bound = treatment_obs$t_obs - GeoLift$summary$att$lower_bound * q_treatment_locations,
    Time = 1:length(treatment_obs$t_obs)
  )

  if (!is.null(treatment_end_date)) {
    plot_dates <- get_date_from_test_periods(GeoLift, treatment_end_date, frequency = frequency)
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

  if (nchar(title) == 0) {
    title <- "Observations per Timestamp and Test Group"
  }

  colors <- c("Treatment" = "#52854C", "Control" = "#7030A0")

  ggplot(df, aes(x = Time)) +
    geom_line(
      aes(y = c_obs, color = "Control"),
      linetype = "dashed", alpha = 1.5
    ) +
    geom_ribbon(aes(ymin = c_obs_lower_bound, ymax = c_obs_upper_bound), alpha = 0.2, fill = "#4B4196") +
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
      text = element_text(size = 20),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_vline(xintercept = plot_dates$treatment_start, linetype = "dashed", alpha = 0.3) +
    scale_color_manual(values = colors)
}


#' Daily Incrementality or ATT plot function for GeoLift output.
#'
#' @description
#'
#' \code{absolute_value.plot} returns chart for daily absolute values using GeoLift output.
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

  if (!is.null(treatment_end_date)) {
    plot_dates <- get_date_from_test_periods(GeoLift, treatment_end_date, frequency = frequency)
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

  ggplot(df, aes(x = Time, y = Estimate)) +
    geom_line(linetype = "dashed", color = "#373472", size = 0.75) +
    geom_vline(xintercept = plot_dates$treatment_start, linetype = "dashed", alpha = 0.3) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.2, fill = "#4B4196") +
    theme_minimal() +
    labs(
      y = ylab,
      x = "Periods",
      title = title,
      subtitle = subtitle,
      caption = notes
    ) +
    theme(
      text = element_text(size = 20),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}


#' Plot the accumulated lift effect.
#'
#' @description
#' Plot the accumulated lift effect.
#'
#' @param data DataFrame that GeoLfit will use to determine a result.
#' Should be the output of \code{GeoDataRead}.
#' @param treatment_locations Vector of locations where the treatment was applied.
#' @param treatment_start_period Integer representing period where test started.
#' @param treatment_end_period Integer representing period where test finished.
#' @param Y_id Name of the outcome variable (String).
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
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
                                  treatment_locations,
                                  treatment_start_period,
                                  treatment_end_period,
                                  location_id = "location",
                                  time_id = "time",
                                  Y_id = "Y",
                                  treatment_end_date = NULL,
                                  frequency = "daily",
                                  plot_start_date = NULL,
                                  title = "",
                                  subtitle = "",
                                  notes = "",
                                  ...) {
  cumulative_lift_df <- cumulative_lift(
    data = data,
    treatment_locations = treatment_locations,
    treatment_start_period = treatment_start_period,
    treatment_end_period = treatment_end_period,
    location_id = location_id,
    time_id = time_id,
    Y_id = Y_id
  )

  if (nchar(title) == 0) {
    title <- "Accumulated Incremental Value"
  }
  GeoLift <- list(TreatmentEnd = treatment_end_period, TreatmentStart = treatment_start_period)
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
      text = element_text(size = 20),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    geom_vline(xintercept = plot_dates$treatment_start, linetype = "dashed", alpha = 0.3)
}
