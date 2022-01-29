# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function GeoPlot, plot.GeoLiftPower.


#' Plotting function for Exploratory Analysis.
#'
#' @description
#'
#' \code{GeoPlot} takes a data frame used for GeoLift to generate
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
#'
#' @return
#' A plot of each location's time series.
#'
#' @export
GeoPlot <- function(data,
                    Y_id = "Y",
                    time_id = "time",
                    location_id = "location",
                    treatment_start = 0) {
  if (treatment_start == 0) {
    size_vline <- 0
  } else {
    size_vline <- 0.8
  }

  p <- ggplot(data, aes(y = !!sym(Y_id), x = !!sym(time_id), colour = !!sym(location_id), label = !!sym(location_id))) +
    geom_line(show.legend = FALSE) +
    geom_vline(xintercept = treatment_start, linetype = "dashed", size = size_vline, color = "grey35") +
    geom_dl(aes(label = !!sym(location_id)), method = list(dl.combine("last.points"), cex = 0.8)) +
    xlim(1, 1.15 * (max(data[[time_id]]))) +
    ylab("") +
    theme_minimal()

  print(p)
}


#' Plotting function for GeoLiftPower.
#'
#' @description
#'
#' Plotting function for \code{GeoLiftPower}. The function smooths the power curve
#' for ease of interpretation.
#'
#' @param x GeoLiftPower object.
#' @param power Power level. By default 0.8.
#' @param table Plot the table of power estimates. TRUE by default.
#' @param actual_values Logic flag indicating whether to include in the plot
#' the actual values in addition to the smoothed values.
#' @param ... additional arguments
#'
#' @return
#' GeoLiftPower plot.
#'
#' @export
plot.GeoLiftPower <- function(x,
                              power = 0.8,
                              table = TRUE,
                              actual_values = FALSE,
                              ...) {
  if (!inherits(x, "GeoLiftPower")) {
    stop("object must be class GeoLiftPower")
  }

  treatment_periods <- unique(x$duration)
  lift <- unique(x$lift)

  # NewChange: Standardize Plots
  PowerPlot <- x %>%
    dplyr::group_by(duration, lift) %>%
    # dplyr::mutate(power = 1 - pvalue) %>%
    # dplyr::summarise(power = mean(power))
    dplyr::summarise(power = mean(pow))

  spending <- x %>%
    dplyr::group_by(duration, lift) %>%
    dplyr::summarize(inv = mean(investment))

  if (table == TRUE) {
    print(as.data.frame(PowerPlot))
  }

  if (actual_values == FALSE) {
    if (sum(spending$inv > 0)) {
      for (dur in unique(PowerPlot$duration)) {
        PowerPlot_aux <- as.data.frame(PowerPlot %>% dplyr::filter(duration == dur))

        CostPerLift <- as.numeric(x %>%
          dplyr::filter(duration == dur, lift > 0) %>%
          dplyr::mutate(AvgCost = investment / lift) %>%
          dplyr::summarise(mean(AvgCost)))

        PowerPlot_graph <- ggplot(PowerPlot_aux, aes(x = lift, y = power)) +
          geom_smooth(formula = y ~ x, color = "indianred3", method = "loess", se = FALSE) +
          scale_x_continuous(sec.axis = sec_axis(~ . * CostPerLift, name = "Estimated Investment")) +
          ylim(0, 1) +
          geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey") +
          labs(title = paste0("Treatment Periods: ", dur), x = "Effect Size", y = "Power") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))

        plot(PowerPlot_graph)
      }
    } else {
      for (dur in unique(PowerPlot$duration)) {
        PowerPlot_aux <- as.data.frame(PowerPlot %>% dplyr::filter(duration == dur))

        PowerPlot_graph <- ggplot(PowerPlot_aux, aes(x = lift, y = power)) +
          geom_smooth(formula = y ~ x, color = "indianred3", method = "loess", se = FALSE) +
          ylim(0, 1) +
          geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey") +
          labs(title = paste0("Treatment Periods: ", dur), x = "Effect Size", y = "Power") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))

        plot(PowerPlot_graph)
      }
    }
  } else if (actual_values == TRUE) {
    if (sum(spending$inv > 0)) {
      for (dur in unique(PowerPlot$duration)) {
        PowerPlot_aux <- as.data.frame(PowerPlot %>% dplyr::filter(duration == dur))

        CostPerLift <- as.numeric(x %>%
          dplyr::filter(duration == dur, lift > 0) %>%
          dplyr::mutate(AvgCost = investment / lift) %>%
          dplyr::summarise(mean(AvgCost)))

        PowerPlot_graph <- ggplot(PowerPlot_aux, aes(x = lift, y = power)) +
          geom_smooth(formula = y ~ x, color = "indianred3", method = "loess", se = FALSE) +
          geom_line(color = "gray80", size = 0.62, alpha = 0.8) +
          scale_x_continuous(sec.axis = sec_axis(~ . * CostPerLift, name = "Estimated Investment")) +
          ylim(0, 1) +
          geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey") +
          labs(title = paste0("Treatment Periods: ", dur), x = "Effect Size", y = "Power") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))

        plot(PowerPlot_graph)
      }
    } else {
      for (dur in unique(PowerPlot$duration)) {
        PowerPlot_aux <- as.data.frame(PowerPlot %>% dplyr::filter(duration == dur))

        PowerPlot_graph <- ggplot(PowerPlot_aux, aes(x = lift, y = power)) +
          geom_smooth(formula = y ~ x, color = "indianred3", method = "loess", se = FALSE) +
          geom_line(color = "gray80", size = 0.62, alpha = 0.8) +
          ylim(0, 1) +
          geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey") +
          labs(title = paste0("Treatment Periods: ", dur), x = "Effect Size", y = "Power") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))

        plot(PowerPlot_graph)
      }
    }
  }
}
