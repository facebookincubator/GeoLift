# Copyright (c) Meta Platforms, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function MarketSelection, stochastic_market_selector, pvalueCalc,
# type_of_test, run_simulations, GeoLiftMarketSelection, GeoLiftPowerFinder,
# GeoLiftPower.search, NumberLocations, GeoLiftPower.


#' Market selection tool.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `MarketSelection` helps calculate the best markets based
#' on Dynamic Time Warping between the locations' time-series.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
#' @param Y_id Name of the outcome variable (String).
#' @param dtw Emphasis on Dynamic Time Warping (DTW), dtw = 1 focuses exclusively
#' on this metric while dtw = 0 (default) relies on correlations only.
#' @param exclude_markets A list of markets or locations that won't be considered
#' for the test market selection, but will remain in the pool of controls. Empty
#' list by default.
#'
#' @return
#' Matrix of the best markets. The second to last columns show
#' the best to worst market matches for the location in the first
#' column.
#'
#' @export
MarketSelection <- function(data,
                            location_id = "location",
                            time_id = "time",
                            Y_id = "Y",
                            dtw = 0,
                            exclude_markets = c()) {
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  data$location <- tolower(data$location)
  astime <- seq(as.Date("2000/1/1"), by = "day", length.out = max(data$time))
  data$astime <- astime[data$time]
  exclude_markets <- tolower(exclude_markets)

  # Check that the provided markets exist in the data.
  if (!all(exclude_markets %in% tolower(unique(data$location)))) {
    message(paste0(
      "Error: One or more markets in exclude_markets were not",
      " found in the data. Check the provided list and try again."
    ))
    return(NULL)
  }

  # Exclude markets input by user by filter them out from the uploaded file data
  if (length(exclude_markets) > 0) {
    data <- data[!data$location %in% exclude_markets, ]
  }

  if (dtw == 0) {
    best_controls <- MarketCorrelations(data)
  } else {
    # Find the best matches based on DTW
    mm <- MarketMatching::best_matches(
      data = data,
      id_variable = "location",
      date_variable = "astime",
      matching_variable = "Y",
      parallel = FALSE,
      warping_limit = 1,
      dtw_emphasis = dtw,
      start_match_period = min(data$astime),
      end_match_period = max(data$astime),
      matches = length(unique(data$location)) - 1
    )

    # Create a matrix with each row being the raked best controls for each location
    best_controls <- mm$BestMatches %>% tidyr::pivot_wider(
      id_cols = location,
      names_from = rank,
      values_from = BestControl
    )
  }

  best_controls <- as.matrix(best_controls)
  colnames(best_controls) <- NULL

  return(best_controls)
}


#' Stochastic Market Selector.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `stochastic_market_selector` selects the markets to be tested
#' by randomly sampling from the `similarity_matrix`.
#' It gets groups of 2 elements and samples one of them. It repeats
#' this process until the `treatment_size` is equal to the sample.
#'
#' @param treatment_size Is the amount of location units within the
#' treatment group.
#' @param similarity_matrix Matrix that sorts each location in terms
#' of descending correlation.
#' @param run_stochastic_process A logic flag indicating whether to select test
#' markets through random sampling of the the similarity matrix. Given that
#' interpolation biases may be relevant if the synthetic control matches
#' the characteristics of the test unit by averaging away large discrepancies
#' between the characteristics of the test and the units in the synthetic controls,
#' it is recommended to only use random sampling after making sure all units are
#' similar. This parameter is set by default to FALSE.
#'
#' @return
#' Returns a matrix of sampled combinations of treatments.
#' Each row represents a different treatment.
#'
#' @export
stochastic_market_selector <- function(treatment_size,
                                       similarity_matrix,
                                       run_stochastic_process = FALSE) {
  if (!run_stochastic_process) {
    message("\nDeterministic setup with ", treatment_size, " locations in treatment.")
    sample_matrix <- matrix(similarity_matrix[, 1:treatment_size], ncol = treatment_size)
  } else {
    message("\nRandom setup with ", treatment_size, " locations in treatment.")
    if (treatment_size > 0.5 * ncol(similarity_matrix)) {
      stop(paste0(
        "Treatment size (",
        treatment_size,
        ") should be <= to half the amount of units: ",
        ncol(similarity_matrix)
      ))
    }
    sample_size <- round(ncol(similarity_matrix) / treatment_size)
    sample_matrix <- c()
    i <- 0
    while ((i / 2) < treatment_size) {
      sampled_number <- sample(1:2, 1) + i
      sample_matrix <- cbind(sample_matrix, similarity_matrix[, sampled_number])
      i <- i + 2
    }
  }
  for (row in 1:nrow(sample_matrix)) { # Sort rows.
    sample_matrix[row, ] <- sort(sample_matrix[row, ])
  }
  return(matrix(unique(sample_matrix), ncol = treatment_size))
}


#' Decides type of statistical function being applied for Conformal
#' Inference.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `type_of_test` returns stat_func being used for GeoLiftPower;
#' GeoLiftPowerFinder & GeoLift.
#'
#' @param side_of_test A string indicating whether confidence will be determined
#' using a one sided or a two sided test.
#' \itemize{
#'          \item{"two_sided":}{ The test statistic is the sum of all treatment effects, i.e. sum(abs(x)). Defualt.}
#'          \item{"one_sided":}{ One-sided test against positive or negaative effects i.e.
#'          If the effect being applied is negative, then defaults to -sum(x). H0: ES >= 0; HA: ES < 0.
#'          If the effect being applied is positive, then defaults to sum(x). H0: ES <= 0; HA: ES > 0.}
#'          }
#' @param alternative_hypothesis A string indicating what is the alternative hypothesis being tested. Defaults to NULL.
#' \itemize{
#'          \item{"negative":}{ H0: ES >= 0; HA: ES < 0.}
#'          \item{"positive":}{ H0: ES <= 0; HA: ES > 0.}
#' }
#' @return
#' Statistical function being used to sum ATT effects over all treatment periods.
#'
#' @export
type_of_test <- function(side_of_test = "two_sided", alternative_hypothesis = NULL) {
  if (side_of_test == "two_sided") {
    stat_func <- function(x) sum(abs(x))
  } else if (side_of_test == "one_sided") {
    if (is.null(alternative_hypothesis)) {
      stop("If running a one sided test, please define alternative_hypotehsis parameter.
  Either 'positive' or 'negative'")
    }
    if (tolower(alternative_hypothesis) == "negative") {
      stat_func <- function(x) -sum(x)
    } else if (tolower(alternative_hypothesis) == "positive") {
      stat_func <- function(x) sum(x)
    } else {
      stop("Please define a valid alternative_hypothesis. Can be either {'Negative', 'Positive'}.")
    }
  } else {
    stop("Please define a valid side_of_test. Can be either {'one_sided', 'two_sided'}.")
  }

  return(stat_func)
}


#' Calculate p-value for GeoLift.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `pvalueCalc` calculates the p-value for a GeoLift object.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param sim Time simulation index.
#' @param max_time Treatment end index.
#' @param tp Time period index.
#' @param es Effect Size.
#' @param locations List of test locations.
#' @param cpic Cost Per Incremental Conversion.
#' @param X List of names of covariates.
#' @param type Method of inference used in the analysis.
#'             pValue=Provides conformal inference to provide the aggregate
#'             p-value for the null hypothesis of no effect from the intervention.
#'             The Default type is pValue.
#'             Imbalance=Uses the model's Scaled L2 Imbalance metric.
#' @param normalize A logic flag indicating whether to scale the outcome which is
#' useful to accelerate computing speed when the magnitude of the data is large. The
#' default is FALSE.
#' @param fixed_effects A logic flag indicating whether to include unit fixed
#' effects in the model. Set to FALSE by default.
#' @param stat_func Function to compute test statistic. NULL by default.
#' @param model A string indicating the outcome model used in the Augmented Synthetic
#' Control Method. Set to Generalized Synthetic Controls "none" by default.
#' @param conformal_type Type of conformal inference used. Can be either "iid" for Independent and identically
#' distributed or "block" for moving block permutations. Set to "iid" by default.
#' @param ns Number of resamples for "iid" permutations if `conformal_type = "iid`. Set to 1000 by default.
#'
#' @return
#' List that contains:
#'          \itemize{
#'          \item{"location":}{ Test locations.}
#'          \item{"pvalue":}{ P Value.}
#'          \item{"tp":}{ Time period index.}
#'          \item{"es":}{ Effect Size used for the simulation.}
#'          \item{"treatment_start_time":}{ Treatment start time for the simulation}
#'          \item{"investment":}{ Estimated Investment}
#'          \item{"ScaledL2Imbalance":}{ Scaled L2 Imbalance metric}
#'         }
#'
#' @export
pvalueCalc <- function(data,
                       sim,
                       max_time,
                       tp,
                       es,
                       locations,
                       cpic,
                       X,
                       type = "pValue",
                       normalize = FALSE,
                       fixed_effects = FALSE,
                       stat_func = stat_func,
                       model = "none",
                       conformal_type = conformal_type,
                       ns = ns) {
  treatment_start_time <- max_time - tp - sim + 2
  treatment_end_time <- treatment_start_time + tp - 1
  pre_test_duration <- treatment_start_time - 1
  pre_treatment_start_time <- 1

  if (normalize == TRUE) {
    factor <- sd(as.matrix(data$Y))
    data$Y <- data$Y / factor
  }

  data_aux <- fn_treatment(data,
    locations = locations,
    treatment_start_time,
    treatment_end_time
  )

  data_aux$Y_inc <- data_aux$Y
  data_aux$Y_inc[data_aux$D == 1] <- data_aux$Y_inc[data_aux$D == 1] * (1 + es)

  if (length(X) == 0) {
    ascm_obj <- augsynth::augsynth(Y_inc ~ D,
      unit = location,
      time = time,
      data = data_aux,
      t_int = treatment_start_time,
      progfunc = model,
      scm = T,
      fixedeff = fixed_effects
    )
  } else if (length(X) > 0) {
    fmla <- as.formula(paste(
      "Y_inc ~ D |",
      sapply(list(X),
        paste,
        collapse = "+"
      )
    ))

    ascm_obj <- augsynth::augsynth(fmla,
      unit = location,
      time = time,
      data = data_aux,
      t_int = treatment_start_time,
      progfunc = model,
      scm = T,
      fixedeff = fixed_effects
    )
  }

  ave_treatment_convs <- sum(ascm_obj$data$y[which(ascm_obj$data$trt == 1), ]) / sum(ascm_obj$data$trt == 1)
  ave_pred_control_convs <- predict(ascm_obj)[treatment_start_time:treatment_end_time]
  ave_incremental_convs <- ave_treatment_convs - sum(ave_pred_control_convs)

  att_estimator <- ave_incremental_convs / tp
  lift_estimator <- ave_incremental_convs / sum(ave_pred_control_convs)

  if (type == "pValue") {
    wide_data <- ascm_obj$data
    new_wide_data <- wide_data
    new_wide_data$X <- cbind(wide_data$X, wide_data$y)
    new_wide_data$y <- matrix(1, nrow = nrow(wide_data$X), ncol = 1)
    pVal <- augsynth:::compute_permute_pval(
      wide_data = new_wide_data,
      ascm = ascm_obj,
      h0 = 0,
      post_length = ncol(wide_data$y),
      type = conformal_type,
      q = 1,
      ns = ns,
      stat_func = stat_func
    )
    ScaledL2Imbalance <- ascm_obj$scaled_l2_imbalance
  } else if (type == "Imbalance") {
    pVal <- NA
    ScaledL2Imbalance <- ascm_obj$scaled_l2_imbalance
  } else {
    message(paste0("ERROR: Please input a valid type: pValue or Imbalance."))
    pVal <- NA
    ScaledL2Imbalance <- NA
  }

  investment <- cpic * sum(data_aux$Y[data_aux$D == 1]) * (es)

  return(
    list(
      locations = c(paste(locations, collapse = ", ")),
      pvalue = pVal,
      treatment_periods = tp,
      effect_size = es,
      treatment_start_time = treatment_start_time,
      investment = investment,
      scaled_l2_imbalance = ScaledL2Imbalance,
      att_estimator = att_estimator,
      lift_estimator = lift_estimator
    )
  )
}


#' Run simulations for treatment markets.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `run_simulations` computes simulations for each treatment market.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param treatment_combinations A matrix of treatment locations.
#' Each row symbolizes a combination, each column one element of the combination.
#' @param treatment_durations Expected durations of the experiment.
#' @param effect_sizes A vector of effect sizes to simulate.
#' @param side_of_test A string indicating whether confidence will be determined
#' using a one sided or a two sided test.
#' \itemize{
#'          \item{"two_sided":}{ The test statistic is the sum of all treatment effects, i.e. sum(abs(x)). Defualt.}
#'          \item{"one_sided":}{ One-sided test against positive or negaative effects i.e.
#'          If the effect being applied is negative, then defaults to -sum(x). H0: ES >= 0; HA: ES < 0.
#'          If the effect being applied is positive, then defaults to sum(x). H0: ES <= 0; HA: ES > 0.}
#'          }
#' @param conformal_type Type of conformal inference used. Can be either "iid" for Independent and identically
#' distributed or "block" for moving block permutations. Set to "iid" by default.
#' @param ns Number of resamples for "iid" permutations if `conformal_type = "iid`. Set to 1000 by default.
#' @param lookback_window A number indicating how far back in time the simulations
#' for the power analysis should go. For instance, a value equal to 5 will simulate
#' power for the last five possible tests. By default lookback_window = 1 which
#' will only execute the most recent test based on the data.
#' @param cpic Number indicating the Cost Per Incremental Conversion.
#' @param parallel A logic flag indicating whether to use parallel computing to
#' speed up calculations. Set to TRUE by default.
#' @param ProgressBar A logic flag indicating whether to display a progress bar
#' to track progress. Set to FALSE by default.
#' @param X List of names of covariates.
#' @param normalize A logic flag indicating whether to scale the outcome which is
#' useful to accelerate computing speed when the magnitude of the data is large. The
#' default is FALSE.
#' @param fixed_effects A logic flag indicating whether to include unit fixed
#' effects in the model. Set to TRUE by default.
#' @param model A string indicating the outcome model used to augment the Augmented
#' Synthetic Control Method. Augmentation through a prognostic function can improve
#' fit and reduce L2 imbalance metrics.
#' \itemize{
#'          \item{"None":}{ ASCM is not augmented by a prognostic function. Defualt.}
#'          \item{"Ridge":}{ Augments with a Ridge regression. Recommended to improve fit
#'                           for smaller panels (less than 40 locations and 100 time-stamps.))}
#'          \item{"GSYN":}{ Augments with a Generalized Synthetic Control Method. Recommended
#'                          to improve fit for larger panels (more than 40 locations and 100
#'                          time-stamps. }
#'          }
#'
#' @return
#' DataFrame that contains:
#'          \itemize{
#'          \item{"location":}{ Test locations.}
#'          \item{"pvalue":}{ P Value.}
#'          \item{"tp":}{ Time period index.}
#'          \item{"es":}{ Effect Size used for the simulation.}
#'          \item{"treatment_start_time":}{ Treatment start time for the simulation}
#'          \item{"investment":}{ Estimated Investment}
#'          \item{"ScaledL2Imbalance":}{ Scaled L2 Imbalance metric}
#'          \item{"att_estimator":}{ Detected Average Treatment on the Treated}
#'          \item{"detected_lift":}{ Detected % Lift by GeoLift for an effect size}
#'         }
#'
#' @export
run_simulations <- function(data,
                            treatment_combinations,
                            treatment_durations,
                            effect_sizes = 0,
                            side_of_test = "two_sided",
                            conformal_type = conformal_type,
                            ns = ns,
                            lookback_window = 1,
                            parallel = TRUE,
                            ProgressBar = FALSE,
                            cpic = 0,
                            X = c(),
                            normalize = FALSE,
                            fixed_effects = TRUE,
                            model = "none") {
  `%parallel_connector%` <- ifelse(parallel, foreach::`%dopar%`, foreach::`%do%`)
  results <- data.frame(matrix(ncol = 10, nrow = 0))
  colnames(results) <- c(
    "location",
    "pvalue",
    "duration",
    "EffectSize",
    "treatment_start",
    "Investment",
    "cpic",
    "ScaledL2Imbalance",
    "att_estimator",
    "detected_lift"
  )
  param_combination <- expand.grid(
    effect_sizes,
    treatment_durations,
    1:lookback_window,
    1:nrow(as.matrix(treatment_combinations))
  )

  colnames(param_combination) <- c(
    "effect_size",
    "treatment_duration",
    "lookback_window",
    "treatment_combination_row"
  )

  combine_function <- function(iterator) {
    pb <- txtProgressBar(min = 1, max = iterator - 1, style = 3)
    count <- 0
    function(...) {
      count <<- count + length(list(...)) - 1
      setTxtProgressBar(pb, count)
      flush.console()
      cbind(...) # this can feed into .combine option of foreach
    }
  }
  simulation_results <- foreach(
    effect_size = param_combination$effect_size,
    treatment_duration = param_combination$treatment_duration,
    sim = param_combination$lookback_window,
    test = param_combination$treatment_combination_row,
    .combine = ifelse(
      ProgressBar,
      combine_function(nrow(param_combination)),
      cbind
    ),
    .errorhandling = "stop",
    .verbose = FALSE
  ) %parallel_connector% {
    suppressMessages(pvalueCalc(
      data = data,
      sim = sim,
      max_time = max(data$time),
      tp = treatment_duration,
      es = effect_size,
      locations = as.list(as.matrix(treatment_combinations)[test, ]),
      cpic = cpic,
      X,
      type = "pValue",
      normalize = normalize,
      fixed_effects = fixed_effects,
      model = model,
      stat_func = type_of_test(
        side_of_test = side_of_test,
        alternative_hypothesis = ifelse(
          effect_size > 0, "positive", "negative"
        )
      ),
      conformal_type = conformal_type,
      ns = ns
    ))
  }

  if (is.null(dim(simulation_results))) {
    simulation_results <- matrix(
      simulation_results,
      nrow = length(names(simulation_results))
    )
  }
  for (i in 1:ncol(simulation_results)) {
    results <- rbind(
      results,
      data.frame(
        location = simulation_results[[1, i]],
        pvalue = as.numeric(simulation_results[[2, i]]),
        duration = as.numeric(simulation_results[[3, i]]),
        EffectSize = as.numeric(simulation_results[[4, i]]),
        treatment_start = as.numeric(simulation_results[[5, i]]),
        Investment = as.numeric(simulation_results[[6, i]]),
        cpic = cpic,
        ScaledL2Imbalance = as.numeric(simulation_results[[7, i]]),
        att_estimator = as.numeric(simulation_results[[8, i]]),
        detected_lift = as.numeric(simulation_results[[9, i]])
      )
    )
  }
  return(results)
}


#' Power calculations for unknown test market locations, number of
#' test markets, and test duration.
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Development on `GeoLiftPowerFinder()` is complete.
#' We recommend switching to `GeoLiftMarketSelection()`
#' for new code, which is easier to use, more featureful,
#' and still under active development.
#'
#' `GeoLiftPowerFinder` provides power calculations for unknown
#' test markets, number of test locations, and test duration.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param treatment_periods List of treatment periods to calculate power for.
#' @param N List of number of test markets to calculate power for.
#' @param X List of names of covariates.
#' @param Y_id Name of the outcome variable (String).
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
#' @param effect_size A vector of effect sizes to test by default a
#' sequence between 0 - 25 percent in 5 percent increments: seq(0,0.25,0.05).
#' Make sure that the sequence includes zero.
#' @param top_results Number of results to display.
#' @param alpha Significance Level. By default 0.1.
#' @param normalize A logic flag indicating whether to scale the outcome which is
#' useful to accelerate computing speed when the magnitude of the data is large. The
#' default is FALSE.
#' @param model A string indicating the outcome model used to augment the Augmented
#' Synthetic Control Method. Augmentation through a prognostic function can improve
#' fit and reduce L2 imbalance metrics.
#' \itemize{
#'          \item{"None":}{ ASCM is not augmented by a prognostic function. Defualt.}
#'          \item{"Ridge":}{ Augments with a Ridge regression. Recommended to improve fit
#'                           for smaller panels (less than 40 locations and 100 time-stamps.))}
#'          \item{"GSYN":}{ Augments with a Generalized Synthetic Control Method. Recommended
#'                          to improve fit for larger panels (more than 40 locations and 100
#'                          time-stamps. }
#'          }
#' @param fixed_effects A logic flag indicating whether to include unit fixed
#' effects in the model. Set to TRUE by default.
#' @param dtw Emphasis on Dynamic Time Warping (DTW), dtw = 1 focuses exclusively
#' on this metric while dtw = 0 (default) relies on correlations only.
#' @param ProgressBar A logic flag indicating whether to display a progress bar
#' to track progress. Set to FALSE by default.
#' @param plot_best A logic flag indicating whether to plot the best 4 tests for
#' each treatment length. Set to FALSE by default.
#' @param run_stochastic_process A logic flag indicating whether to select test
#' markets through random sampling of the the similarity matrix. Given that
#' interpolation biases may be relevant if the synthetic control matches
#' the characteristics of the test unit by averaging away large discrepancies
#' between the characteristics of the test and the units in the synthetic controls,
#' it is recommended to only use random sampling after making sure all units are
#' similar. This parameter is set by default to FALSE.
#' @param parallel A logic flag indicating whether to use parallel computing to
#' speed up calculations. Set to TRUE by default.
#' @param parallel_setup A string indicating parallel workers set-up.
#' Set to "sequential" by default.
#' @param side_of_test A string indicating whether confidence will be determined
#' using a one sided or a two sided test.
#' \itemize{
#'          \item{"two_sided":}{ The test statistic is the sum of all treatment effects, i.e. sum(abs(x)). Defualt.}
#'          \item{"one_sided":}{ One-sided test against positive or negaative effects i.e.
#'          If the effect being applied is negative, then defaults to -sum(x). H0: ES >= 0; HA: ES < 0.
#'          If the effect being applied is positive, then defaults to sum(x). H0: ES <= 0; HA: ES > 0.}
#'          }
#' @param import_augsynth_from Points to where the augsynth package
#' should be imported from to send to the nodes.
#' @param import_tidyr_from Points to where the tidyr package
#' should be imported from to send to the nodes.
#'
#' @return
#' Data frame with the ordered list of best locations and their
#' average power.
#'
#' @export
GeoLiftPowerFinder <- function(data,
                               treatment_periods,
                               N = 1,
                               X = c(),
                               Y_id = "Y",
                               location_id = "location",
                               time_id = "time",
                               effect_size = seq(0, 0.25, 0.05),
                               top_results = 5,
                               alpha = 0.1,
                               normalize = FALSE,
                               model = "none",
                               fixed_effects = TRUE,
                               dtw = 0,
                               ProgressBar = FALSE,
                               plot_best = FALSE,
                               run_stochastic_process = FALSE,
                               parallel = TRUE,
                               parallel_setup = "sequential",
                               side_of_test = "two_sided",
                               import_augsynth_from = "library(augsynth)",
                               import_tidyr_from = "library(tidyr)") {
  if (parallel == TRUE) {
    cl <- build_cluster(
      parallel_setup = parallel_setup,
      import_augsynth_from = import_augsynth_from,
      import_tidyr_from = import_tidyr_from
    )
  }

  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)

  # Small Pre-treatment Periods
  if (max_time / max(treatment_periods) < 4) {
    message(paste0("Caution: Small pre-treatment period!.
                   \nIt's recommended to have at least 4x pre-treatment periods for each treatment period.\n"))
  }

  results <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(results) <- c(
    "location",
    "pvalue",
    "duration",
    "EffectSize",
    "treatment_start",
    "ScaledL2Imbalance"
  )

  BestMarkets <- MarketSelection(data,
    location_id = "location",
    time_id = "time",
    Y_id = "Y",
    dtw = dtw
  )

  N <- limit_test_markets(BestMarkets, N, run_stochastic_process)

  # Aggregated Y Per Location
  AggYperLoc <- data %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(Total_Y = sum(Y))

  num_sim <- length(N) * length(treatment_periods) * length(effect_size)
  if (ProgressBar == TRUE) {
    pb <- progress::progress_bar$new(
      format = "  Running Simulations [:bar] :percent",
      total = num_sim,
      clear = FALSE,
      width = 60
    )
  } else {
    pb <- NULL
  }

  message("Finding the best markets for your experiment.")

  for (n in N) {
    BestMarkets_aux <- stochastic_market_selector(
      n,
      BestMarkets,
      run_stochastic_process = run_stochastic_process
    )

    partial_results <- run_simulations(
      data = data,
      treatment_combinations = BestMarkets_aux,
      treatment_durations = treatment_periods,
      effect_sizes = effect_size,
      side_of_test = side_of_test,
      lookback_window = 1,
      parallel = parallel,
      ProgressBar = ProgressBar,
      cpic = 0,
      X = X,
      normalize = normalize,
      fixed_effects = fixed_effects,
      model = model
    )
    results <- rbind(results, partial_results)
  }

  if (parallel == TRUE) {
    parallel::stopCluster(cl)
  }

  # Sort Locations alphabetically
  results$location <- strsplit(stringr::str_replace_all(results$location, ", ", ","), split = ",")
  results$location <- lapply(results$location, sort)
  results$location <- lapply(results$location, function(x) paste(x, collapse = ", "))
  results$location <- unlist(results$location)

  results <- results %>%
    dplyr::mutate(significant = ifelse(pvalue < alpha, 1, 0)) %>%
    dplyr::filter(significant > 0) %>%
    dplyr::distinct()

  resultsM <- results %>%
    dplyr::filter(EffectSize != 0) %>%
    dplyr::group_by(location, duration) %>%
    dplyr::slice(which.min(abs(EffectSize)))

  # Add Percent of Y in test markets
  resultsM$ProportionTotal_Y <- 1
  resultsM$Locs <- strsplit(stringr::str_replace_all(resultsM$location, ", ", ","), split = ",")

  for (row in 1:nrow(resultsM)) {
    resultsM$ProportionTotal_Y[row] <- as.numeric(AggYperLoc %>%
      dplyr::filter(location %in% resultsM$Locs[[row]]) %>%
      dplyr::summarize(total = sum(Total_Y))) /
      sum(AggYperLoc$Total_Y)
  }

  # Remove any duplicates
  resultsM <- resultsM %>%
    dplyr::group_by(location, duration) %>%
    dplyr::slice_min(order_by = pvalue, n = 1)

  resultsM$abs_lift_in_zero <- round(abs(resultsM$detected_lift - resultsM$EffectSize), 3)

  resultsM <- as.data.frame(resultsM) %>%
    dplyr::mutate(
      rank_mde = dplyr::dense_rank(abs(EffectSize)),
      rank_pvalue = dplyr::dense_rank(pvalue),
      rank_abszero = dplyr::dense_rank(abs_lift_in_zero)
    )

  resultsM$rank <- rank(
    rowMeans(resultsM[, c("rank_mde", "rank_pvalue", "rank_abszero")]),
    ties.method = "min"
  )

  resultsM <- resultsM %>%
    dplyr::mutate(
      rank_mde = NULL,
      rank_pvalue = NULL,
      rank_abszero = NULL,
      Locs = NULL,
      treatment_start = NULL,
      significant = NULL
    ) %>%
    dplyr::arrange(rank)

  class(results) <- c("GeoLiftPowerFinder", class(resultsM))

  if (top_results > nrow(resultsM)) {
    top_results <- nrow(resultsM)
  }

  if (plot_best == TRUE) {
    message("Calculating GeoLifts to plot top results.")
    for (tp in treatment_periods) {
      BestResults <- resultsM %>%
        dplyr::filter(duration == tp) %>%
        dplyr::arrange(rank)

      bestmodels <- list()
      for (i in 1:4) {
        locs_aux <- unlist(strsplit(stringr::str_replace_all(BestResults$location[i], ", ", ","), split = ","))

        data_lifted <- data

        data_lifted$Y[data_lifted$location %in% locs_aux &
          data_lifted$time >= max_time - tp + 1] <-
          data_lifted$Y[data_lifted$location %in% locs_aux &
            data_lifted$time >= max_time - tp + 1] * (1 + BestResults$EffectSize[i])

        bestmodels[[i]] <- suppressMessages(GeoLift::GeoLift(
          Y_id = "Y",
          time_id = "time",
          location_id = "location",
          data = data_lifted,
          locations = locs_aux,
          treatment_start_time = max_time - tp + 1,
          treatment_end_time = max_time,
          model = model,
          fixed_effects = fixed_effects
        ))
      }
      suppressMessages(gridExtra::grid.arrange(
        plot(bestmodels[[1]], notes = paste(
          "locations:", BestResults$location[1],
          "\n Treatment Periods:", tp, "\n Minimum Detectable Effect: ",
          BestResults$EffectSize[1],
          "\n Proportion Total Y: ", 100 * round(BestResults$ProportionTotal_Y[1], 3), "%"
        )),
        plot(bestmodels[[2]], notes = paste(
          "locations:", BestResults$location[2],
          "\n Treatment Periods:", tp, "\n Minimum Detectable Effect: ",
          BestResults$EffectSize[2],
          "\n Proportion Total Y: ", 100 * round(BestResults$ProportionTotal_Y[2], 3), "%"
        )),
        plot(bestmodels[[3]], notes = paste(
          "locations:", BestResults$location[3],
          "\n Treatment Periods:", tp, "\n Minimum Detectable Effect: ",
          BestResults$EffectSize[3],
          "\n Proportion Total Y: ", 100 * round(BestResults$ProportionTotal_Y[3], 3), "%"
        )),
        plot(bestmodels[[4]], notes = paste(
          "locations:", BestResults$location[4],
          "\n Treatment Periods:", tp, "\n Minimum Detectable Effect: ",
          BestResults$EffectSize[4],
          "\n Proportion Total Y: ", 100 * round(BestResults$ProportionTotal_Y[4], 3), "%"
        )),
        ncol = 2
      ))
    }
  }

  resultsM <- resultsM %>% dplyr::rename(MinDetectableEffect = EffectSize)

  return(resultsM)
}


#' Power calculations for unknown test market locations, number of
#' test markets, and test duration.
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Development on `GeoLiftPower.search()` is complete.
#' We recommend switching to `GeoLiftMarketSelection()`
#' for new code, which is easier to use, more featureful,
#' and still under active development.
#'
#' `GeoLiftPower.search` provides power calculations for unknown
#' test markets, number of test locations, and test duration.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param treatment_periods List of treatment periods to calculate power for.
#' @param N List of number of test markets to calculate power for.
#' @param lookback_window A number indicating how far back in time the simulations
#' for the power analysis should go. For instance, a value equal to 5 will simulate
#' power for the last five possible tests. By default lookback_window = 1 which
#' will only execute the most recent test based on the data.
#' @param X List of names of covariates.
#' @param Y_id Name of the outcome variable (String).
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
#' @param top_results Number of results to display.
#' @param alpha Significance Level. By default 0.1.
#' @param type Method of inference used in the analysis.
#'             pValue=Provides conformal inference to provide the aggregate
#'             p-value for the null hypothesis of no effect from the intervention.
#'             The Default type is pValue.
#'             Imbalance=Uses the model's Scaled L2 Imbalance metric.
#' @param normalize A logic flag indicating whether to scale the outcome which is
#' useful to accelerate computing speed when the magnitude of the data is large. The
#' default is FALSE.
#' @param model A string indicating the outcome model used to augment the Augmented
#' Synthetic Control Method. Augmentation through a prognostic function can improve
#' fit and reduce L2 imbalance metrics.
#' \itemize{
#'          \item{"None":}{ ASCM is not augmented by a prognostic function. Defualt.}
#'          \item{"Ridge":}{ Augments with a Ridge regression. Recommended to improve fit
#'                           for smaller panels (less than 40 locations and 100 time-stamps.))}
#'          \item{"GSYN":}{ Augments with a Generalized Synthetic Control Method. Recommended
#'                          to improve fit for larger panels (more than 40 locations and 100
#'                          time-stamps. }
#'          }
#' @param fixed_effects A logic flag indicating whether to include unit fixed
#' effects in the model. Set to TRUE by default.
#' @param stat_func Function to compute test statistic. NULL by default.
#' @param dtw Emphasis on Dynamic Time Warping (DTW), dtw = 1 focuses exclusively
#' on this metric while dtw = 0 (default) relies on correlations only.
#' @param ProgressBar A logic flag indicating whether to display a progress bar
#' to track progress. Set to FALSE by default.
#' @param run_stochastic_process A logic flag indicating whether to select test
#' markets through random sampling of the the similarity matrix. Given that
#' interpolation biases may be relevant if the synthetic control matches
#' the characteristics of the test unit by averaging away large discrepancies
#' between the characteristics of the test and the units in the synthetic controls,
#' it is recommended to only use random sampling after making sure all units are
#' similar. This parameter is set by default to FALSE.
#' @param parallel A logic flag indicating whether to use parallel computing to
#' speed up calculations. Set to TRUE by default.
#' @param parallel_setup A string indicating parallel workers set-up.
#' Set to "sequential" by default.
#' @param import_augsynth_from Points to where the augsynth package
#' should be imported from to send to the nodes.
#' @param import_tidyr_from Points to where the tidyr package
#' should be imported from to send to the nodes.
#'
#' @return
#' Data frame with the ordered list of best locations and their
#' average power.
#'
#' @export
GeoLiftPower.search <- function(data,
                                treatment_periods,
                                N = 1,
                                lookback_window = 1,
                                X = c(),
                                Y_id = "Y",
                                location_id = "location",
                                time_id = "time",
                                top_results = 5,
                                alpha = 0.1,
                                type = "pValue",
                                normalize = FALSE,
                                model = "none",
                                fixed_effects = TRUE,
                                stat_func = NULL,
                                dtw = 0,
                                ProgressBar = FALSE,
                                run_stochastic_process = FALSE,
                                parallel = TRUE,
                                parallel_setup = "sequential",
                                import_augsynth_from = "library(augsynth)",
                                import_tidyr_from = "library(tidyr)") {
  if (parallel == TRUE) {
    cl <- build_cluster(
      parallel_setup = parallel_setup,
      import_augsynth_from = import_augsynth_from,
      import_tidyr_from = import_tidyr_from
    )
  }

  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)

  # Small Pre-treatment Periods
  if (max_time / max(treatment_periods) < 4) {
    message(paste0("Caution: Small pre-treatment period!.
                   \nIt's recommended to have at least 4x pre-treatment periods for each treatment period.\n"))
  }

  results <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(results) <- c(
    "location",
    "pvalue",
    "duration",
    "treatment_start",
    "ScaledL2Imbalance"
  )

  BestMarkets <- MarketSelection(data,
    location_id = "location",
    time_id = "time",
    Y_id = "Y",
    dtw = dtw
  )

  N <- limit_test_markets(BestMarkets, N, run_stochastic_process)

  if (lookback_window <= 0) {
    lookback_window <- 1
  }

  # Aggregated Y Per Location
  AggYperLoc <- data %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(Total_Y = sum(Y))

  if (ProgressBar == TRUE) {
    num_sim <- length(N) * length(treatment_periods) * nrow(BestMarkets)
    pb <- progress::progress_bar$new(
      format = "  Running Simulations [:bar] :percent",
      total = num_sim,
      clear = FALSE,
      width = 60
    )
  } else {
    pb <- NULL
  }

  for (n in N) {
    BestMarkets_aux <- stochastic_market_selector(
      n,
      BestMarkets,
      run_stochastic_process = run_stochastic_process
    )

    partial_results <- run_simulations(
      data = data,
      treatment_combinations = BestMarkets_aux,
      treatment_durations = treatment_periods,
      effect_sizes = 0,
      side_of_test = "two_sided",
      lookback_window = lookback_window,
      parallel = parallel,
      ProgressBar = ProgressBar,
      cpic = 0,
      X = X,
      normalize = normalize,
      fixed_effects = fixed_effects,
      model = model
    )
    results <- rbind(results, partial_results)
  }

  if (parallel == TRUE) {
    parallel::stopCluster(cl)
  }

  # Sort Locations alphabetically
  results$location <- strsplit(stringr::str_replace_all(results$location, ", ", ","), split = ",")
  results$location <- lapply(results$location, sort)
  results$location <- lapply(results$location, function(x) paste(x, collapse = ", "))
  results$location <- unlist(results$location)

  if (type == "pValue") {
    results$pow <- 0
    results$pow[results$pvalue > alpha] <- 1

    resultsM <- results %>%
      dplyr::group_by(location) %>%
      dplyr::summarize(mean_pow = mean(pow), mean_scaled_l2_imbalance = mean(ScaledL2Imbalance)) %>%
      dplyr::distinct()
  } else if (type == "Imbalance") {
    resultsM <- results %>%
      dplyr::group_by(location) %>%
      dplyr::summarize(mean_scaled_l2_imbalance = mean(ScaledL2Imbalance)) %>%
      dplyr::distinct()
  }

  # Add Percent of Y in test markets
  resultsM$ProportionTotal_Y <- 1
  resultsM$Locs <- strsplit(stringr::str_replace_all(resultsM$location, ", ", ","), split = ",")

  for (row in 1:nrow(resultsM)) {
    resultsM$ProportionTotal_Y[row] <- as.numeric(AggYperLoc %>%
      dplyr::filter(location %in% resultsM$Locs[[row]]) %>%
      dplyr::summarize(total = sum(Total_Y))) /
      sum(AggYperLoc$Total_Y)
  }

  # Sort Before Ranking
  if (type == "pValue") {
    resultsM <- resultsM %>%
      dplyr::arrange(
        dplyr::desc(mean_pow),
        mean_scaled_l2_imbalance,
        dplyr::desc(ProportionTotal_Y)
      )
  } else if (type == "Imbalance") {
    resultsM <- resultsM %>%
      dplyr::arrange(
        mean_scaled_l2_imbalance,
        dplyr::desc(ProportionTotal_Y)
      )
  }

  # Remove the Locs column
  resultsM <- dplyr::select(resultsM, -c(Locs))

  class(results) <- c("GeoLift.search", class(resultsM))

  resultsM$rank <- 1:nrow(resultsM)

  if (top_results > nrow(resultsM)) {
    top_results <- nrow(resultsM)
  }

  print(paste0("Best ", top_results, " test markets:"))
  print(head(resultsM[, 1], top_results))

  return(as.data.frame(resultsM))
}


#' Power Calculation to determine the number of test periods
#' with unknown test locations.
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Development on `NumberLocations()` is complete.
#' We recommend switching to `GeoLiftMarketSelection()`
#' for new code, which is easier to use, more featureful,
#' and still under active development.#'
#'
#' `NumberLocations` calculates power to determine the
#' number of test periods with unknown test locations.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param number_locations List of number of locations to test. If not specified,
#' the number of locations will be computed by percentiles up to half of the
#' total number of locations.
#' @param treatment_periods Number of treatment periods.
#' @param n_sim Number of simulations.
#' @param X List of covariate names.
#' @param Y_id Name of the outcome variable (String).
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
#' @param plot Plots results when TRUE.
#' @param power Power level. By default 0.8.
#' @param alpha Significance Level. By default 0.1.
#' @param type Method of inference used in the analysis.
#'             pValue=Provides conformal inference to provide the aggregate
#'             p-value for the null hypothesis of no effect from the intervention.
#'             The Default type is pValue.
#'             Imbalance=Uses the model's Scaled L2 Imbalance metric.
#' @param normalize A logic flag indicating whether to scale the outcome which is
#' useful to accelerate computing speed when the magnitude of the data is large. The
#' default is FALSE.
#' @param model A string indicating the outcome model used to augment the Augmented
#' Synthetic Control Method. Augmentation through a prognostic function can improve
#' fit and reduce L2 imbalance metrics.
#' \itemize{
#'          \item{"None":}{ ASCM is not augmented by a prognostic function. Defualt.}
#'          \item{"Ridge":}{ Augments with a Ridge regression. Recommended to improve fit
#'                           for smaller panels (less than 40 locations and 100 time-stamps.))}
#'          \item{"GSYN":}{ Augments with a Generalized Synthetic Control Method. Recommended
#'                          to improve fit for larger panels (more than 40 locations and 100
#'                          time-stamps. }
#'          }
#' @param fixed_effects A logic flag indicating whether to include unit fixed
#' effects in the model. Set to TRUE by default.
#' @param stat_func Function to compute test statistic. NULL by default.
#' @param ProgressBar A logic flag indicating whether to display a progress bar
#' to track progress. Set to FALSE by default.
#' @param parallel A logic flag indicating whether to use parallel computing to
#' speed up calculations. Set to TRUE by default.
#' @param parallel_setup A string indicating parallel workers set-up.
#' Set to "sequential" by default.
#' @param import_augsynth_from Points to where the augsynth package
#' should be imported from to send to the nodes.
#' @param import_tidyr_from Points to where the tidyr package
#' should be imported from to send to the nodes.
#'
#' @return
#' Table of average power by number of locations.
#'
#' @export
NumberLocations <- function(data,
                            number_locations = c(),
                            treatment_periods,
                            n_sim = 50,
                            X = c(),
                            Y_id = "Y",
                            location_id = "location",
                            time_id = "time",
                            plot = TRUE,
                            power = 0.8,
                            alpha = 0.1,
                            type = "pValue",
                            normalize = FALSE,
                            model = "none",
                            fixed_effects = TRUE,
                            stat_func = NULL,
                            ProgressBar = FALSE,
                            parallel = TRUE,
                            parallel_setup = "sequential",
                            import_augsynth_from = "library(augsynth)",
                            import_tidyr_from = "library(tidyr)") {
  if (parallel == TRUE) {
    cl <- build_cluster(
      parallel_setup = parallel_setup,
      import_augsynth_from = import_augsynth_from,
      import_tidyr_from = import_tidyr_from
    )
  }

  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)
  locs <- unique(as.character(data$location))

  # Small Pre-treatment Periods
  if (max_time / max(treatment_periods) < 4) {
    message(paste0("Caution: Small pre-treatment period!.
                   \nIt's recommended to have at least 4x pre-treatment periods for each treatment period.\n"))
  }

  results <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(results) <- c("location", "pvalue", "n", "treatment_start", "ScaledL2Imbalance")

  if (length(number_locations) == 0) {
    number_locations <- unique(round(quantile(c(1:length(unique(data$location))),
      probs = seq(0, 0.5, 0.05),
      type = 1,
      names = FALSE
    )))
  }

  num_sim <- length(number_locations)

  if (ProgressBar == TRUE) {
    pb <- progress::progress_bar$new(
      format = "  Running Simulations [:bar] :percent",
      total = num_sim,
      clear = FALSE,
      width = 60
    )
  }

  for (n in number_locations) {
    if (ProgressBar == TRUE) {
      pb$tick()
    }

    if (parallel == TRUE) {
      `%dopar%` <- foreach::`%dopar%`
      a <- foreach(
        sim = 1:n_sim,
        .combine = cbind,
        .errorhandling = "stop"
      ) %dopar% {
        pvalueCalc(
          data = data,
          sim = 1,
          max_time = max_time,
          tp = treatment_periods,
          es = 0,
          locations = as.list(sample(locs, n, replace = FALSE)),
          cpic = 0,
          X = c(),
          type = type,
          normalize = normalize,
          fixed_effects = fixed_effects,
          model = model,
          stat_func = stat_func
        )
      }

      for (i in 1:ncol(a)) {
        results <- rbind(results, data.frame(
          location = a[[1, i]],
          pvalue = as.numeric(a[[2, i]]),
          n = n,
          treatment_start = as.numeric(a[[5, i]]),
          ScaledL2Imbalance = as.numeric(a[[7, i]])
        ))
      }
    } else {
      for (sim in 1:n_sim) {
        aux <- NULL
        aux <- suppressMessages(pvalueCalc(
          data = data,
          sim = 1,
          max_time = max_time,
          tp = treatment_periods,
          es = 0,
          locations = as.list(sample(locs, n, replace = FALSE)),
          cpic = 0,
          X = c(),
          type = type,
          normalize = normalize,
          fixed_effects = fixed_effects,
          model = model,
          stat_func = stat_func
        ))

        results <- rbind(results, data.frame(
          location = aux[1],
          pvalue = as.numeric(aux[2]),
          n = n,
          treatment_start = as.numeric(aux[5]),
          ScaledL2Imbalance = as.numeric(aux[7])
        ))
      }
    }
  }

  if (parallel == TRUE) {
    parallel::stopCluster(cl)
  }

  if (plot == TRUE) {
    results$pow <- 0
    results$pow[results$pvalue > alpha] <- 1

    resultsM <- results %>%
      dplyr::group_by(n) %>%
      dplyr::summarize(mean_pow = mean(pow), mean_L2ScaledImbalance = mean(ScaledL2Imbalance))
    resultsM <- dplyr::add_row(resultsM, n = 0, mean_pow = 0, mean_L2ScaledImbalance = 1, .before = 1)

    if (type == "pValue") {
      print("Average Power By Number of Locations")
      print(resultsM)

      powerplot <- ggplot(resultsM, aes(y = mean_pow, x = n)) +
        geom_line(color = "#52854C", size = 0.95) +
        ylim(0, 1) +
        ggtitle("Average Power By Number of Locations") +
        xlab("Number of Locations") +
        ylab("Average Power") +
        geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey") +
        theme_minimal()

      imbalanceplot <- ggplot(resultsM, aes(y = mean_L2ScaledImbalance, x = n)) +
        geom_line(color = "#7030A0", size = 0.95) +
        ylim(0, 1) +
        ggtitle("Average Scaled L2 Imbalance By Number of Locations") +
        xlab("Number of Locations") +
        ylab("Average Scaled L2 Imbalance") +
        theme_minimal()

      gridExtra::grid.arrange(powerplot, imbalanceplot, nrow = 1)
    } else if (type == "Imbalance") {
      print("Average Scaled L2 Imbalance By Number of Locations")
      print(resultsM[, -2])

      imbalanceplot <- ggplot(resultsM, aes(y = mean_L2ScaledImbalance, x = n)) +
        geom_line(color = "#7030A0", size = 0.95) +
        ylim(0, 1) +
        ggtitle("Average Scaled L2 Imbalance By Number of Locations") +
        xlab("Number of Locations") +
        ylab("Average Scaled L2 Imbalance") +
        theme_minimal()
      plot(imbalanceplot)
    }
  }

  return(results)
}


#' Power Calculation for GeoLift for known test locations.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function runs power calculations for input historical geo data
#' for a pre-determined set of test locations.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param locations A semi-colon separated list of test geo locations.
#' @param effect_size A vector of effect sizes to test by default a
#' sequence between 0 - 100 percent in 5 percent increments: seq(0,1,0.05).
#' @param treatment_periods Expected length of the test. A vector of
#' possible lengths can be entered for multiple options.
#' @param lookback_window A number indicating how far back in time the simulations
#' for the power analysis should go. For instance, a value equal to 5 will simulate
#' power for the last five possible tests. By default lookback_window = 1 which
#' will only execute the most recent test based on the data.
#' @param cpic Cost Per Incremental Conversion for estimated test
#' minimum budget. The default value is 0, in which case no investment
#' estimation will be provided.
#' @param X List of names of covariates. No covariates are used
#' by default.
#' @param Y_id Name of the outcome variable. "Y" by default.
#' @param location_id Name of the location variable. "Y" by default.
#' @param time_id Name of the time variable. "Y" by default.
#' @param alpha Significance level. Set to 0.1 by default.
#' @param type Method of inference used in the analysis.
#'             pValue=Provides conformal inference to provide the aggregate
#'             p-value for the null hypothesis of no effect from the intervention.
#'             The Default type is pValue.
#'             Imbalance=Uses the model's Scaled L2 Imbalance metric.
#' @param normalize A logic flag indicating whether to scale the outcome which is
#' useful to accelerate computing speed when the magnitude of the data is large. The
#' default is FALSE.
#' @param model A string indicating the outcome model used to augment the Augmented
#' Synthetic Control Method. Augmentation through a prognostic function can improve
#' fit and reduce L2 imbalance metrics.
#' \itemize{
#'          \item{"None":}{ ASCM is not augmented by a prognostic function. Defualt.}
#'          \item{"Ridge":}{ Augments with a Ridge regression. Recommended to improve fit
#'                           for smaller panels (less than 40 locations and 100 time-stamps.))}
#'          \item{"GSYN":}{ Augments with a Generalized Synthetic Control Method. Recommended
#'                          to improve fit for larger panels (more than 40 locations and 100
#'                          time-stamps. }
#'          }
#' @param fixed_effects A logic flag indicating whether to include unit fixed
#' effects in the model. Set to TRUE by default.
#' @param ProgressBar A logic flag indicating whether to display a progress bar
#' to track progress. Set to FALSE by default.
#' @param parallel A logic flag indicating whether to use parallel computing to
#' speed up calculations. Set to TRUE by default.
#' @param parallel_setup A string indicating parallel workers set-up.
#' Set to "sequential" by default.
#' @param side_of_test A string indicating whether confidence will be determined
#' using a one sided or a two sided test.
#' \itemize{
#'          \item{"two_sided":}{ The test statistic is the sum of all treatment effects, i.e. sum(abs(x)). Defualt.}
#'          \item{"one_sided":}{ One-sided test against positive or negaative effects i.e.
#'          If the effect being applied is negative, then defaults to -sum(x). H0: ES >= 0; HA: ES < 0.
#'          If the effect being applied is positive, then defaults to sum(x). H0: ES <= 0; HA: ES > 0.}
#'          }
#' @param conformal_type Type of conformal inference used. Can be either "iid" for Independent and identically
#' distributed or "block" for moving block permutations. Set to "iid" by default.
#' @param ns Number of resamples for "iid" permutations if `conformal_type = "iid`. Set to 1000 by default.
#' @param import_augsynth_from Points to where the augsynth package
#' should be imported from to send to the nodes.
#' @param import_tidyr_from Points to where the tidyr package
#' should be imported from to send to the nodes.
#'
#' @return
#' GeoLiftPower object that contains:
#'      \itemize{
#'          \item{"location":}{ Test units of the simulation}
#'          \item{"pvalue":}{ P Value for each simulation}
#'          \item{"duration":}{ Duration of the simulation}
#'          \item{"effect_size":}{ Effect Size used for the simulation}
#'          \item{"treatment_start":}{ Treatment start time for the simulation}
#'          \item{"investment":}{ Estimated Investment}
#'          \item{"cpipc":}{ Cost Per Incremental Conversion}
#'          \item{"ScaledL2Imbalance":}{ Scaled L2 Imbalance metric}
#'      }
#'
#' @order 1
#' @export
GeoLiftPower <- function(data,
                         locations,
                         effect_size = seq(0, 1, 0.05),
                         treatment_periods,
                         lookback_window = 1,
                         cpic = 0,
                         X = c(),
                         Y_id = "Y",
                         location_id = "location",
                         time_id = "time",
                         alpha = 0.1,
                         type = "pValue",
                         normalize = FALSE,
                         model = "none",
                         fixed_effects = TRUE,
                         ProgressBar = FALSE,
                         parallel = TRUE,
                         parallel_setup = "sequential",
                         side_of_test = "two_sided",
                         conformal_type = "iid",
                         ns = 1000,
                         import_augsynth_from = "library(augsynth)",
                         import_tidyr_from = "library(tidyr)") {
  if (parallel == TRUE) {
    cl <- build_cluster(
      parallel_setup = parallel_setup,
      import_augsynth_from = import_augsynth_from,
      import_tidyr_from = import_tidyr_from
    )
  }

  message(
    "Calculating Power for the following treatment group: ",
    paste0(locations, collapse = "; "), "."
  )
  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)

  # Small Pre-treatment Periods
  if (max_time / max(treatment_periods) < 4) {
    message(paste0("Caution: Small pre-treatment period!.
                   \nIt's recommended to have at least 4x pre-treatment periods for each treatment period.\n"))
  }

  if (lookback_window <= 0) {
    lookback_window <- 1
  }

  results <- run_simulations(
    data = data,
    treatment_combinations = matrix(locations, nrow = 1),
    treatment_durations = treatment_periods,
    effect_sizes = effect_size,
    side_of_test = side_of_test,
    conformal_type = conformal_type,
    ns = ns,
    lookback_window = lookback_window,
    parallel = parallel,
    ProgressBar = ProgressBar,
    cpic = cpic,
    X = X,
    normalize = normalize,
    fixed_effects = fixed_effects,
    model = model
  )

  if (parallel == TRUE) {
    parallel::stopCluster(cl)
  }

  class(results) <- c("GeoLiftPower", class(results))

  results$power <- 0
  results$power[results$pvalue < alpha] <- 1

  return(results)
}


#' GeoLift Market Selection algorithm based on a Power Analysis.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `GeoLiftMarketSelection` provides a ranking of test markets  for a
#' GeoLift test based on a power analysis.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param treatment_periods List of treatment periods to calculate power for.
#' @param N List of number of test markets to calculate power for. If left empty (default)
#' and if no locations are included through `include_locations`, it will populate
#' the list of markets with the deciles of the total number of locations. If left empty
#' and a set of markets is provided by `include_locations` only the deciles larger
#' or equal than `length(include_locations)` will be used.
#' @param X List of names of covariates.
#' @param Y_id Name of the outcome variable (String).
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
#' @param effect_size A vector of effect sizes to test by default a
#' sequence between 0 - 25 percent in 5 percent increments: seq(0,0.25,0.05).
#' Make sure that the sequence includes zero.
#' @param lookback_window A number indicating how far back in time the simulations
#' for the power analysis should go. For instance, a value equal to 5 will simulate
#' power for the last five possible tests. By default lookback_window = 1 which
#' will only execute the most recent test based on the data.
#' @param include_markets A list of markets or locations that should be part of the
#' test group. Make sure to specify an N as large or larger than the number of
#' provided markets or locations. Empty list by default.
#' @param exclude_markets A list of markets or locations that won't be considered
#' for the test market selection, but will remain in the pool of controls. Empty
#' list by default.
#' @param holdout A vector with two values: the first one the smallest desirable
#' holdout and the second the largest desirable holdout. If left empty (default)
#' all market selections will be provided regardless of their size.
#' @param cpic Number indicating the Cost Per Incremental Conversion.
#' @param budget Number indicating the maximum budget available for a GeoLift test.
#' @param alpha Significance Level. By default 0.1.
#' @param normalize A logic flag indicating whether to scale the outcome which is
#' useful to accelerate computing speed when the magnitude of the data is large. The
#' default is FALSE.
#' @param model A string indicating the outcome model used to augment the Augmented
#' Synthetic Control Method. Augmentation through a prognostic function can improve
#' fit and reduce L2 imbalance metrics.
#' \itemize{
#'          \item{"None":}{ ASCM is not augmented by a prognostic function. Defualt.}
#'          \item{"Ridge":}{ Augments with a Ridge regression. Recommended to improve fit
#'                           for smaller panels (less than 40 locations and 100 time-stamps.))}
#'          \item{"GSYN":}{ Augments with a Generalized Synthetic Control Method. Recommended
#'                          to improve fit for larger panels (more than 40 locations and 100
#'                          time-stamps. }
#'          }
#' @param fixed_effects A logic flag indicating whether to include unit fixed
#' effects in the model. Set to TRUE by default.
#' @param dtw Emphasis on Dynamic Time Warping (DTW), dtw = 1 focuses exclusively
#' on this metric while dtw = 0 (default) relies on correlations only.
#' @param Correlations A logic flag indicating whether an additional column with
#' the correlations between the test regions and total control markets will be
#' included in the final output. Set to FALSE by default.
#' @param ProgressBar A logic flag indicating whether to display a progress bar
#' to track progress. Set to FALSE by default.
#' @param print A logic flag indicating whether to print the top results. Set to
#' TRUE by default.
#' @param run_stochastic_process A logic flag indicating whether to select test
#' markets through random sampling of the the similarity matrix. Given that
#' interpolation biases may be relevant if the synthetic control matches
#' the characteristics of the test unit by averaging away large discrepancies
#' between the characteristics of the test and the units in the synthetic controls,
#' it is recommended to only use random sampling after making sure all units are
#' similar. This parameter is set by default to FALSE.
#' @param parallel A logic flag indicating whether to use parallel computing to
#' speed up calculations. Set to TRUE by default.
#' @param parallel_setup A string indicating parallel workers set-up.
#' Set to "sequential" by default.
#' @param side_of_test A string indicating whether confidence will be determined
#' using a one sided or a two sided test.
#' \itemize{
#'          \item{"two_sided":}{ The test statistic is the sum of all treatment effects, i.e. sum(abs(x)). Defualt.}
#'          \item{"one_sided":}{ One-sided test against positive or negaative effects i.e.
#'          If the effect being applied is negative, then defaults to -sum(x). H0: ES >= 0; HA: ES < 0.
#'          If the effect being applied is positive, then defaults to sum(x). H0: ES <= 0; HA: ES > 0.}
#'          }
#' @param conformal_type Type of conformal inference used. Can be either "iid" for Independent and identically
#' distributed or "block" for moving block permutations. Set to "iid" by default.
#' @param ns Number of resamples for "iid" permutations if `conformal_type = "iid`. Set to 1000 by default.
#' @param import_augsynth_from Points to where the augsynth package
#' should be imported from to send to the nodes.
#' @param import_tidyr_from Points to where the tidyr package
#' should be imported from to send to the nodes.
#'
#' @return
#' A list with three Data Frames. \itemize{
#'          \item{"BestMarkets":}{Data Frame with a ranking of the best markets
#'          based on power, Scaled L2 Imbalance, Minimum Detectable Effect, and
#'          proportion of total KPI in the test markets.}
#'          \item{"PowerCurves":}{Data Frame with the resulting power curves for
#'          each recommended market.}
#'          \item{"parameters;"}{List of parameters to plot the results.
#'          Includes the data set, model, fixed-effects, and CPIC parameters.}
#' }
#'
#' @order 1
#' @export
GeoLiftMarketSelection <- function(data,
                                   treatment_periods,
                                   N = c(),
                                   X = c(),
                                   Y_id = "Y",
                                   location_id = "location",
                                   time_id = "time",
                                   effect_size = seq(-0.2, 0.2, 0.05),
                                   lookback_window = 1,
                                   include_markets = c(),
                                   exclude_markets = c(),
                                   holdout = c(),
                                   cpic = 1,
                                   budget = NULL,
                                   alpha = 0.1,
                                   normalize = FALSE,
                                   model = "none",
                                   fixed_effects = TRUE,
                                   dtw = 0,
                                   Correlations = FALSE,
                                   ProgressBar = FALSE,
                                   print = TRUE,
                                   run_stochastic_process = FALSE,
                                   parallel = TRUE,
                                   parallel_setup = "sequential",
                                   side_of_test = "two_sided",
                                   conformal_type = "iid",
                                   ns = 1000,
                                   import_augsynth_from = "library(augsynth)",
                                   import_tidyr_from = "library(tidyr)") {
  if (parallel == TRUE) {
    cl <- build_cluster(
      parallel_setup = parallel_setup,
      import_augsynth_from = import_augsynth_from,
      import_tidyr_from = import_tidyr_from
    )
  }

  message("Calculating which the best treatment groups are.")
  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)
  include_markets <- tolower(include_markets)
  exclude_markets <- tolower(exclude_markets)

  # Data Checks

  if (any(include_markets %in% exclude_markets)) {
    stop("\ninclude_markets and exclude_markets overlap. Please define where these locations should go.")
  }

  # Small Pre-treatment Periods
  if (max_time / max(treatment_periods) < 4) {
    message(paste0("Caution: Small pre-treatment period!.
                   \nIt's recommended to have at least 4x pre-treatment periods for each treatment period.\n"))
  }

  # Populate N if it's not provided
  if (length(N) == 0) {
    N <- unique(round(quantile(c(1:length(unique(data$location))),
      probs = seq(0, 0.5, 0.1),
      type = 1,
      names = FALSE
    )))

    if (length(include_markets) > 0) {
      # Keep only those equal or larger than included markets
      N <- N[length(include_markets) <= N]
      if (length(N) == 0) {
        stop("All N are smaller than amount of included markets. Please increase N.")
      }
    }
  }

  # More include_markets than N
  if (length(include_markets) > 0 & min(N) < length(include_markets)) {
    message(paste0(
      "Error: More forced markets than total test ones.",
      " Consider increasing the values of N."
    ))
    return(NULL)
  }

  # Check that the provided markets exist in the data.
  if (!all(tolower(include_markets) %in% tolower(unique(data$location)))) {
    message(paste0(
      "Error: One or more markets in include_markets were not",
      " found in the data. Check the provided list and try again."
    ))
    return(NULL)
  }

  # Check that the provided markets exist in the data.
  if (!all(tolower(exclude_markets) %in% tolower(unique(data$location)))) {
    message(paste0(
      "Error: One or more markets in exclude_markets were not",
      " found in the data. Check the provided list and try again."
    ))
    return(NULL)
  }

  #  Check the holdout parameter
  if (length(holdout) > 1) {
    if (length(holdout) > 2) {
      message("Error: Too many arguments in holdout. Provide the min and max holdout sizes.")
      return(NULL)
    } else if (holdout[1] >= holdout[2]) {
      message("Error: The first argument in holdout should be strictly smaller than the second.")
      return(NULL)
    } else if (min(holdout) < 0 | max(holdout > 1)) {
      message("Error: Please specify valid values for holdouts (values from 0 to 1)")
      return(NULL)
    }
  } else if (length(holdout) == 1) {
    message("Error: Too few arguments in holdout. Provide the min and max holdout sizes.")
    return(NULL)
  }

  results <- data.frame(matrix(ncol = 10, nrow = 0))

  # Setting the lookback window to the smallest length of treatment if not provided.
  if (lookback_window <= 0) {
    lookback_window <- 1
  }

  BestMarkets <- MarketSelection(
    data,
    location_id = "location",
    time_id = "time",
    Y_id = "Y",
    dtw = dtw,
    exclude_markets = exclude_markets
  )

  N <- limit_test_markets(BestMarkets, N, run_stochastic_process)

  # Aggregated Y Per Location
  AggYperLoc <- data %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(Total_Y = sum(Y))

  for (n in N) {
    BestMarkets_aux <- stochastic_market_selector(
      n,
      BestMarkets,
      run_stochastic_process = run_stochastic_process
    )

    # Force included markets into the selection
    if (length(include_markets) > 0) {
      temp_Markets <- NULL
      for (row in 1:nrow(BestMarkets_aux)) {
        if (all(include_markets %in% BestMarkets_aux[row, ])) {
          temp_Markets <- rbind(temp_Markets, BestMarkets_aux[row, ])
        }
      }
      BestMarkets_aux <- temp_Markets
    }

    if (is.null(BestMarkets_aux) && length(N) == 1) {
      stop("\nNo markets meet the criteria you provided. Consider modifying market exclusion/inclusion hyperparameters")
    }

    # Skip iteration if no Markets are feasible
    if (is.null(BestMarkets_aux)) {
      next
    }

    if (nrow(BestMarkets_aux) == 0) {
      stop("\nNo markets meet the criteria you provided. Consider modifying market exclusion/inclusion hyperparameters")
    }

    partial_results <- run_simulations(
      data = data,
      treatment_combinations = BestMarkets_aux,
      treatment_durations = treatment_periods,
      effect_sizes = effect_size,
      side_of_test = side_of_test,
      conformal_type = conformal_type,
      ns = ns,
      lookback_window = lookback_window,
      parallel = parallel,
      ProgressBar = ProgressBar,
      cpic = cpic,
      X = X,
      normalize = normalize,
      fixed_effects = fixed_effects,
      model = model
    )
    results <- rbind(results, partial_results)
  }

  if (parallel == TRUE) {
    parallel::stopCluster(cl)
  }

  # Step 1 - Sort Locations Alphabetically
  results$location <- strsplit(stringr::str_replace_all(results$location, ", ", ","), split = ",")
  results$location <- lapply(results$location, sort)
  results$location <- lapply(results$location, function(x) paste(x, collapse = ", "))
  results$location <- unlist(results$location)

  # Step 2 - Compute Significant & Remove Duplicates
  results <- results %>%
    dplyr::mutate(significant = ifelse(pvalue < alpha, 1, 0)) %>%
    dplyr::distinct()

  # Step 3 - Compute average Metrics
  results <- results %>%
    dplyr::group_by(location, duration, EffectSize) %>%
    dplyr::summarise(
      power = mean(significant),
      AvgScaledL2Imbalance = mean(ScaledL2Imbalance),
      Investment = mean(Investment),
      AvgATT = mean(att_estimator),
      AvgDetectedLift = mean(detected_lift), .groups = "keep"
    )

  # Step 4 - Find the MDE that achieved power
  resultsM <- NULL

  for (locs in unique(results$location)) {
    for (ts in treatment_periods) {
      resultsFindAux <- results %>% dplyr::filter(location == locs & duration == ts & power > 0.8)

      if (nrow(resultsFindAux) != 0) {
        negative_mde <- max(
          ifelse(resultsFindAux$EffectSize < 0,
            resultsFindAux$EffectSize,
            min(effect_size) - 1
          )
        )
        positive_mde <- min(
          ifelse(resultsFindAux$EffectSize > 0,
            resultsFindAux$EffectSize,
            max(effect_size) + 1
          )
        )
        MDEAux <- ifelse(
          positive_mde > abs(negative_mde) & negative_mde != 0,
          negative_mde,
          positive_mde
        )

        resultsFindAux <- resultsFindAux %>% dplyr::filter(EffectSize == MDEAux)

        if (MDEAux != 0) { # Drop tests significant with ES = 0
          resultsM <- resultsM %>% dplyr::bind_rows(resultsFindAux)
        }
      }
    }
  }
  if (is.null(resultsM)) {
    stop("\nNo markets meet the criteria you provided. Consider modifying the input hyperparameters.")
  }

  # Step 5 - Add Percent of Y in test markets
  # Step 5.1 - Create the overall prop
  AggYperLoc <- data %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(Total_Y = sum(Y))

  # Step 5.2 - Attach to Table
  resultsM$ProportionTotal_Y <- 1
  resultsM$Locs <- strsplit(stringr::str_replace_all(resultsM$location, ", ", ","), split = ",")

  for (row in 1:nrow(resultsM)) {
    resultsM$ProportionTotal_Y[row] <- as.numeric(AggYperLoc %>%
      dplyr::filter(location %in% resultsM$Locs[[row]]) %>%
      dplyr::summarize(total = sum(Total_Y))) /
      sum(AggYperLoc$Total_Y)
  }

  # Step 6 - Remove any duplicates
  resultsM <- resultsM %>%
    dplyr::group_by(location, duration) %>%
    dplyr::slice_max(order_by = power, n = 1)

  # Step 7 - Create Rank variable - Adding New Ranking System
  resultsM$abs_lift_in_zero <- round(abs(resultsM$AvgDetectedLift - resultsM$EffectSize), 3)

  resultsM <- as.data.frame(resultsM) %>%
    dplyr::mutate(
      rank_mde = dplyr::dense_rank(abs(EffectSize)),
      rank_pvalue = dplyr::dense_rank(power),
      rank_abszero = dplyr::dense_rank(abs_lift_in_zero)
    )

  resultsM$rank <- rank(
    rowMeans(resultsM[, c("rank_mde", "rank_pvalue", "rank_abszero")]),
    ties.method = "min"
  )

  # Step 8 - Remove unused columns and sort by rank
  resultsM <- resultsM %>%
    dplyr::mutate(
      rank_mde = NULL,
      rank_pvalue = NULL,
      rank_abszero = NULL,
      Locs = NULL
    ) %>%
    dplyr::arrange(rank, location)

  # Step 9 - Rename columns
  resultsM <- dplyr::rename(resultsM,
    Average_MDE = AvgDetectedLift,
    Power = power
  )

  # Step 10: Adjust signs if Negative Lift
  resultsM$Investment <- ifelse(
    resultsM$EffectSize < 0,
    -1 * resultsM$Investment,
    resultsM$Investment
  )
  results$Investment <- ifelse(
    results$EffectSize < 0,
    -1 * results$Investment,
    results$Investment
  )

  # Step 11 - Remove tests out of budget (if applicable)
  if (!is.null(budget)) {
    resultsM <- resultsM %>% dplyr::filter(abs(budget) > abs(Investment))
    # Re-rank
    resultsM$rank <- rank(resultsM$rank, ties.method = "min")
  }

  # Step 12: Holdout Size
  resultsM$Holdout <- ifelse(
    resultsM$EffectSize < 0,
    resultsM$ProportionTotal_Y,
    1 - resultsM$ProportionTotal_Y
  )

  # Step 13: Test Size
  if (length(holdout) > 0) {
    resultsM <- resultsM %>% dplyr::filter(
      holdout[1] <= Holdout,
      holdout[2] >= Holdout
    )
    # Re-rank
    resultsM$rank <- rank(resultsM$rank, ties.method = "min")
  }

  # Step 14 - Create ID
  # Make sure there are viable options
  if (nrow(resultsM) > 0) {
    resultsM$ID <- 1:nrow(resultsM)
  } else {
    message("\nWarning: No markets meet the criteria you provided. Consider modifying
          the input parameters")
  }

  # Re-order columns
  resultsM <- resultsM[, c(13, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 11)]

  # Add correlations to total
  if (Correlations) {
    resultsM$correlation <- 0
    for (row in 1:nrow(resultsM)) {
      resultsM$correlation[row] <- CorrelationCoefficient(
        data,
        locs = unlist(
          strsplit(
            stringr::str_replace_all(resultsM$location[row], ", ", ","),
            split = ","
          )
        )
      )
    }
  }

  # Print top Results
  if (print) {
    print(head(resultsM))
  }

  # Save Parameters for plotting
  parameters <- list(
    data = data,
    model = model,
    fixed_effects = fixed_effects,
    cpic = cpic,
    side_of_test = side_of_test
  )

  output <- list(
    BestMarkets = as.data.frame(resultsM),
    PowerCurves = as.data.frame(results),
    parameters = parameters
  )

  class(output) <- c("GeoLiftMarketSelection", class(output))
  return(output)
}


#' @param x \code{GeoLiftMarketSelection()}
#' @param ... Optional arguments.
#'
#' @rdname GeoLiftMarketSelection
#' @order 2
#' @export
print.GeoLiftMarketSelection <- function(x, ...) {
  if (!inherits(x, "GeoLiftMarketSelection")) {
    stop("object must be class GeoLiftMarketSelection")
  }

  print(x$BestMarkets)
}
