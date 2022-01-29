# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function GeoLiftMarketSelection, GeoLiftPowerFinder, 
# GeoLiftPower.search, NumberLocations, GeoLiftPower.


#' GeoLift Market Selection algorithm based on a Power Analysis.
#'
#' @description
#'
#' \code{GeoLiftMarketSelection} provides a ranking of test markets  for a
#' GeoLift test based on a power analysis.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param treatment_periods List of treatment periods to calculate power for.
#' @param N List of number of test markets to calculate power for. If left empty (default)
#' and if no locations are included through \code{include_locations}, it will populate
#' the list of markets with the deciles of the total number of locations. If left empty
#' and a set of markets is provided by \code{include_locations} only the deciles larger
#' or equal than \code{length(include_locations)} will be used.
#' @param X List of names of covariates.
#' @param Y_id Name of the outcome variable (String).
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
#' @param effect_size A vector of effect sizes to test by default a
#' sequence between 0 - 25 percent in 5 percent increments: seq(0,0.25,0.05).
#' Only input sequences that are entirely positive or negative and that include
#' zero.
#' @param lookback_window A number indicating how far in time the simulations
#' for the power analysis should go. For instance, a value equal to 5 will simulate
#' power for the last five possible tests. By default lookback_window = -1 which
#' will set the window to the smallest provided test \code{min(treatment_periods)}.
#' @param include_markets A list of markets or locations that should be part of the
#' test group. Make sure to specify an N as large or larger than the number of
#' provided markets or locations.
#' @param exclude_markets A list of markets or locations that will be removed from the
#' analysis.
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
#'
#' @return
#' A list with two Data Frames. \itemize{
#'          \item{"BestMarkets":}{Data Frame with a ranking of the best markets
#'          based on power, Scaled L2 Imbalance, Minimum Detectable Effect, and
#'          proportion of total KPI in the test markets.}
#'          \item{"PowerCurves":}{Data Frame with the resulting power curves for
#'          each recommended market}
#' }
#'
#' @export
GeoLiftMarketSelection <- function(data,
                                   treatment_periods,
                                   N = c(),
                                   X = c(),
                                   Y_id = "Y",
                                   location_id = "location",
                                   time_id = "time",
                                   effect_size = seq(0, 0.25, 0.05),
                                   lookback_window = -1,
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
                                   ProgressBar = FALSE,
                                   plot_best = FALSE,
                                   run_stochastic_process = FALSE,
                                   parallel = TRUE,
                                   parallel_setup = "sequential",
                                   side_of_test = "two_sided",
                                   import_augsynth_from = "library(augsynth)") {
  if (parallel == TRUE) {
    cl <- build_cluster(
      parallel_setup = parallel_setup, import_augsynth_from = import_augsynth_from
    )
  }

  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)
  include_markets <- tolower(include_markets)
  exclude_markets <- tolower(exclude_markets)

  # Data Checks

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
      N <- append(length(include_markets), N[length(include_markets) <= N])
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

  # Make sure all simulated effect sizes have the same sign.
  if (min(effect_size < 0 & max(effect_size) > 0)) {
    message(paste0(
      "Error: The specified simulated effect sizes are not all of the same ",
      " sign. \nTry again with a vector of all positive or negative effects",
      " sizes that includes zero."
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

  results <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(results) <- c(
    "location",
    "pvalue",
    "duration",
    "lift",
    "treatment_start",
    "investment",
    "cpic",
    "ScaledL2Imbalance"
  )

  # Setting the lookback window to the smallest length of treatment if not provided.
  if (lookback_window < 0) {
    lookback_window <- min(treatment_periods)
  }

  # Exclude markets input by user by filter them out from the uploaded file data
  if (length(exclude_markets) > 0) {
    data <- data[!(data$location %in% exclude_markets), ]
  }

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

  # NEWCHANGE: Progress Bar
  num_sim <- length(N) * length(treatment_periods) * length(effect_size)
  if (ProgressBar == TRUE) {
    pb <- progress::progress_bar$new(
      format = "  Running Simulations [:bar] :percent",
      total = num_sim,
      clear = FALSE,
      width = 60
    )
  }


  for (n in N) {
    BestMarkets_aux <- stochastic_market_selector(
      n,
      BestMarkets,
      run_stochastic_process = run_stochastic_process
    )

    # Force included markets into the selection
    if (length(include_markets) > 0) {
      for (row in 1:nrow(BestMarkets_aux)) {
        if (all(include_markets %in% BestMarkets_aux[row, ])) {
          temp_Markets <- rbind(temp_Markets, BestMarkets_aux[row, ])
        }
      }
      BestMarkets_aux <- temp_Markets
    }

    # Skip iteration if no Markets are feasible
    if (is.null(BestMarkets_aux)) {
      next
    }

    for (es in effect_size) { # iterate through lift %

      stat_func <- type_of_test(
        side_of_test = side_of_test,
        alternative_hypothesis = ifelse(es > 0, "positive", "negative")
      )

      for (tp in treatment_periods) { # lifts

        if (ProgressBar == TRUE) {
          pb$tick()
        }

        t_n <- max(data$time) - tp + 1 # Number of simulations without extrapolation (latest start time possible for #tp)

        for (sim in 1:(lookback_window)) {
          if (parallel == TRUE) {
            a <- foreach(
              test = 1:nrow(as.matrix(BestMarkets_aux)),
              .combine = cbind,
              .errorhandling = "stop",
              .verbose = FALSE
            ) %dopar% {
              suppressMessages(pvalueCalc(
                data = data,
                sim = sim,
                max_time = max_time,
                tp = tp,
                es = es,
                locations = as.list(as.matrix(BestMarkets_aux)[test, ]),
                cpic = cpic,
                X,
                type = "pValue",
                normalize = normalize,
                fixed_effects = fixed_effects,
                model = model,
                stat_func = stat_func
              ))
            }


            if (!is.null(dim(a))) {
              for (i in 1:ncol(a)) {
                results <- rbind(results, data.frame(
                  location = a[[1, i]],
                  pvalue = as.numeric(a[[2, i]]),
                  duration = as.numeric(a[[3, i]]),
                  lift = as.numeric(a[[4, i]]),
                  treatment_start = as.numeric(a[[5, i]]),
                  investment = as.numeric(a[[6, i]]),
                  cpic = cpic,
                  ScaledL2Imbalance = as.numeric(a[[7, i]])
                ))
              }
            } else if (length(a) > 0) {
              results <- rbind(results, data.frame(
                location = a[1],
                pvalue = as.numeric(a[2]),
                duration = as.numeric(a[3]),
                lift = as.numeric(a[4]),
                treatment_start = as.numeric(a[5]),
                investment = as.numeric(a[6]),
                cpic = cpic,
                ScaledL2Imbalance = as.numeric(a[7])
              ))
            }
          } else {
            for (test in 1:nrow(as.matrix(BestMarkets_aux))) {
              aux <- NULL
              aux <- suppressMessages(pvalueCalc(
                data = data,
                sim = sim,
                max_time = max_time,
                tp = tp,
                es = es,
                locations = as.list(as.matrix(BestMarkets_aux)[test, ]),
                cpic = cpic,
                X,
                type = "pValue",
                normalize = normalize,
                fixed_effects = fixed_effects,
                model = model,
                stat_func = stat_func
              ))


              results <- rbind(results, data.frame(
                location = aux[1],
                pvalue = as.numeric(aux[2]),
                duration = as.numeric(aux[3]),
                lift = as.numeric(aux[4]),
                treatment_start = as.numeric(aux[5]),
                investment = as.numeric(aux[6]),
                cpic = cpic,
                ScaledL2Imbalance = as.numeric(aux[7])
              ))
            }
          }
        }
      } # tp
    }
  }

  if (parallel == TRUE) {
    parallel::stopCluster(cl)
  }

  # Step 1 - Sort
  results$location <- strsplit(stringr::str_replace_all(results$location, ", ", ","), split = ",")
  results$location <- lapply(results$location, sort)
  results$location <- lapply(results$location, function(x) paste(x, collapse = ", "))
  results$location <- unlist(results$location)

  # Step 2 - Compute Significant
  results <- results %>%
    dplyr::mutate(pow = ifelse(pvalue < 0.1, 1, 0)) %>%
    #   dplyr::filter(significant > 0) %>%
    dplyr::distinct()

  # Step 3 - Compute average Metrics
  results <- results %>%
    dplyr::group_by(location, duration, lift) %>%
    dplyr::summarise(
      power = mean(pow),
      AvgScaledL2Imbalance = mean(ScaledL2Imbalance),
      Investment = mean(investment), .groups = "keep"
    )

  # Step 4 - Find the MDE that achieved power

  for (locs in unique(results$location)) {
    for (ts in treatment_periods) { # for(ts in treatment_periods)
      resultsFindAux <- results %>% dplyr::filter(location == locs & duration == ts & power > 0.8)

      if (min(effect_size) < 0) { # if ( min(effect_size) < 0){
        resultsFindAux <- resultsFindAux %>% dplyr::filter(lift != 0)
        MDEAux <- suppressWarnings(max(resultsFindAux$lift))
        resultsFindAux <- resultsFindAux %>% dplyr::filter(lift == MDEAux)
      } else {
        MDEAux <- suppressWarnings(min(resultsFindAux$lift))
        resultsFindAux <- resultsFindAux %>% dplyr::filter(lift == MDEAux)
      }

      if (MDEAux != 0) { # Drop tests significant with ES = 0
        resultsM <- resultsM %>% dplyr::bind_rows(resultsFindAux)
      }
    }
  }

  # Step 5 - Add Percent of Y in test markets
  # Step 5.1 - Create the overall prop
  AggYperLoc <- data %>% # data %>%
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

  # Step 7 - Sort Before Ranking
  resultsM <- resultsM %>%
    dplyr::arrange(
      dplyr::desc(power),
      AvgScaledL2Imbalance,
      lift,
      dplyr::desc(ProportionTotal_Y)
    )

  # Step 8 - Remove the Locs column
  resultsM <- dplyr::select(resultsM, -c(Locs))

  # Step 9 - Rename columns
  resultsM <- dplyr::rename(resultsM,
    Average_MDE = lift,
    Power = power
  )

  # Step 10: Adjust signs if Negative Lift
  if (min(effect_size) < 0) {
    resultsM$Investment <- -1 * resultsM$Investment
    results$Investment <- -1 * results$Investment
  }

  # Step 11 - Remove tests out of budget (if aplicable)
  if (!is.null(budget)) {
    resultsM <- resultsM %>% dplyr::filter(abs(budget) > abs(Investment))
  }

  # Step 12: Holdout Size
  if (min(effect_size) < 0) {
    resultsM$Holdout <- resultsM$ProportionTotal_Y
  } else {
    resultsM$Holdout <- 1 - resultsM$ProportionTotal_Y
  }

  # Step 13: Test Size
  if (length(holdout) > 0) {
    resultsM <- resultsM %>% dplyr::filter(
      holdout[1] <= Holdout,
      holdout[2] >= Holdout
    )
  }

  # Step 14 - Create Ranking
  # Make sure there are viable options
  if (nrow(resultsM) > 0) {
    resultsM$rank <- 1:nrow(resultsM)
  } else {
    message("\nWarning: No markets meet the criteria you provided. Consider modifying
            the input parameters")
  }

  class(results) <- c("GeoLift.MarketSelection", class(resultsM))

  return(list(BestMarkets = as.data.frame(resultsM), PowerCurves = as.data.frame(results)))
}


#' Power calculations for unknown test market locations, number of
#' test markets, and test duration.
#'
#' @description
#'
#' \code{GeoLiftPowerFinder} provides power calculations for unknown
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
#' Only input sequences that are entirely positive or negative.
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
                               import_augsynth_from = "library(augsynth)") {
  if (parallel == TRUE) {
    cl <- build_cluster(
      parallel_setup = parallel_setup, import_augsynth_from = import_augsynth_from
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
    "lift",
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

  # NEWCHANGE: Progress Bar
  num_sim <- length(N) * length(treatment_periods) * length(effect_size)
  if (ProgressBar == TRUE) {
    pb <- progress::progress_bar$new(
      format = "  Running Simulations [:bar] :percent",
      total = num_sim,
      clear = FALSE,
      width = 60
    )
  }

  message("Finding the best markets for your experiment.")

  for (n in N) {
    BestMarkets_aux <- stochastic_market_selector(
      n,
      BestMarkets,
      run_stochastic_process = run_stochastic_process
    )

    for (es in effect_size) { # iterate through lift %

      stat_func <- type_of_test(
        side_of_test = side_of_test,
        alternative_hypothesis = ifelse(es > 0, "positive", "negative")
      )

      for (tp in treatment_periods) { # lifts

        if (ProgressBar == TRUE) {
          pb$tick()
        }

        if (parallel == TRUE) {
          simulation_results <- foreach(
            test = 1:nrow(as.matrix(BestMarkets_aux)), # NEWCHANGE: Horizon = earliest start time for simulations
            .combine = cbind,
            .errorhandling = "stop"
          ) %dopar% {
            pvalueCalc(
              data = data,
              sim = 1,
              max_time = max_time,
              tp = tp,
              es = es,
              locations = as.list(as.matrix(BestMarkets_aux)[test, ]),
              cpic = 0,
              X,
              type = "pValue",
              normalize = normalize,
              fixed_effects = fixed_effects,
              model = model,
              stat_func = stat_func
            )
          }

          for (i in 1:ncol(simulation_results)) {
            results <- rbind(results, data.frame(
              location = simulation_results[[1, i]],
              pvalue = as.numeric(simulation_results[[2, i]]),
              duration = as.numeric(simulation_results[[3, i]]),
              true_lift = as.numeric(simulation_results[[4, i]]),
              treatment_start = as.numeric(simulation_results[[5, i]]),
              ScaledL2Imbalance = as.numeric(simulation_results[[7, i]]),
              att_estimator = as.numeric(simulation_results[[8, i]]),
              detected_lift = as.numeric(simulation_results[[9, i]])
            ))
          }
        } else {
          for (test in 1:nrow(as.matrix(BestMarkets_aux))) {
            simulation_results <- suppressMessages(pvalueCalc(
              data = data,
              sim = 1,
              max_time = max_time,
              tp = tp,
              es = es,
              locations = as.list(as.matrix(BestMarkets_aux)[test, ]),
              cpic = 0,
              X,
              type = "pValue",
              normalize = normalize,
              fixed_effects = fixed_effects,
              model = model,
              stat_func = stat_func
            ))

            results <- rbind(results, data.frame(
              location = simulation_results[1],
              pvalue = as.numeric(simulation_results[2]),
              duration = as.numeric(simulation_results[3]),
              true_lift = as.numeric(simulation_results[4]),
              treatment_start = as.numeric(simulation_results[5]),
              ScaledL2Imbalance = as.numeric(simulation_results[7]),
              att_estimator = as.numeric(simulation_results[8]),
              detected_lift = as.numeric(simulation_results[9])
            ))
          }
        }
      }
    }
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
    dplyr::filter(true_lift != 0) %>%
    dplyr::group_by(location, duration) %>%
    dplyr::slice(which.min(abs(true_lift)))

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

  # Sort Before Ranking

  resultsM$abs_lift_in_zero <- round(abs(resultsM$detected_lift - resultsM$true_lift), 3)

  resultsM <- as.data.frame(resultsM) %>%
    dplyr::mutate(
      rank_mde = dplyr::dense_rank(abs(true_lift)),
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

  class(results) <- c("GeoLift.search", class(resultsM))

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
            data_lifted$time >= max_time - tp + 1] * (1 + BestResults$true_lift[i])

        bestmodels[[i]] <- suppressMessages(GeoLift::GeoLift(
          Y_id = "Y",
          time_id = "time",
          location_id = "location",
          data = data_lifted,
          locations = locs_aux,
          treatment_start_time = max_time - tp + 1,
          treatment_end_time = max_time,
          model = model,
          fixed_effects = fixed_effects,
          print = FALSE
        ))
      }
      suppressMessages(gridExtra::grid.arrange(plot(bestmodels[[1]], notes = paste(
        "locations:", BestResults$location[1],
        "\n Treatment Periods:", tp, "\n Minimum Detectable Effect: ",
        BestResults$true_lift[1],
        "\n Proportion Total Y: ", 100 * round(BestResults$ProportionTotal_Y[1], 3), "%"
      )),
      plot(bestmodels[[2]], notes = paste(
        "locations:", BestResults$location[2],
        "\n Treatment Periods:", tp, "\n Minimum Detectable Effect: ",
        BestResults$true_lift[2],
        "\n Proportion Total Y: ", 100 * round(BestResults$ProportionTotal_Y[2], 3), "%"
      )),
      plot(bestmodels[[3]], notes = paste(
        "locations:", BestResults$location[3],
        "\n Treatment Periods:", tp, "\n Minimum Detectable Effect: ",
        BestResults$true_lift[3],
        "\n Proportion Total Y: ", 100 * round(BestResults$ProportionTotal_Y[3], 3), "%"
      )),
      plot(bestmodels[[4]], notes = paste(
        "locations:", BestResults$location[4],
        "\n Treatment Periods:", tp, "\n Minimum Detectable Effect: ",
        BestResults$true_lift[4],
        "\n Proportion Total Y: ", 100 * round(BestResults$ProportionTotal_Y[4], 3), "%"
      )),
      ncol = 2
      ))
    }
  }

  # NEWCHANGE: Rename Lift to MDE
  resultsM <- resultsM %>% dplyr::rename(MinDetectableEffect = true_lift)

  return(resultsM)
}


#' Power calculations for unknown test market locations, number of
#' test markets, and test duration.
#'
#' @description
#'
#' \code{GeoLiftPower.search} provides power calculations for unknown
#' test markets, number of test locations, and test duration.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param treatment_periods List of treatment periods to calculate power for.
#' @param N List of number of test markets to calculate power for.
#' @param horizon An integer that defines at which time-stamp the power simulations
#' will start. This parameter allows the user to define which period is most relevant
#' for the test's current in-market dynamics (a very small horizon will include
#' simulations of time periods with dynamics that might not be relevant anymore).
#' Ideally the horizon should encompass at least of couple of times the test length.
#' For instance, for a power analysis with 180 days of historical data and a 15 day
#' test, we would recommend setting horizon to at least 150. By default horizon is set
#' to -1 which will  execute the smallest possible horizon with the provided data.
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
#'
#' @return
#' Data frame with the ordered list of best locations and their
#' average power.
#'
#' @export
GeoLiftPower.search <- function(data,
                                treatment_periods,
                                N = 1,
                                horizon = -1,
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
                                import_augsynth_from = "library(augsynth)") {
  if (parallel == TRUE) {
    cl <- build_cluster(
      parallel_setup = parallel_setup, import_augsynth_from = import_augsynth_from
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

  if (horizon < 0) { # NEWCHANGE
    horizon <- max(treatment_periods)
  }

  # Aggregated Y Per Location
  AggYperLoc <- data %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(Total_Y = sum(Y))

  # NEWCHANGE: Progress Bar
  num_sim <- length(N) * length(treatment_periods) * nrow(BestMarkets)
  if (ProgressBar == TRUE) {
    pb <- progress::progress_bar$new(
      format = "  Running Simulations [:bar] :percent",
      total = num_sim,
      clear = FALSE,
      width = 60
    )
  }

  for (n in N) {
    BestMarkets_aux <- stochastic_market_selector(
      n,
      BestMarkets,
      run_stochastic_process = run_stochastic_process
    )
    for (test in 1:nrow(as.matrix(BestMarkets_aux))) { # iterate through lift %
      for (tp in treatment_periods) { # lifts

        if (ProgressBar == TRUE) {
          pb$tick()
        }

        t_n <- max(data$time) - tp + 1 # Number of simulations without extrapolation (latest start time possible for #tp)

        if (parallel == TRUE) {
          a <- foreach(
            sim = 1:(t_n - horizon + 1), # NEWCHANGE: Horizon = earliest start time for simulations
            .combine = cbind,
            .errorhandling = "stop"
          ) %dopar% {
            pvalueCalc(
              data = data,
              sim = sim,
              max_time = max_time,
              tp = tp,
              es = 0,
              locations = as.list(as.matrix(BestMarkets_aux)[test, ]),
              cpic = 0,
              X,
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
              duration = as.numeric(a[[3, i]]),
              treatment_start = as.numeric(a[[5, i]]),
              ScaledL2Imbalance = as.numeric(a[[7, i]])
            ))
          }
        } else {
          for (sim in 1:(t_n - horizon + 1)) {
            aux <- NULL
            aux <- suppressMessages(pvalueCalc(
              data = data,
              sim = sim,
              max_time = max_time,
              tp = tp,
              es = 0,
              locations = as.list(as.matrix(BestMarkets_aux)[test, ]),
              cpic = 0,
              X,
              type = type,
              normalize = normalize,
              fixed_effects = fixed_effects,
              model = model,
              stat_func = stat_func
            ))

            results <- rbind(results, data.frame(
              location = aux[1],
              pvalue = as.numeric(aux[2]),
              duration = as.numeric(aux[3]),
              treatment_start = as.numeric(aux[5]),
              ScaledL2Imbalance = as.numeric(aux[7])
            ))
          }
        }
      }
    }
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
      # dplyr::arrange(dplyr::desc(mean_pow)) %>%
      dplyr::distinct()
  } else if (type == "Imbalance") {
    resultsM <- results %>%
      dplyr::group_by(location) %>%
      dplyr::summarize(mean_scaled_l2_imbalance = mean(ScaledL2Imbalance)) %>%
      # dplyr::arrange(mean_scaled_l2_imbalance) %>%
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
#'
#' \code{NumberLocations} calculates power to determine the
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
                            import_augsynth_from = "library(augsynth)") {
  if (parallel == TRUE) {
    cl <- build_cluster(
      parallel_setup = parallel_setup, import_augsynth_from = import_augsynth_from
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

  results <- data.frame(matrix(ncol = 5, nrow = 0)) # NEWCHANGE: Add Imbalance
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
    # for (t in times){

    if (ProgressBar == TRUE) {
      pb$tick()
    }

    if (parallel == TRUE) {
      a <- foreach(
        sim = 1:n_sim,
        .combine = cbind,
        .errorhandling = "stop"
      ) %dopar% {
        pvalueCalc(
          data = data,
          sim = 1,
          max_time = max_time, # max_time,
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
          max_time = max_time, # max_time,
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
    resultsM <- tibble::add_row(resultsM, n = 0, mean_pow = 0, mean_L2ScaledImbalance = 1, .before = 1)

    if (type == "pValue") {
      print("Average Power By Number of Locations")
      print(resultsM)

      powerplot <- ggplot(resultsM, aes(y = mean_pow, x = n)) +
        geom_line(color = "indianred3", size = 0.95) +
        ylim(0, 1) +
        ggtitle("Average Power By Number of Locations") +
        xlab("Number of Locations") +
        ylab("Average Power") +
        geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey") +
        theme_minimal()

      imbalanceplot <- ggplot(resultsM, aes(y = mean_L2ScaledImbalance, x = n)) +
        geom_line(color = "steelblue4", size = 0.95) +
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
        geom_line(color = "steelblue4", size = 0.95) +
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
#' @param horizon An integer that defines at which time-stamp the power simulations
#' will start. This parameter allows the user to define which period is most relevant
#' for the test's current in-market dynamics (a very small horizon will include
#' simulations of time periods with dynamics that might not be relevant anymore).
#' Ideally the horizon should encompass at least of couple of times the test length.
#' For instance, for a power analysis with 180 days of historical data and a 15 day
#' test, we would recommend setting horizon to at least 150. By default horizon is set
#' to -1 which will  execute the smallest possible horizon with the provided data.
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
#' @param import_augsynth_from Points to where the augsynth package
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
#' @export
GeoLiftPower <- function(data,
                         locations,
                         effect_size = seq(0, 1, 0.05),
                         treatment_periods,
                         horizon = -1,
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
                         import_augsynth_from = "library(augsynth)") {
  if (parallel == TRUE) {
    cl <- build_cluster(
      parallel_setup = parallel_setup, import_augsynth_from = import_augsynth_from
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

  results <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(results) <- c(
    "location",
    "pvalue",
    "duration",
    "lift",
    "treatment_start",
    "investment",
    "cpic",
    "ScaledL2Imbalance"
  )

  if (horizon < 0) { # NEWCHANGE
    horizon <- max(treatment_periods)
  }

  num_sim <- length(effect_size) * length(treatment_periods)

  if (ProgressBar == TRUE) {
    pb <- progress::progress_bar$new(
      format = "  Running Simulations [:bar] :percent",
      total = num_sim,
      clear = FALSE,
      width = 60
    )
  }

  for (es in effect_size) { # iterate through lift %

    stat_func <- type_of_test(
      side_of_test = side_of_test,
      alternative_hypothesis = ifelse(es > 0, "Positive", "Negative")
    )

    for (tp in treatment_periods) { # lifts
      t_n <- max(data$time) - tp + 1 # Number of simulations without extrapolation

      if (ProgressBar == TRUE) {
        pb$tick()
      }

      if (parallel == TRUE) {
        a <- foreach(
          sim = 1:(t_n - horizon + 1), # NEWCHANGE: Horizon = earliest start time for simulations
          .combine = cbind,
          .errorhandling = "stop"
        ) %dopar% {
          pvalueCalc(
            data = data,
            sim = sim,
            max_time = max_time,
            tp = tp,
            es = es,
            locations = locations,
            cpic = cpic,
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
            duration = as.numeric(a[[3, i]]),
            lift = as.numeric(a[[4, i]]),
            treatment_start = as.numeric(a[[5, i]]),
            investment = as.numeric(a[[6, i]]),
            cpic = cpic,
            ScaledL2Imbalance = as.numeric(a[[7, i]])
          ))
        }
      } else {
        for (sim in 1:(t_n - horizon + 1)) {
          aux <- NULL
          aux <- suppressMessages(pvalueCalc(
            data = data,
            sim = sim,
            max_time = max_time,
            tp = tp,
            es = es,
            locations = locations,
            cpic = cpic,
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
            duration = as.numeric(aux[3]),
            lift = as.numeric(aux[4]),
            treatment_start = as.numeric(aux[5]),
            investment = as.numeric(aux[6]),
            cpic = cpic,
            ScaledL2Imbalance = as.numeric(aux[7])
          ))
        }
      }
    }
  }

  if (parallel == TRUE) {
    parallel::stopCluster(cl)
  }

  class(results) <- c("GeoLiftPower", class(results))

  results$pow <- 0
  results$pow[results$pvalue < alpha] <- 1

  return(results)
}
