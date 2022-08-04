# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes functions MultiCellMarketSelection, print.MultiCellMarketSelection,
# MultiCellPower, print.MultiCellPower, MultiCellWinner, print.MultiCellWinner,
# GeoLiftMultiCell, print.GeoLiftMultiCell, summary.GeoLiftMultiCell,


#' Multi-Cell Sampling Method.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `MultiCellMarketSelection` performs a Power-Analysis driven Market Selection
#' for a Multi-Cell GeoLift test using a specified sampling methodology to
#' split the data set into k similar partitions.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param k Number of partitions or cells. k = 2 by default.
#' @param sampling_method Sampling Method used to create the k partitions. Set
#' to "systematic" by default.
#' @param top_choices Number of top Market Selection choices to print for each cell.
#' 10 by default.
#' @param N List of number of test markets to calculate power for. If left empty (default),
#' it will populate the list of markets with the deciles of the total number of locations.
#' @param X List of names of covariates.
#' @param Y_id Name of the outcome variable (String).
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
#' @param effect_size A vector of effect sizes to test by default a
#' sequence between 0 - 25 percent in 5 percent increments: seq(0,0.25,0.05).
#' Make sure that the sequence includes zero.
#' @param treatment_periods List of treatment periods to calculate power for. It
#' is recommended to specify a single treatment length for multi-cell Market Selections.
#' @param lookback_window A number indicating how far back in time the simulations
#' for the power analysis should go. For instance, a value equal to 5 will simulate
#' power for the last five possible tests. By default lookback_window = 1 which
#' will only execute the most recent test based on the data.
#' @param cpic Number indicating the Cost Per Incremental Conversion.
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
#'
#' @return A 'MultiCellMarketSelection' object of four objects:
#' \itemize{
#'          \item{"TopChoices":}{ Data frame with the top choices by cell.}
#'          \item{"Models":}{ The complete list of all Market Selections for each cell. }
#'          \item{"data":}{ The input data.}
#'          \item{"test_details":}{ The test details.}
#'          }
#'
#' @export

MultiCellMarketSelection <- function(data,
                                     k = 2,
                                     sampling_method = "systematic",
                                     top_choices = 10,
                                     N = c(),
                                     X = c(),
                                     Y_id = "Y",
                                     location_id = "location",
                                     time_id = "time",
                                     effect_size = seq(-0.5, 0.5, 0.05),
                                     treatment_periods,
                                     lookback_window = 1,
                                     cpic = 1,
                                     alpha = 0.1,
                                     normalize = FALSE,
                                     model = "none",
                                     fixed_effects = TRUE,
                                     dtw = 0,
                                     Correlations = FALSE,
                                     run_stochastic_process = FALSE,
                                     parallel = TRUE,
                                     parallel_setup = "sequential",
                                     side_of_test = "two_sided"
){

  # Rename variables
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)

  # Check that all treatment_periods are numeric
  if(!(all(sapply(treatment_periods, is.numeric)))){
    stop("\nMake sure all treatment_periods are numeric.")
  }

  # Small Pre-treatment Periods
  if (max_time / max(treatment_periods) < 4) {
    message(paste0("Caution: Small pre-treatment period!.
                   \nIt's recommended to have at least 4x pre-treatment periods for each treatment period.\n"))
  }

  # Input parameter checks

  # Check k
  if(k %% 1 != 0){
    stop("\nMake sure k is an integer.")
  }

  # Check Sampling Method
  if(!(tolower(sampling_method) %in% c("systematic"))){
    stop("\nEnter a valid sampling_method (check the function documentation for more details).")
  }

  # Check side_of_test
  if(!(tolower(side_of_test) %in% c("one_sided", "two_sided"))){
    stop("\nEnter a valid side_of_test ('one_sided', 'two_sided').")
  }

  # Populate N if it's not provided
  if (length(N) == 0) {
    N <- unique(round(quantile(c(1:round(length(unique(data$location))/k)),
                               probs = seq(0, 0.5, 0.1),
                               type = 1,
                               names = FALSE
    )))
  }

  # Check max(N)
  if (max(N) >= round(length(unique(data$location))/k)) {
    stop("\nN is too large for this multi-cell test.")
  }

  # Check lookback_window
  if (lookback_window <= 0) {
    lookback_window <- 1
  }

  # CPIC for Multiple Cells
  if(length(cpic) != k && length(cpic) > 1){
    stop("\nEnter a CPIC for each cell or a single average CPIC for all cells.")
  } else if(length(cpic == 1)){
    cpic <- rep(cpic,k)
  }

  # Create ranking by KPI
  rank_by_loc <- data %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(sum_y = base::sum(Y)) %>%
    dplyr::arrange(dplyr::desc(sum_y)) %>%
    dplyr::mutate(rank = dplyr::row_number())

  #Number of locations
  locs_N <- nrow(rank_by_loc)

  # Systematic Sampling
  if(tolower(sampling_method) == "systematic"){
    r <- sample(1:k, k)

    rank_by_loc$cell <- 0

    for (i in 1:length(r)){
      rank_by_loc$cell[seq(r[i], locs_N, k)] <- i
    }
  }

  # Initializing output objects
  GeoLift_Markets <- list()
  TopChoices <- data.frame(matrix(ncol = 11, nrow = 0))

  for (cell_id in 1:k){
    locations <- rank_by_loc$location[rank_by_loc$cell == cell_id]
    data_aux <- data %>% dplyr::filter(location %in% locations)

    MarketSelections <- suppressMessages(GeoLiftMarketSelection(data = data_aux,
                                                                treatment_periods = treatment_periods,
                                                                N = N,
                                                                X = X,
                                                                effect_size = effect_size,
                                                                lookback_window = lookback_window,
                                                                cpic = cpic[cell_id],
                                                                alpha = alpha,
                                                                normalize = normalize,
                                                                model = model,
                                                                fixed_effects = fixed_effects,
                                                                dtw = dtw,
                                                                Correlations = Correlations,
                                                                print = FALSE,
                                                                parallel = parallel,
                                                                run_stochastic_process = run_stochastic_process,
                                                                parallel_setup = parallel_setup,
                                                                side_of_test = side_of_test
    ))

    MarketSelections$BestMarkets$cell <- cell_id

    #Re-scale ProportionTotal_Y to total DF values
    MarketSelections$BestMarkets$ProportionTotal_Y <- MarketSelections$BestMarkets$ProportionTotal_Y *
      sum(rank_by_loc$sum_y[rank_by_loc$location %in% locations]) /
      sum(sum(rank_by_loc$sum_y))

    MarketSelections$BestMarkets$Holdout <- ifelse(MarketSelections$BestMarkets$EffectSize < 0,
                                                   MarketSelections$BestMarkets$ProportionTotal_Y,
                                                   1 - MarketSelections$BestMarkets$ProportionTotal_Y)


    GeoLift_Markets <- append(GeoLift_Markets, list(MarketSelections))
    names(GeoLift_Markets)[cell_id] <- paste0("cell_",eval(cell_id))

    # Create top-choices
    TopChoices <- rbind(TopChoices, MarketSelections$BestMarkets[c("cell",
                                                                   "ID",
                                                                   "location",
                                                                   "duration",
                                                                   "EffectSize",
                                                                   "AvgScaledL2Imbalance",
                                                                   "abs_lift_in_zero",
                                                                   "Investment",
                                                                   "ProportionTotal_Y",
                                                                   "Holdout",
                                                                   "rank")][1:min(top_choices,
                                                                                  nrow(MarketSelections$BestMarkets)),])
  }

  TopChoices <- TopChoices %>% dplyr::arrange(cell, rank)

  res <- list(TopChoices = TopChoices,
              Models = GeoLift_Markets,
              data = data,
              test_details = list("X" = X,
                                  "Y_id" = Y_id,
                                  "location_id" = location_id,
                                  "time_id" = time_id,
                                  "k" = k,
                                  "cpic" = cpic,
                                  "alpha" = alpha,
                                  "model" = model,
                                  "fixed_effects" = fixed_effects,
                                  "side_of_test" = side_of_test,
                                  "lookback_window" = lookback_window,
                                  "parallel" = parallel,
                                  "parallel_setup" = parallel_setup))

  class(res) <- c("MultiCellMarketSelection", class(res))

  return(res)

}


#' Print pretty MultiCellMarketSelection output.
#'
#' @description
#'
#' Print MultiCellMarketSelection output.
#'
#' @param x MultiCellMarketSelection object.
#' @param ... Optional arguments
#'
#' @return
#' MultiCellMarketSelection output message
#'
#' @export

print.MultiCellMarketSelection <- function(x, ...) {
  if (!inherits(x, "MultiCellMarketSelection")) {
    stop("object must be class MultiCellMarketSelection")
  }

  print(x$TopChoices)

}


#' Multi-Cell Power Analysis Method.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `MultiCellPower` performs a Power-Analysis for a given set of Market Selections
#' obtained from `MultiCellMarketSelection`.
#'
#' @param x A `MultiCellMarketSelection` object.
#' @param test_markets List of market IDs per cell. The list must contain exactly
#' k numeric values corresponding to the power analysis. The recommended layout is
#' `list(cell_1 = 1, cell2 = 1, cell3 = 1,...)`.
#' @param effect_size A vector of effect sizes to test by default a
#' sequence between 0 - 25 percent in 5 percent increments: seq(0,0.25,0.05).
#' Make sure that the sequence includes zero.
#' @param lookback_window A number indicating how far back in time the simulations
#' for the power analysis should go. For instance, a value equal to 5 will simulate
#' power for the last five possible tests. By default lookback_window = 1 which
#' will only execute the most recent test based on the data.
#' @param side_of_test A string indicating whether confidence will be determined
#' using a one sided or a two sided test. If set to NULL (default), the `side_of_test`
#' will be populated by the value specified in `MultiCellMarketSelection`.
#' \itemize{
#'          \item{"two_sided":}{ The test statistic is the sum of all treatment effects, i.e. sum(abs(x)). Defualt.}
#'          \item{"one_sided":}{ One-sided test against positive or negaative effects i.e.
#'          If the effect being applied is negative, then defaults to -sum(x). H0: ES >= 0; HA: ES < 0.
#'          If the effect being applied is positive, then defaults to sum(x). H0: ES <= 0; HA: ES > 0.}
#'          }
#'
#' @return A 'MultiCellPower' object with three objects:
#' \itemize{
#'          \item{"PowerCurves":}{ List containing the Power Curves for each cell.}
#'          \item{"data":}{ The input data.}
#'          \item{"test_details":}{ The test details.}
#'          }
#'
#' @export

MultiCellPower <- function(x,
                           test_markets = list(),
                           effect_size = seq(-0.25,0.25,0.05),
                           lookback_window = NULL,
                           side_of_test = NULL){

  if (!inherits(x, "MultiCellMarketSelection")) {
    stop("object must be class MultiCellMarketSelection")
  }

  if(length(test_markets) != length(x$Models)){
    stop("\nMake sure an ID is provided for each cell in the analysis.")
  }

  if(!(all(sapply(test_markets, is.numeric)))){
    stop("\nMake sure all input IDs in test_markets are numeric.")
  }

  if(is.null(lookback_window)){
    lookback_window <- x$test_details$lookback_window
  }

  # Check side_of_test
  if (!is.null(side_of_test)){
    if(!(tolower(side_of_test) %in% c("one_sided", "two_sided"))){
      stop("\nEnter a valid side_of_test ('one_sided', 'two_sided').")
    }
  }

  PowerCurves <- c()
  test_locs <- c()

  # List of test locations
  for (cell in 1:length(test_markets)){
    test_locs <- append(test_locs, list(stringr::str_split(x[[2]][[cell]]$BestMarkets$location[test_markets[[cell]]], ", ")[[1]]))
  }


  for (cell in 1:length(test_markets)){
    PowerAux <- NULL
    data_aux <- x$data %>% dplyr::filter(!(location %in% unlist(test_locs[-c(cell)]) ))

    PowerAux <- suppressMessages(GeoLiftPower(data = data_aux,
                                              locations = test_locs[[cell]],
                                              effect_size = effect_size,
                                              treatment_periods = x[[2]][[cell]]$BestMarkets$duration[test_markets[[cell]]],
                                              lookback_window = lookback_window,
                                              cpic = x$test_details$cpic[cell],
                                              X = x$test_details$X,
                                              Y_id = x$test_details$Y_id,
                                              location_id = x$test_details$location_id,
                                              time_id = x$test_details$time_id,
                                              alpha = x$test_details$alpha,
                                              fixed_effects = x$test_details$fixed_effects,
                                              parallel = x$test_details$parallel,
                                              parallel_setup = x$test_details$parallel_setup,
                                              side_of_test = ifelse(is.null(side_of_test),
                                                                    x$test_details$side_of_test,
                                                                    side_of_test)))

    PowerCurves <- append(PowerCurves, list(PowerAux))
    names(PowerCurves)[cell] <- paste0("cell_",eval(cell))

  }

  res <- list(PowerCurves = PowerCurves,
              data = x$data,
              test_details = x$test_details)

  class(res) <- c("MultiCellPower", class(res))

  return(res)

}


#' Print pretty MultiCellPower output.
#'
#' @description
#'
#' Print MultiCellPower output.
#'
#' @param x MultiCellPower object.
#' @param ... Optional arguments
#'
#' @return
#' MultiCellPower output message
#'
#' @export

print.MultiCellPower <- function(x, ...) {
  if (!inherits(x, "MultiCellPower")) {
    stop("object must be class MultiCellPower")
  }

  print(x$PowerCurves)

}


#' Multi-Cell Winner Declaration Method for Market Selection.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `MultiCellWinner` explores the likelihood of observe a winning cell for a
#' given Multi-Cell Market Selection. This method analyzes all pairwise comparisons to
#' determine how much larger the incremental ROAS (iROAS) must be for a cell to be
#' declared the winner based on a statistical significance test.
#'
#' @param x A `MultiCellPower` object.
#' @param effect_size A numeric value representing the lift to be simulated
#' across all cells. If not specified (default), the algorithm will use the
#' largest lift needed to obtain a well-powered test across all cells.
#' @param geolift_type String that specifies the type of GeoLift test to be performed:
#' \itemize{
#'          \item{"standard":}{ Standard GeoLift test where ads are shown to test regions. Defualt.}
#'          \item{"inverse":}{ Inverse or Negative GeoLift test where the test group is holded-out
#'          from the treatment.}
#'          }
#' @param ROAS Vector of incremental Return on Ad Spend (iROAS) values to assess. Set to
#' `seq(0,5,0.05)` by default.
#' @param alpha Significance Level. By default 0.1.
#' @param method A string indicating the method used to calculate the
#' aggregate ATT Confidence Intervals.
#' \itemize{
#'          \item{"conformal":}{ Conformal Inference. Defualt.}
#'          \item{"jackknife+":}{ Jackknife+ (exclusively for stat_test = "Total").}
#'          }
#' @param stat_test A string indicating the test statistic.
#' \itemize{
#'          \item{"Total":}{ The test statistic is the sum of all treatment effects, i.e. sum(abs(x)). Default.}
#'          \item{"Negative":}{ One-sided test against positive effects i.e. -sum(x).
#'          Recommended for Negative Lift tests.}
#'          \item{"Positive":}{ One-sided test against negative effects i.e. sum(x).
#'          Recommended for Positive Lift tests.}
#' }
#'
#' @return A list with two objects:
#' \itemize{
#'          \item{"results":}{ Data frame with all pairwise comparisons and required
#'          iROAS needed to declare a winner for the multi-cell test.}
#'          \item{"simulations":}{ The complete data frame of all simulations for all
#'          pairwise comparisons. }
#'          }
#'
#' @export

MultiCellWinner <- function(x,
                            effect_size = NULL,
                            geolift_type = "standard",
                            ROAS = seq(0,5,0.05),
                            alpha = 0.1,
                            method = "conformal",
                            stat_test = "Total"
){

  if (!inherits(x, "MultiCellPower")) {
    stop("object must be class MultiCellPower")
  }

  # Check ROAS
  if (min(ROAS) < 0){
    stop("\nMake sure all ROAS values are positive.")
  } else if(min(ROAS == 0)){
    ROAS <- c(0, ROAS)
  }

  # Check effect_size
  if(length(effect_size) > 1){
    stop("\nPlease specify a single value of effect_size to analyze.")
  }

  # Check geolift_type
  if(!(tolower(geolift_type) %in% c("standard", "inverse"))){
    stop("\nPlease specify a valid geolift_type test ('standard', 'inverse').")
  }

  # Check method
  if(!(tolower(method) %in% c("conformal", "jackknife+"))){
    stop("\nPlease specify a valid method test ('conformal', 'jackknife+').")
  }

  # Check stat_test
  if(!(stat_test %in% c("Total", "Negative", "Positive"))){
    stop("\nEnter a valid stat_test ('Total', 'Negative', 'Positive').")
  }

  #Set variables
  test_params <- as.data.frame(matrix(0, ncol=5, nrow = length(x$PowerCurves)))
  names(test_params) <- c("locations", "effect_size", "duration", "cpic", "investment")
  test_locs <- c()

  for(cell in 1:length(x$PowerCurves)){
    if(tolower(geolift_type) == "standard"){
      aux <- x$PowerCurves[[cell]] %>%
        dplyr::filter(power > 0.8, EffectSize > 0 ) %>%
        dplyr::slice_min(EffectSize, n = 1)
    } else{
      aux <- x$PowerCurves[[cell]] %>%
        dplyr::filter(power > 0.8, EffectSize < 0 ) %>%
        dplyr::slice_max(EffectSize, n = 1)
    }
    test_params[cell,1] <- aux$location[1] #location
    test_params[cell,2] <- mean(aux$EffectSize) #effect_size
    test_params[cell,3] <- max(aux$duration) #duration
    test_params[cell,4] <- mean(aux$cpic) #cpic
    test_params[cell,5] <- mean(aux$Investment) #investment

    test_locs <- append(test_locs, list(stringr::str_split(x$PowerCurves[[cell]]$location[1], ", ")[[1]]))
  }

  # Set test parameters
  duration <- max(test_params$duration)
  if(is.null(effect_size)){
    effect_size <- ifelse(geolift_type == "standard", max(test_params$effect_size), min(test_params$effect_size))
  }

  # Resulting dataframe
  sims <- data.frame(matrix(ncol=12,nrow=0))
  colnames(sims) <- c("cell_A",
                      "incremental_A",
                      "lowerCI_Cell_A",
                      "upperCI_Cell_A",
                      "cell_B",
                      "incremental_B",
                      "lowerCI_Cell_B",
                      "upperCI_Cell_B",
                      "duration",
                      "base_lift",
                      "ROAS",
                      "DID")

  combinations <- combn(seq(1:length(x$PowerCurves)),2)

  for (combo in 1:ncol(combinations)){
    DID_aux <- 0
    i <- 1
    for (roas in ROAS){
      test_locs_aux <- c(test_locs[[combinations[,combo][1]]],test_locs[[combinations[,combo][2]]])
      data_aux <- x$data %>% dplyr::filter(!(location %in% unlist(test_locs)[!(unlist(test_locs) %in% test_locs_aux)]))

      data_aux$Y[data_aux$time >= (max(data_aux$time) - duration + 1) & data_aux$location %in% test_locs[[combinations[,combo][1]]]] <- data_aux$Y[data_aux$time >= (max(data_aux$time) - duration + 1) & data_aux$location %in% test_locs[[combinations[,combo][1]]]] * (1 + effect_size*roas)
      data_aux$Y[data_aux$time >= (max(data_aux$time) - duration + 1) & data_aux$location %in% test_locs[[combinations[,combo][2]]]] <- data_aux$Y[data_aux$time >= (max(data_aux$time) - duration + 1) & data_aux$location %in% test_locs[[combinations[,combo][2]]]] * (1 + effect_size)


      gl_a <- suppressMessages(GeoLift(Y_id = "Y",
                                       time_id = x$test_details$time_id,
                                       location_id = x$test_details$location_id,
                                       X = x$test_details$X,
                                       data = data_aux[!(data_aux$location %in% test_locs[[combinations[,combo][2]]]),],
                                       locations = test_locs[[combinations[,combo][1]]],
                                       treatment_start_time = max(data_aux$time) - duration + 1,
                                       treatment_end_time = max(data_aux$time),
                                       alpha = alpha,
                                       model = x$test_details$model,
                                       fixed_effects = x$test_details$fixed_effects,
                                       ConfidenceIntervals = TRUE,
                                       method = method,
                                       grid_size = 250,
                                       stat_test = stat_test))

      gl_b <- suppressMessages(GeoLift(Y_id = "Y",
                                       time_id = x$test_details$time_id,
                                       location_id = x$test_details$location_id,
                                       X = x$test_details$X,
                                       data = data_aux[!(data_aux$location %in% test_locs[[combinations[,combo][1]]]),],
                                       locations = test_locs[[combinations[,combo][2]]],
                                       treatment_start_time = max(data_aux$time) - duration + 1,
                                       treatment_end_time = max(data_aux$time),
                                       alpha = alpha,
                                       model = x$test_details$model,
                                       fixed_effects = x$test_details$fixed_effects,
                                       ConfidenceIntervals = TRUE,
                                       method = method,
                                       grid_size = 250,
                                       stat_test = stat_test))

      if(gl_a$upper_bound < gl_b$lower_bound){
        DID_aux <- 1
      } else if(gl_b$upper_bound < gl_a$lower_bound){
        DID_aux <- 1
      }

      sims <- rbind(sims, list("cell_A" = unlist(lapply(test_locs[combinations[,combo][1]], function(x) paste(x, collapse = ", "))),
                               "incremental_A" = gl_a$incremental,
                               "lowerCI_Cell_A" = round(gl_a$lower_bound, 2),
                               "upperCI_Cell_A" = round(gl_a$upper_bound, 2),
                               "cell_B" = unlist(lapply(test_locs[combinations[,combo][2]], function(x) paste(x, collapse = ", "))),
                               "incremental_B" = gl_b$incremental,
                               "lowerCI_Cell_B" = round(gl_b$lower_bound, 2),
                               "upperCI_Cell_B" = round(gl_b$upper_bound, 2),
                               "duration" = duration,
                               "base_lift" = effect_size,
                               "ROAS" = roas,
                               "DID" = DID_aux) )

      if(DID_aux == 1){break}

    }

  }

  results <- sims %>% dplyr::filter(DID == 1) %>% dplyr::select(-c(base_lift, DID))

  res <- list(results = results,
              simulations = sims)

  class(res) <- c("MultiCellWinner", class(res))

  if(nrow(results) < 1){
    message("No Winners were found. Please change the locations, baseline effect_size, or ROAS sequence to find a Winner.")
  }

  return(res)

}

#' Print pretty MultiCellWinner output.
#'
#' @description
#'
#' Print MultiCellWinner output.
#'
#' @param x MultiCellWinner object.
#' @param ... Optional arguments
#'
#' @return
#' MultiCellWinner output message
#'
#' @export

print.MultiCellWinner <- function(x, ...) {
  if (!inherits(x, "MultiCellWinner")) {
    stop("object must be class MultiCellWinner")
  }

  if(nrow(x$results) > 0){
    print(x$results)
  } else{
    message("No Winners were found. Please change the locations, baseline effect_size, or ROAS sequence to find a Winner.")
  }


}


#' GeoLiftMultiCell inference calculation.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `GeoLiftMultiCell` performs inference for a Multi-Cell GeoLift.
#'
#' @param Y_id Name of the outcome variable (String).
#' @param time_id Name of the time variable (String).
#' @param location_id Name of the location variable (String).
#' @param X List of names of covariates.
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param locations A list of lists of test markets per cell. The recommended layout is
#' `list(cell_1 = list("locA"), cell2 = list("locB"), cell3 = list("locC"),...)`.
#' @param treatment_start_time Time index of the start of the treatment.
#' @param treatment_end_time Time index of the end of the treatment.
#' @param alpha Significance level. Set to 0.1 by default.
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
#'          \item{"best:}{ Fits the model with the lowest Scaled L2 Imbalance.}
#'          }
#' @param fixed_effects A logic flag indicating whether to include unit fixed
#' effects in the model. Set to TRUE by default.
#' @param ConfidenceIntervals A logic flag indicating whether to estimate confidence intervals.
#' Set to FALSE by default.
#' @param method A string indicating the method used to calculate the
#' aggregate ATT Confidence Intervals.
#' \itemize{
#'          \item{"conformal":}{ Conformal Inference. Defualt.}
#'          \item{"jackknife+":}{ Jackknife+ (exclusively for stat_test = "Total").}
#' }
#' @param grid_size Number of grid points to use when inverting the hypothesis
#' test for Conformal Inference. Set to 250 by default.
#' @param stat_test A string indicating the test statistic.
#' \itemize{
#'          \item{"Total":}{ The test statistic is the sum of all treatment effects, i.e. sum(abs(x)). Default.}
#'          \item{"Negative":}{ One-sided test against positive effects i.e. -sum(x).
#'          Recommended for Negative Lift tests.}
#'          \item{"Positive":}{ One-sided test against negative effects i.e. sum(x).
#'          Recommended for Positive Lift tests.}
#' }
#' @param winner_declaration Logic flag indicating whether to compute a winner cell analysis.
#' If set to TRUE (default), both pairwise and total statistical significance tests will be
#' performed.
#' @param geolift_type String that specifies the type of GeoLift test to be performed:
#' \itemize{
#'          \item{"standard":}{ Standard GeoLift test where ads are shown to test regions. Defualt.}
#'          \item{"inverse":}{ Inverse or Negative GeoLift test where the test group is holded-out
#'          from the treatment.}
#'          }
#'
#' @return
#' `GeoLiftMultiCell` object that contains:
#'          \itemize{
#'          \item{"results":}{ List of `GeoLift` objects (one per cell).}
#'          \item{"pairwise":}{ Table of pairwise comparisons and pairwise winner declarations.)}
#'          \item{"Winner":}{ List of winner locations for the total comparison (if any).}
#'          }
#'
#' @export

GeoLiftMultiCell <- function(Y_id = "Y",
                             time_id = "time",
                             location_id = "location",
                             X = c(),
                             data,
                             locations,
                             treatment_start_time,
                             treatment_end_time,
                             alpha = 0.1,
                             model = "none",
                             fixed_effects = TRUE,
                             ConfidenceIntervals = TRUE,
                             method = "conformal",
                             grid_size = 250,
                             stat_test = "Total",
                             winner_declaration = TRUE,
                             geolift_type = "standard"){

  # Rename variables
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)

  # Check that locations is a list
  if(!(is.list(locations))){
    stop("\nPlease enter a list of test locations by cell.")
  }

  # Check that the locations are characters
  if(!(all(sapply(unlist(locations), is.character)))){
    stop("\nMake sure all test locations are characters.")
  }

  # Check geolift_type
  if(!(tolower(geolift_type) %in% c("standard", "inverse"))){
    stop("\nPlease specify a valid geolift_type test ('standard', 'inverse').")
  }

  # Check stat_test
  if(!(stat_test %in% c("Total", "Negative", "Positive"))){
    stop("\nEnter a valid stat_test ('Total', 'Negative', 'Positive').")
  }

  k <- length(locations)

  GeoLiftResults <- list()

  for(cell in 1:k){
    data_aux <- data %>% dplyr::filter(!(location %in% unlist(locations[-c(cell)]) ))
    aux <- GeoLift(Y_id = Y_id,
                   time_id = time_id,
                   location_id = location_id,
                   X = X,
                   data = data_aux,
                   locations = locations[[cell]],
                   treatment_start_time = treatment_start_time,
                   treatment_end_time = treatment_end_time,
                   alpha = alpha,
                   model = model,
                   fixed_effects = fixed_effects,
                   ConfidenceIntervals = ConfidenceIntervals,
                   method = method,
                   grid_size = grid_size,
                   stat_test = stat_test)

    GeoLiftResults <- rbind(GeoLiftResults, list(aux))
  }

  if(winner_declaration){
    combinations <- combn(seq(1:k),2)
    pairwise <- as.data.frame(matrix(ncol = 7, nrow = 0))

    for (combo in 1:ncol(combinations)){
      aux_locs_a <- locations[combinations[,combo][1]]
      aux_locs_b <- locations[combinations[,combo][2]]
      winner_aux <- NA

      if (tolower(geolift_type) == "standard"){
        if(GeoLiftResults[[combinations[,combo][1]]]$lower_bound >
           GeoLiftResults[[combinations[,combo][2]]]$upper_bound){
          winner_aux <- unlist(lapply(aux_locs_a, function(x) paste(x, collapse = ", ")))
        } else if(GeoLiftResults[[combinations[,combo][2]]]$lower_bound >
                  GeoLiftResults[[combinations[,combo][1]]]$upper_bound){
          winner_aux <- unlist(lapply(aux_locs_b, function(x) paste(x, collapse = ", ")))
        }
      } else{
        if(GeoLiftResults[[combinations[,combo][1]]]$upper_bound <
           GeoLiftResults[[combinations[,combo][2]]]$lower_bound){
          winner_aux <- unlist(lapply(aux_locs_a, function(x) paste(x, collapse = ", ")))
        } else if(GeoLiftResults[[combinations[,combo][2]]]$upper_bound <
                  GeoLiftResults[[combinations[,combo][1]]]$lower_bound){
          winner_aux <- unlist(lapply(aux_locs_b, function(x) paste(x, collapse = ", ")))
        }
      }

      pairwise <- rbind(pairwise,c("Cell_A" = unlist(lapply(aux_locs_a, function(x) paste(x, collapse = ", "))),
                                   "Lower_A" = round(GeoLiftResults[[combinations[,combo][1]]]$lower_bound,2),
                                   "Upper_A" = round(GeoLiftResults[[combinations[,combo][1]]]$upper_bound,2),
                                   "Cell_B" = unlist(lapply(aux_locs_b, function(x) paste(x, collapse = ", "))),
                                   "Lower_B" = round(GeoLiftResults[[combinations[,combo][2]]]$lower_bound,2),
                                   "Upper_B" = round(GeoLiftResults[[combinations[,combo][2]]]$upper_bound,2),
                                   "Winner" = winner_aux))

      names(pairwise) <- c("Cell_A",
                           "Lower_A",
                           "Upper_A",
                           "Cell_B",
                           "Lower_B",
                           "Upper_B",
                           "Winner")

    }

    message(paste0(
      "\n##################################",
      "\n##### Pairwise  Comparisons #####\n",
      "##################################\n"))

    print(pairwise)

    winner <- NA

    for(cell in 1:k){
      aux_pairwise <- pairwise %>% dplyr::filter(Winner == unlist(lapply(locations[cell], function(x) paste(x, collapse = ", "))))
      n_wins <- nrow(aux_pairwise)
      if(n_wins == k-1){
        winner <- unlist(lapply(locations[cell], function(x) paste(x, collapse = ", ")))
        break
      }
    }

    if(!(is.na(winner))){
      message(paste0(
        "\n##################################",
        "\n#####   Winner Declaration   #####\n",
        "##################################\n\n",
      "* Winner Cell:\n",
      " ", toupper(winner)))
    }

  }


  res <- list(results = GeoLiftResults,
              pairwise_comparisons = pairwise,
              Winner = winner)

  class(res) <- c("GeoLiftMultiCell", class(res))

  return(res)

}


#' Print pretty GeoLiftMultiCell output.
#'
#' @description
#'
#' Print GeoLiftMultiCell output.
#'
#' @param x GeoLiftMultiCell object.
#' @param ... Optional arguments
#'
#' @return
#' GeoLiftMultiCell output message
#'
#' @export

print.GeoLiftMultiCell <- function(x, ...) {
  if (!inherits(x, "GeoLiftMultiCell")) {
    stop("object must be class GeoLiftMultiCell")
  }

  for(cell in 1:length(x$results)){

    if (x$results[[cell]]$inference$pvalue < 0.05) {
      is_significant <- "The results are significant at a 95% level."
    } else if (x$results[[cell]]$inference$pvalue < 0.10) {
      is_significant <- "The results are significant at a 90% level."
    } else if (x$results[[cell]]$inference$pvalue < 0.20) {
      is_significant <- "The results are significant at a 80% level."
    } else {
      is_significant <- "The results are not statistically significant."
    }

    if (toupper(x$results[[cell]]$stat_test) == "TOTAL") {
      test_type <- "TWO-SIDED LIFT TEST)"
    } else if (toupper(x$results[[cell]]$stat_test) == "POSITIVE") {
      test_type <- "ONE-SIDED POSITIVE LIFT TEST)"
    } else {
      test_type <- "ONE-SIDED NEGATIVE LIFT TEST)"
    }

    message(paste0(
      "##################################",
      "\n#####     Cell ",
      cell,
      " Results    #####\n",
      "##################################\n"))

    message(paste0(
      paste0(
        "Test results for ", (x$results[[cell]]$TreatmentEnd - x$results[[cell]]$TreatmentStart + 1),
        " treatment periods, from time-stamp ",
        x$results[[cell]]$TreatmentStart, " to ", x$results[[cell]]$TreatmentEnd,
        " for test markets:"
      )
    ))

    for (i in 1:length(x$results[[cell]]$test_id$name)) {
      message(paste(i, toupper(x$results[[cell]]$test_id$name[i])))
    }
    message(paste0(
      "##################################",
      "\n#####     Test Statistics    #####\n",
      "##################################\n",
      "\nPercent Lift: ",
      round(x$results[[cell]]$inference$Perc.Lift, 3), "%\n\n",
      "p-value: ",
      round(x$results[[cell]]$inference$pvalue, 2), "\n\n",
      "Incremental ", paste(x$results[[cell]]$Y_id), ": ", round(x$results[[cell]]$incremental, 0), "\n\n",
      "Average Estimated Treatment Effect (ATT): ", round(x$results[[cell]]$inference$ATT, 3),
      "\n\n", is_significant, " (", test_type,
      "\n\nThere is a ", round(100 * x$results[[cell]]$inference$pvalue, 2),
      "% chance of observing an effect this large or larger assuming treatment effect is zero.\n\n",
      sep = ""
    ))

  }

}


#' Summary method for GeoLiftMultiCell
#'
#' @description
#'
#' GeoLiftMultiCell summary output with additional information about the
#' test.
#'
#' @param object GeoLiftMultiCell object.
#' @param table Logic flag indicating whether to plot only a table
#' summarizing the results or the entire verbose output of each cell's
#' GeoLift object summary. FALSE by default.
#' @param ... Optional arguments
#'
#' @export
summary.GeoLiftMultiCell <- function(object,
                                     table = FALSE,
                                     ...) {
  if (!inherits(object, "GeoLiftMultiCell")) {
    stop("object must be class GeoLiftMultiCell")
  }

  if(!table){

    for(cell in 1:length(object$results)){

      message(paste0(
        "##################################",
        "\n#####     Cell ",
        cell,
        " Results    #####\n",
        "##################################"))

      print(summary(object$results[[cell]]))

      message(paste0("\n\n"))

    }

  } else{


    cells_aux <- list()
    locations_aux <- list()
    duration_aux <- list()
    lift_aux <- list()
    pvalue_aux <- list()
    incremental_aux <- list()
    ATT_aux <- list()
    stattest_aux <- list()
    statsig_aux <- list()
    progfunc_aux <- list()
    winner_aux <- list()

    for (cell in 1:length(object$results)){

      if (toupper(object$results[[cell]]$stat_test) == "TOTAL") {
        test_type <- "TWO-SIDED LIFT TEST"
      } else if (toupper(object$results[[cell]]$stat_test) == "POSITIVE") {
        test_type <- "ONE-SIDED POSITIVE LIFT TEST"
      } else {
        test_type <- "ONE-SIDED NEGATIVE LIFT TEST"
      }

      cells_aux <- append(cells_aux, cell)
      locations_aux <- append(locations_aux, toupper(paste0(object$results[[cell]]$test_id$name, collapse = ", ")))
      duration_aux <- append(duration_aux, (object$results[[cell]]$TreatmentEnd - object$results[[cell]]$TreatmentStart + 1))
      lift_aux <- append(lift_aux, paste0(round(object$results[[cell]]$inference$Perc.Lift, 3),"%"))
      pvalue_aux <- append(pvalue_aux, round(object$results[[cell]]$inference$pvalue, 2))
      incremental_aux <- append(incremental_aux, round(object$results[[cell]]$incremental, 0))
      ATT_aux <- append(ATT_aux, round(object$results[[cell]]$inference$ATT, 3))
      stattest_aux <- append(stattest_aux, test_type)
      statsig_aux <- append(statsig_aux, ifelse(object$results[[cell]]$inference$pvalue < object$results[[cell]]$summary$alpha, 1 ,0))
      progfunc_aux <- append(progfunc_aux, toupper(object$results[[cell]]$results$progfunc))
      ifelse(all(object$results[[cell]]$test_id$name %in% unlist(stringr::str_split(object$Winner[[1]], ", "))),
             winner_aux <- append(winner_aux, "Winner"),
             winner_aux <- append(winner_aux, ""))

    }

    printresults <- data.frame(Cell = unlist(cells_aux),
                               Location = unlist(locations_aux),
                               Duration = unlist(duration_aux),
                               Lift = unlist(lift_aux),
                               Incremental = unlist(incremental_aux),
                               ATT = unlist(ATT_aux),
                               pValue = unlist(pvalue_aux),
                               Stat_Test = unlist(stattest_aux),
                               Stat_Sig = unlist(statsig_aux),
                               Prognostic_Func = unlist(progfunc_aux),
                               Winner = unlist(winner_aux)
    )

    message(paste0(knitr::kable(printresults), collapse="\n"))
  }


}
