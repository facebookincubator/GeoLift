# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function fn_treatment, build_cluster, limit_test_markets,
# get_date_from_test_periods.


#' Auxiliary function to generate the treatment variable D.
#'
#' @description
#'
#' `fn_treatment` generates the treatment variable D, where
#' D = 1 for test locations in the test period and D = 0 otherwise.
#'
#' @param df A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param locations List of names of the test locations (String).
#' @param treatment_start_time Time index of the start of the treatment.
#' @param treatment_end_time Time index of the end of the treatment.
#'
#' @return
#' Data frame with the additional treatment variable (D).
#'
#' @export
fn_treatment <- function(df,
                         locations,
                         treatment_start_time,
                         treatment_end_time) {
  df$D <- 0
  df$D[df$location %in% locations &
    treatment_start_time <= df$time &
    df$time <= treatment_end_time] <- 1

  df <- df %>% dplyr::filter(time <= treatment_end_time) # Remove periods after treatment

  return(df)
}



#' Build cluster to parallelize operations across nodes in machine.
#'
#' @description
#' This function builds the cluster and imports the necessary packages
#' to run Geolift: augsynth, dplyr and tidyr.
#'
#' @param parallel_setup A string indicating parallel workers set-up.
#' Can be "sequential" or "parallel".
#' @param import_augsynth_from Points to where the augsynth package
#' should be imported from to send to the nodes.
#' @return
#' Cluster object that will parallelize operations.
#' @export
build_cluster <- function(parallel_setup,
                          import_augsynth_from) {
  message("Setting up cluster.")
  if (parallel_setup == "sequential") {
    cl <- parallel::makeCluster(parallel::detectCores() - 1, setup_strategy = parallel_setup)
  } else if (parallel_setup == "parallel") {
    cl <- parallel::makeCluster(parallel::detectCores() - 1, setup_strategy = parallel_setup, setup_timeout = 0.5)
  } else {
    stop('Please specify a valid set-up. Can be one of "sequential" or "parallel".')
  }

  doParallel::registerDoParallel(cl)

  message("Importing functions into cluster.")
  parallel::clusterCall(cl, function() {
    eval(parse(text = import_augsynth_from))
  })

  parallel::clusterCall(cl, function() {
    attachNamespace("dplyr")
  })
  parallel::clusterCall(cl, function() {
    attachNamespace("tidyr")
  })

  parallel::clusterExport(
    cl,
    c("fn_treatment", "pvalueCalc", "MarketSelection"),
    envir = environment()
  )

  return(cl)
}


#' Limit the amount of test market potential elements.
#'
#' @description
#'
#' `limit_test_markets` determines if the user would like
#' to find combinations of treatments that is higher than half
#' the total amount of locations available while running a random
#' assignment of locations. If this is the case, it will cap the
#' treatment sizes at half the total amount of locations.
#'
#' @param similarity_matrix Matrix that sorts each location in terms
#' of descending correlation.
#' @param treatment_sizes Vector of number of test markets to calculate
#' power for.

#' @param run_stochastic_process A logic flag indicating whether to select test
#' markets through random sampling of the the similarity matrix. Given that
#' interpolation biases may be relevant if the synthetic control matches
#' the characteristics of the test unit by averaging away large discrepancies
#' between the characteristics of the test and the units in the synthetic controls,
#' it is recommended to only use random sampling after making sure all units are
#' similar. Set to FALSE by default.
#'
#' @return
#' Returns the limited number of treatment sizes if required. If the condition
#' is not met, it returns the original treatment sizes.
#'
#' @export
limit_test_markets <- function(similarity_matrix,
                               treatment_sizes,
                               run_stochastic_process = FALSE) {
  if (
    run_stochastic_process == TRUE &
      max(treatment_sizes) > 0.5 * ncol(similarity_matrix)) {
    message(
      "Maximum number of test markets ",
      max(treatment_sizes),
      " is greater than half the total number of test markets ",
      0.5 * ncol(similarity_matrix),
      ". Limiting number of test markets to less or equal than half total locations."
    )
    treatment_sizes <- treatment_sizes[treatment_sizes <= 0.5 * ncol(similarity_matrix)]
  }

  return(treatment_sizes)
}


#' Link dates to GeoLift time periods.
#'
#' @description
#'
#' Link dates to GeoLift time periods.
#'
#' @param GeoLift GeoLift object.
#' @param treatment_end_date Character that represents a date in year-month=day format.
#' @param frequency Character that represents periodicity of time stamps. Can be either
#' weekly or daily. Defaults to daily.
#'
#' @return
#' List that contains:
#'          \itemize{
#'          \item{"date_vector":}{ Vector of dates, going from first pre test time to end of test.}
#'          \item{"treatment_start":}{ start date of study.}
#'          \item{"treatment_end":}{ End date of study.}
#'         }
#'
#' @export
get_date_from_test_periods <- function(GeoLift, treatment_end_date, frequency = "daily") {
  treatment_end_period <- GeoLift$TreatmentEnd
  treatment_start_period <- GeoLift$TreatmentStart
  if (tolower(frequency) == "daily") {
    date_vector <- seq(
      as.Date(treatment_end_date) - treatment_end_period + 1,
      as.Date(treatment_end_date),
      by = "day"
    )
  } else if (tolower(frequency) == "weekly") {
    date_vector <- seq(
      as.Date(treatment_end_date) - treatment_end_period * 7 + 7,
      as.Date(treatment_end_date),
      by = "week"
    )
  } else {
    stop("If converting time periods to dates, specify frequency param. Can be 'daily' or 'weekly'.")
  }

  treatment_start_date <- date_vector[treatment_start_period]

  return(list(
    date_vector = as.Date(date_vector),
    treatment_start = as.Date(treatment_start_date),
    treatment_end = as.Date(treatment_end_date)
  ))
}

#' Calculate the total and test market's aggregated KPI by time and location.
#'
#' @description
#'
#' Append the aggregated KPI values for the test market and all markets to the
#' `GeoDataRead` object.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param locs list of test locations. If none are provided, only the aggregated
#' KPI (total) will be calculated and appended. Set to NULL by default.
#'
#' @return
#' A data frame with additional values for the test regions (combined_test)
#' and total KPI (total).
#'
#' @export
AppendAgg <- function(data, locs = NULL){

  if(is.null(locs)){
    aux <- data %>%
      dplyr::group_by(time) %>%
      dplyr::summarise(Y = sum(Y))
    aux$location <- "control_markets"
    aux <- dplyr::bind_rows(data, aux)
  } else if(!(all(tolower(locs) %in% tolower(unique(data$location))))){
    message("Please specify a valid vector of location names.")
    return(NULL)
  } else{
    aux <- data %>%
      dplyr::filter(!(location %in% locs)) %>%
      dplyr::group_by(time) %>%
      dplyr::summarise(Y = sum(Y))
    aux$location <- "control_markets"
  }


  if(all(tolower(locs) %in% tolower(unique(data$location)))){
    auxCombo <- data %>%
      dplyr::filter(location %in% locs) %>%
      dplyr::group_by(time) %>%
      dplyr::summarise(Y = sum(Y))
    auxCombo$location <- "test_markets"
    aux <- dplyr::bind_rows(aux, auxCombo)
  } else{
    message("Please specify a valid vector of location names.")
    return(NULL)
  }

  return(as.data.frame(aux))

}

#' Auxiliary function to calculate correlations between input markets.
#'
#' @description
#'
#' Auxiliary function to calculate correlations between input markets.
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
#' @param markets List of markets to use in the calculation of the correlations.
#'
#' @return
#' MarketMatching object.
#'
#' @export
MarketCorrelations <- function(data,
                               location_id = "location",
                               time_id = "time",
                               Y_id = "Y",
                               dtw = 0,
                               markets = NULL) {
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  data$location <- tolower(data$location)
  astime <- seq(as.Date("2000/1/1"), by = "day", length.out = max(data$time))
  data$astime <- astime[data$time]

  # Find the best matches based on DTW
  mm <- MarketMatching::best_matches(
    data = data,
    id_variable = "location",
    date_variable = "astime",
    matching_variable = "Y",
    parallel = TRUE,
    warping_limit = 1,
    dtw_emphasis = dtw,
    start_match_period = min(data$astime),
    end_match_period = max(data$astime),
    markets_to_be_matched = markets,
    matches = length(unique(data$location)) - 1
  )

  return(mm)

}

#' Calculate correlations between input markets.
#'
#' @description
#'
#' Calculate correlations between input markets
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param locs List of markets to use in the calculation of the correlations.
#' @param dtw Emphasis on Dynamic Time Warping (DTW), dtw = 1 focuses exclusively
#' on this metric while dtw = 0 (default) relies on correlations only.
#'
#' @return
#' Correlation coefficient.
#'
#' @export
GetCorrel <- function(data, locs = c(), dtw = 0){

  if(!(all(tolower(locs) %in% tolower(unique(data$location))))){
    warning("Please specify a valid set of test locations.")
  }

  data_aux <- AppendAgg(data, locs = locs)

  Correl <- MarketCorrelations(data_aux[data_aux$location %in% c("control_markets", "test_markets"),], dtw = dtw)
  return(Correl$BestMatches$Correlation[[1]])
}

#' Function to obtain the synthetic control weights.
#'
#' @description
#'
#' `GetWeights` returns the synthetic control weights as a data frame
#' for a given test set-up.
#'
#' @param Y_id Name of the outcome variable (String).
#' @param time_id Name of the time variable (String).
#' @param location_id Name of the location variable (String).
#' @param X List of names of covariates.
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param locations List of test locations.
#' @param pretreatment_end_time Time index of the last pre-treatment period.
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
#'
#' @return
#' Data frame with the locations and weights.
#'
#' @export
GetWeights <- function(Y_id = "Y",
                       time_id = "time",
                       location_id = "location",
                       X = c(),
                       data,
                       locations,
                       pretreatment_end_time,
                       model = "none",
                       fixed_effects = TRUE) {

  # Rename variables to standard names used by GeoLift
  data <- data %>% dplyr::rename(
    time = time_id,
    Y = Y_id,
    location = location_id
  )

  data$location <- tolower(data$location)
  locations <- tolower(locations)

  # Add filler rows if all time-stamps will be used for weight calculation
  if(pretreatment_end_time == max(data$time)){
    aux_rows <- data[data$time == max(data$time),]
    aux_rows$time <- aux_rows$time + 1
    data <- data %>% dplyr::add_row(aux_rows)
    treatment_start_time <- max(data$time)
    treatment_end_time <- max(data$time)
  } else{
    treatment_start_time <- pretreatment_end_time + 1
    treatment_end_time <- max(data$time)
  }

  data_aux <- fn_treatment(data,
                           locations = locations,
                           treatment_start_time,
                           treatment_end_time
  )

  if (length(X) == 0) {
    fmla <- as.formula("Y ~ D")
  } else if (length(X) > 0) {
    fmla <- as.formula(paste(
      "Y ~ D |",
      sapply(list(X),
             paste,
             collapse = "+"
      )
    ))
  }

  # Single Augsynth
  augsyn <- augsynth::augsynth(fmla,
                               unit = location, time = time,
                               data = data_aux,
                               t_int = treatment_start_time,
                               progfunc = model,
                               scm = T,
                               fixedeff = fixed_effects
  )

  results <- data.frame(
    location = dimnames(augsyn$weights)[[1]],
    weight = unname(augsyn$weights[, 1])
  )

  return(results)
}
