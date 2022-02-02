# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function fn_treatment, build_cluster, limit_test_markets,
# get_date_from_test_periods.


#' Auxiliary function to generate the treatment variable D.
#'
#' @description
#'
#' \code{fn_treatment} generates the treatment variable D, where
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
#' \code{limit_test_markets} determines if the user would like
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
