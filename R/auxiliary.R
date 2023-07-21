# Copyright (c) Meta Platforms, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function fn_treatment, build_cluster, limit_test_markets,
# get_date_from_test_periods, MarketCorrelations, AppendAgg,
# CorrelationCoefficient, GetWeights


#' Auxiliary function to generate the treatment variable D.
#'
#' @description
#' `r lifecycle::badge("stable")`
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
#' `r lifecycle::badge("stable")`
#'
#' This function builds the cluster and imports the necessary packages
#' to run Geolift: augsynth, dplyr and tidyr.
#'
#' @param parallel_setup A string indicating parallel workers set-up.
#' Can be "sequential" or "parallel".
#' @param import_augsynth_from Points to where the augsynth package
#' should be imported from to send to the nodes.
#' @param import_tidyr_from Points to where the tidyr package
#' should be imported from to send to the nodes.
#' @return
#' Cluster object that will parallelize operations.
#' @export
build_cluster <- function(parallel_setup,
                          import_augsynth_from,
                          import_tidyr_from) {
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
    eval(parse(text = import_tidyr_from))
  })

  parallel::clusterCall(cl, function() {
    attachNamespace("dplyr")
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
#' `r lifecycle::badge("stable")`
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

    if (length(treatment_sizes) == 0) {
      treatment_sizes <- 0.5 * ncol(similarity_matrix)
    }
  }

  if (ncol(similarity_matrix) < min(treatment_sizes)) {
    stop("\nMinimum N param is larger than markets after exclusion.  Consider excluding less markets.")
  }

  return(treatment_sizes)
}


#' Link dates to GeoLift time periods.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Link dates to GeoLift time periods.
#'
#' @param GeoLift GeoLift object.
#' @param treatment_end_date Character that represents a date in year-month=day format.
#' @param post_treatment_periods Number of post-treatment periods. Zero by default.
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
get_date_from_test_periods <- function(GeoLift,
                                       treatment_end_date,
                                       post_treatment_periods = 0,
                                       frequency = "daily") {
  treatment_end_period <- GeoLift$TreatmentEnd
  treatment_start_period <- GeoLift$TreatmentStart
  if (tolower(frequency) == "daily") {
    date_vector <- seq(
      as.Date(treatment_end_date) - treatment_end_period + post_treatment_periods + 1,
      as.Date(treatment_end_date) + post_treatment_periods,
      by = "day"
    )
  } else if (tolower(frequency) == "weekly") {
    date_vector <- seq(
      as.Date(treatment_end_date) - treatment_end_period * 7 + 7,
      as.Date(treatment_end_date) + post_treatment_periods * 7,
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
#' `r lifecycle::badge("stable")`
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
AppendAgg <- function(data, locs = NULL) {
  if (is.null(locs)) {
    aux <- data %>%
      dplyr::group_by(time) %>%
      dplyr::summarise(Y = sum(Y), .groups = "drop") %>%
      dplyr::mutate(location = "control_markets")
    aux <- dplyr::bind_rows(data, aux)
  } else if (!(all(tolower(locs) %in% tolower(unique(data$location))))) {
    stop("Please specify a valid vector of location names.")
  } else {
    aux <- data %>%
      dplyr::mutate(
        location = ifelse(
          !(location %in% locs),
          "control_markets",
          "test_markets"
        )
      ) %>%
      dplyr::group_by(location, time) %>%
      dplyr::summarise(Y = sum(Y), .groups = "drop")
  }

  return(as.data.frame(aux))
}

#' Calculate correlations between input markets.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Calculate correlations between input markets
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param locs List of markets to use in the calculation of the correlations.
#'
#' @return
#' Correlation coefficient.
#'
#' @export
CorrelationCoefficient <- function(data, locs = c()) {
  data_aux <- AppendAgg(data, locs = locs)
  data_aux <- data_aux[data_aux$location %in% c("control_markets", "test_markets"), ] %>%
    tidyr::pivot_wider(names_from = location, values_from = Y, id_cols = time)
  cor_coefficient <- cor(data_aux$control_markets, data_aux$test_markets)
  return(cor_coefficient)
}

#' Function to obtain the synthetic control weights.
#'
#' @description
#' `r lifecycle::badge("stable")`
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
  if (pretreatment_end_time == max(data$time)) {
    aux_rows <- data[data$time == max(data$time), ]
    aux_rows$time <- aux_rows$time + 1
    data <- data %>% dplyr::add_row(aux_rows)
    treatment_start_time <- max(data$time)
    treatment_end_time <- max(data$time)
  } else {
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

#' Function to obtain the synthetic control weights.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `GetMultiCellWeights` returns the synthetic control weights as a data frame
#' for a given test set-up.
#'
#' @param x A `MultiCellMarketSelection` object.
#' @param test_markets List of market IDs per cell. The list must contain exactly
#' k numeric values corresponding to the power analysis. The list's layout must be:
#' `list(cell_1 = 1, cell2 = 1, cell3 = 1,...)`.
#'
#' @return
#' Data-frame with the locations and the synthetic control weights for each cell.
#'
#' @export
#' @rdname MultiCellMarketSelection
#' @order 2
GetMultiCellWeights <- function(x,
                                test_markets = list()) {
  if (!inherits(x, "MultiCellMarketSelection")) {
    stop("object must be class MultiCellMarketSelection")
  }

  if (length(test_markets) != length(x$Models)) {
    stop("\nMake sure an ID is provided for each cell in the analysis.")
  }

  if (!(all(sapply(test_markets, is.numeric)))) {
    stop("\nMake sure all input IDs in test_markets are numeric.")
  }

  # Check test_market
  for (cell_name in names(test_markets)) {
    split_cell_name <- strsplit(cell_name, "_")[[1]]
    if (split_cell_name[1] != "cell") {
      stop(
        paste0(
          "Test locations test_locs must have names as cell_{numeric}.",
          "\nCheck ", cell_name, "."
        )
      )
    }
    if (is.na(as.integer(split_cell_name[2]))) {
      stop(
        paste0(
          "Test locations test_locs must have names as cell_{numeric}.",
          "\nCheck ", cell_name, "."
        )
      )
    }
  }

  # Order input list of test markets
  test_markets <- test_markets[order(names(test_markets))]

  # Initialize Variables
  locations <- data.frame(location = unique(x$data$location))
  other_test_locs <- c()

  # List of all treatment locations
  for (cell in 1:length(test_markets)) {
    other_test_locs <- rbind(other_test_locs, list(stringr::str_split(
      x$Models[[cell]]$BestMarkets$location[test_markets[[cell]]], ", "
    )[[1]]))
  }

  # Adjust dataset & obtain weights.
  for (cell in 1:length(test_markets)) {
    data_aux <- x$data %>%
      dplyr::rename(
        Y = paste0(x$test_details$Y_id),
        location = paste0(x$test_details$location_id),
        time = paste0(x$test_details$time_id)
      ) %>%
      dplyr::filter(!(location %in% unlist(other_test_locs[-cell])))

    aux <- merge(locations, GetWeights(
      X = x$test_details$X,
      data = data_aux,
      locations = other_test_locs[[cell]],
      pretreatment_end_time = max(data_aux$time),
      model = x$test_details$model,
      fixed_effects = x$test_details$fixed_effects
    ),
    all.x = TRUE
    )

    # Create final data frame
    if (cell == 1) {
      results <- aux
    } else {
      results <- dplyr::inner_join(results, aux, by = "location")
    }

    # Format final data frame
    results <- results %>%
      dplyr::rename_with(.cols = cell + 1, ~ paste0("Cell_", cell))
  }

  return(results)
}


#' Get within market correlations for all locations.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' `MarketCorrelations` calculates a notion of similarity between markets
#' to help inform the combinations of treatments.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#'
#' @return A Dataframe where each column represents the closest market to the
#' market in the first column, ordering them by their correlation factor.
#'
#' @export
MarketCorrelations <- function(data) {
  pivoted_data <- data %>%
    tidyr::pivot_wider(id_cols = time, names_from = location, values_from = Y) %>%
    dplyr::select(!time)

  correlation_df <- pivoted_data %>%
    as.matrix() %>%
    cor() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "var1") %>%
    tidyr::pivot_longer(cols = -var1, names_to = "var2", values_to = "correlation")

  sorted_correlation_df <- correlation_df %>%
    dplyr::arrange(var1, -correlation) %>%
    dplyr::mutate(
      name_vble = rep(
        paste0("location_", 2:(ncol(pivoted_data) + 1)),
        ncol(pivoted_data)
      )
    ) %>%
    tidyr::pivot_wider(
      id_cols = var1,
      values_from = var2,
      names_from = name_vble
    ) %>%
    dplyr::select(!location_2)

  return(sorted_correlation_df)
}
