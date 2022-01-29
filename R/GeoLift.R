# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


#' Data reading function for GeoLift.
#'
#' @description
#'
#' \code{GeoDataRead} reads a data-frame and processes it for GeoLift.
#' The function will clean the data, generate a time variable that
#' increases by 1 for each time period (day/week/month), and aggregate
#' the data by time and location. It is important to have data for each
#' location and time-period and avoid special characters in the names of
#' the geographical units.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param date_id Name of the date variable (String).
#' @param location_id Name of the location variable (String).
#' @param Y_id Name of the outcome variable (String).
#' @param format Format of the dates in the data frame.
#' @param X List of names of the covariates.
#' @param summary Display a summary of the data-reading process. FALSE by default.
#' @param keep_unix_time A logic flag indicating whether to keep a column with
#' each event's unix time.
#'
#' @return
#' A data frame for GeoLift inference and power calculations.
#'
#' @export
GeoDataRead <- function(data,
                        date_id = "date",
                        location_id = "location",
                        Y_id = "units",
                        format = "mm/dd/yyyy",
                        X = c(),
                        summary = FALSE,
                        keep_unix_time = FALSE) {
  format <- tolower(format)

  # Acceptable date formats
  valid_formats_day <- c(
    "mm/dd/yyyy", "mm-dd-yyyy", "mm.dd.yyyy", "mmddyyyy",
    "dd/mm/yyyy", "dd-mm-yyyy", "dd.mm.yyyy", "ddmmyyyy",
    "yyyy/mm/dd", "yyyy-mm-dd", "yyyy.mm.dd", "yyyymmdd"
  )
  valid_formats_week <- c(
    "ww/yyyy", "ww-yyyy", "ww.yyyy", "wwyyyy",
    "yyyy/ww", "yyyy-ww", "yyyy.ww", "yyyyww"
  )
  valid_formats_month <- c(
    "mm/yyyy", "mm-yyyy", "mm.yyyy", "mmyyyy",
    "yyyy/mm", "yyyy-mm", "yyyy.mm", "yyyymm"
  )
  valid_formats <- c(valid_formats_day, valid_formats_week, valid_formats_month)

  if (!(format %in% valid_formats)) {
    message("Error: Please enter a valid date format. Valid formats are:")
    print(valid_formats)
    return(NULL)
  }

  # Rename variables to standard names used by GeoLift
  data <- data %>% dplyr::rename(
    date = date_id,
    Y = Y_id,
    location = location_id
  )

  # Remove white spaces in date variable
  data$date <- as.character(data$date)
  data$date <- trimws(data$date)

  # Location in lower-case for compatibility with GeoLift
  data$location <- tolower(data$location)
  initial_locations <- length(unique(data$location))

  # NEWCHANGE: Remove commas from locations
  data$location <- gsub(",", "", gsub("([a-zA-Z]),", "\\1", data$location))

  # Determine the separator
  if (str_count(format, pattern = fixed("/")) > 0) {
    sep <- "/"
  } else if (str_count(format, pattern = fixed("-")) > 0) {
    sep <- "-"
  } else if (str_count(format, pattern = fixed(".")) > 0) {
    sep <- "."
  } else {
    sep <- ""
  }

  # Make sure the data is complete for formats without sep
  if (sep == "" & min(nchar(data$date)) != nchar(format)) {
    message("Error: The length of the date is incorrect.")
    message("Make sure the entries have trailig zeroes (1/1/2012 -> 01/01/2012)")
    return(NULL)
  }

  # Remove separators

  if (format %in% valid_formats_day) {
    date_format <- gsub(sep, "", format) # Remove sep
    date_format <- gsub("yyyy", "Y", date_format)
    date_format <- gsub("mm", "m", date_format)
    date_format <- gsub("dd", "d", date_format)
    date_format <- unlist(strsplit(date_format, split = ""))

    reformat <- paste0("%", date_format[1], sep, "%", date_format[2], sep, "%", date_format[3])

    data$date_unix <- data$date
    # Create date in unix time
    data$date_unix <- data$date_unix %>%
      as.Date(reformat) %>%
      as.POSIXct(format = "%Y-%m-%d") %>%
      as.numeric()
  } else if (format %in% valid_formats_week) {
    date_format <- gsub(sep, "", format) # Remove sep
    date_format <- gsub("yyyy", "Y", date_format)
    date_format <- gsub("ww", "W", date_format)
    date_format <- unlist(strsplit(date_format, split = ""))

    reformat <- paste0("%", date_format[1], sep, "%", date_format[2], sep, "%w")

    data$date_unix <- data$date
    data$date_unix <- paste0(data$date_unix, sep, "0")
    # Create date in unix time
    data$date_unix <- data$date_unix %>%
      as.Date(reformat) %>%
      as.POSIXct(format = "%Y-%m-%d") %>%
      as.numeric()
  } else if (format %in% valid_formats_month) {
    date_format <- gsub(sep, "", format) # Remove sep
    date_format <- gsub("yyyy", "Y", date_format)
    date_format <- gsub("mm", "m", date_format)
    date_format <- unlist(strsplit(date_format, split = ""))

    reformat <- paste0("%", date_format[1], sep, "%", date_format[2], sep, "%d")

    data$date_unix <- data$date
    data$date_unix <- paste0(data$date_unix, sep, "1")
    # Create date in unix time
    data$date_unix <- data$date_unix %>%
      as.Date(reformat) %>%
      as.POSIXct(format = "%Y-%m-%d") %>%
      as.numeric()
  }

  # Remove NAs from date conversion
  if (sum(is.na(data$date_unix)) > 0) {
    message(paste0(sum(is.na(data$date_unix)), " rows dropped due to inconsistent time format."))
    data <- data[is.na(data$date_unix) == FALSE, ]
  }

  # Find the time increments
  time_increments <- unique(sort(data$date_unix, FALSE))[2] -
    unique(sort(data$date_unix, FALSE))[1]

  data$time <- (data$date_unix - min(data$date_unix)) /
    as.numeric(time_increments) + 1

  # Recode to avoid missing weeks (time increases always by 1)
  TimePeriods <- data.frame(time = sort(unique(data$time)))
  TimePeriods$ID <- seq.int(nrow(TimePeriods))

  data <- data %>% dplyr::left_join(TimePeriods, by = "time")
  data$time <- data$ID

  if (keep_unix_time == FALSE) {
    data <- subset(data, select = -c(date_unix, ID))
  } else {
    data <- subset(data, select = -c(ID))
  }

  # Remove revenue with zeroes
  # data <- filter(data, Y > 0)

  # Remove null conversion values
  data <- data[!is.na(data$Y), ]

  # Remove cities with missing time periods
  total_periods <- max(data$time)
  complete_cases <- table(data$location, data$time)
  complete_cases[complete_cases > 0] <- 1
  complete <- rowSums(complete_cases) == total_periods
  incomplete_locations <- length(complete) - length(complete[complete == TRUE])
  complete <- complete[complete == TRUE]
  data <- data %>% dplyr::filter(location %in% names(complete))

  # Aggregate Outcomes by time and location
  data_raw <- data

  if (keep_unix_time == FALSE) {
    data <- data_raw %>%
      dplyr::group_by(location, time) %>%
      dplyr::summarize(Y = sum(Y))
    for (var in X) {
      data_aux <- data_raw %>%
        dplyr::group_by(location, time) %>%
        dplyr::summarize(!!var := sum(!!sym(var)))
      data <- data %>% dplyr::left_join(data_aux, by = c("location", "time"))
    }
  } else {
    data <- data_raw %>%
      dplyr::group_by(location, time, date_unix) %>%
      dplyr::summarize(Y = sum(Y))
    for (var in X) {
      data_aux <- data_raw %>%
        dplyr::group_by(location, time, date_unix) %>%
        dplyr::summarize(!!var := sum(!!sym(var)))
      data <- data %>% dplyr::left_join(data_aux, by = c("location", "time", "date_unix"))
    }
  }

  # Print summary of Data Reading
  if (summary == TRUE) {
    message(paste0(
      "##################################",
      "\n#####       Summary       #####\n",
      "##################################\n",
      "\n* Raw Number of Locations: ", initial_locations,
      "\n* Time Periods: ", total_periods,
      "\n* Final Number of Locations (Complete): ", length(unique(data$location))
    ))
  }

  return(as.data.frame(data))
}


#' Helper to trim controls for shorter run-times with minimal loss
#' of precision.
#'
#' @description
#'
#' As the number of controls for a Geo test increases, the model
#' complexity grows as does the algorithm's run-time. However,
#' there are diminishing marginal returns in adding too many control
#' locations, especially if their time-series are very similar.
#' \code{TrimControls} provides a method to trim the number of controls
#' in order to reduce run-times with minimal loss of precision. In general,
#' it is recommended to have 4 to 5 times the number of controls locations
#' than the ones we have for test locations.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param Y_id Name of the outcome variable (String).
#' @param time_id Name of the time variable (String).
#' @param location_id Name of the location variable (String).
#' @param max_controls Max number of controls, recommended 4x-5x
#' the number of test locations.
#' @param test_locations List of test locations.
#' @param forced_control_locations List of locations to be forced
#' as controls.
#'
#' @return
#' A data frame with reduced control locations.
#'
#' @export
TrimControls <- function(data,
                         Y_id = "Y",
                         time_id = "time",
                         location_id = "location",
                         max_controls = 20,
                         test_locations = c(),
                         forced_control_locations = c()) {
  data <- data %>% dplyr::rename(
    time = time_id,
    Y = Y_id,
    location = location_id
  )

  if (max_controls > length(unique(data$location))) {
    print("Error: There can't be more controls than total locations.")
    return(NULL)
  }

  # Calculate the Average Time-Series
  avg_Y <- data %>%
    dplyr::group_by(time) %>%
    dplyr::summarize(Y_mean = mean(Y))

  # Append it to the data
  data_aux <- data %>% dplyr::left_join(avg_Y, by = "time")

  # Compute the difference for each time/location
  data_aux$diff <- data_aux$Y - data_aux$Y_mean

  # Calculate the average difference
  data_aux <- data_aux %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(mean_diff = mean(diff))

  # Calculate the percentiles for stratified sampling
  perc <- quantile(data_aux$mean_diff, probs = seq(0, 1, 0.2))
  perc <- unname(perc)

  data_aux$percentile <- 0

  for (location in 1:nrow(data_aux)) {
    for (percentile in 1:(length(perc) - 1)) {
      if (data_aux$mean_diff[location] > perc[percentile] &
        data_aux$mean_diff[location] <= perc[percentile + 1]) {
        data_aux$percentile[location] <- percentile
      }
    }
  }

  data_locs <- data_aux %>%
    dplyr::filter(!(location %in% test_locations)) %>%
    dplyr::group_by(percentile) %>%
    dplyr::sample_n(round(max_controls / length(perc)), replace = TRUE) %>%
    dplyr::distinct(location)

  final_locations <- unique(c(data_locs$location, test_locations, forced_control_locations))

  data <- data %>% dplyr::filter(location %in% final_locations)

  return(data)
}


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


#' Market selection tool.
#'
#' @description
#'
#' \code{MarketSelection} helps calculate the best markets based
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
                            dtw = 0) {
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
    matches = length(unique(data$location)) - 1
  )

  # Create a matrix with each row being the raked best controls for each location
  best_controls <- mm$BestMatches %>% tidyr::pivot_wider(
    id_cols = location,
    names_from = rank,
    values_from = BestControl
  )

  best_controls <- as.matrix(best_controls)
  colnames(best_controls) <- NULL

  return(best_controls)
}


#' Stochastic Market Selector.
#'
#' @description
#'
#' \code{build_stochastic_matrix} selects the markets to be tested
#' by randomly sampling from the \code{similarity_matrix}.
#' It gets groups of 2 elements and samples one of them. It repeats
#' this process until the \code{treatment_size} is equal to the sample.
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
    message("Deterministic setup with ", treatment_size, " locations in treatment.")
    return(similarity_matrix[, 1:treatment_size])
  } else {
    message("Random setup with ", treatment_size, " locations in treatment.")
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
    for (row in 1:nrow(sample_matrix)) { # Sort rows.
      sample_matrix[row, ] <- sort(sample_matrix[row, ])
    }

    return(unique(sample_matrix))
  }
}


#' Decides type of statistical function being applied for Conformal
#' Inference.
#'
#' @description
#'
#' \code{type_of_test} returns stat_func being used for GeoLiftPower;
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


#' Calculate p-value for GeoLift.
#'
#' @description
#'
#' \code{pvalueCalc} calculates the p-value for a GeoLift object.
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
                       sim, # Iterator: Sim number
                       max_time, # test end
                       tp, # treatment periods
                       es, # effect size
                       locations, # test locations to try out
                       cpic,
                       X,
                       type = "pValue",
                       normalize = FALSE,
                       fixed_effects = FALSE,
                       stat_func = stat_func,
                       model = "none") {
  treatment_start_time <- max_time - tp - sim + 2
  treatment_end_time <- treatment_start_time + tp - 1
  pre_test_duration <- treatment_start_time - 1
  pre_treatment_start_time <- 1

  if (normalize == TRUE) { # NEWCHANGE: Normalize by the sd
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
      progfunc = "GSYN",
      scm = T,
      fixedeff = fixed_effects
    )
  }

  ave_treatment_convs <- sum(ascm_obj$data$y[which(ascm_obj$data$trt == 1), ]) / sum(ascm_obj$data$trt == 1)
  ave_pred_control_convs <- predict(ascm_obj)[treatment_start_time:treatment_end_time]
  ave_incremental_convs <- ave_treatment_convs - sum(ave_pred_control_convs)

  att_estimator <- ave_incremental_convs / tp
  lift_estimator <- ave_incremental_convs / sum(ave_pred_control_convs)

  # NEWCHANGES: Option for quick imbalance-based calcs or p-value
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
      type = "iid",
      q = 1,
      ns = 1000,
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
