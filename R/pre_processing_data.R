# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function GeoDataRead, TrimControls, SplitTreatmentEstimation,
# ReplaceTreatmentSplit.


#' Data reading function for GeoLift.
#'
#' @description
#'
#' `GeoDataRead` reads a data-frame and processes it for GeoLift.
#' The function will clean the data, generate a time variable that
#' increases by 1 for each time period (day/week/month), and aggregate
#' the data by time and location. It is important to have data for each
#' location and time-period and avoid special characters in the names of
#' the geographical units.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the date, and covariates.
#' Valid date formats are: "mm/dd/yyyy", "mm-dd-yyyy", "mm.dd.yyyy", "mmddyyyy",
#' "dd/mm/yyyy", "dd-mm-yyyy", "dd.mm.yyyy", "ddmmyyyy",
#' "yyyy/mm/dd", "yyyy-mm-dd", "yyyy.mm.dd", "yyyymmdd", "ww/yyyy", "ww-yyyy",
#' "ww.yyyy", "wwyyyy", "yyyy/ww", "yyyy-ww", "yyyy.ww", "yyyyww",
#' "mm/yyyy", "mm-yyyy", "mm.yyyy", "mmyyyy", "yyyy/mm", "yyyy-mm",
#' "yyyy.mm", "yyyymm"
#' @param date_id Name of the date variable (String).
#' @param location_id Name of the location variable (String).
#' @param Y_id Name of the outcome variable (String).
#' @param format Format of the dates in the data frame.
#' @param X List of names of the covariates.
#' @param summary Display a summary of the data-reading process. FALSE by default.
#' @param keep_unix_time A logic flag indicating whether to keep a column with
#' each event's unix time.
#' @param longitude_col_name name of the longitude column in the location_points
#' data.frame.
#' @param latitude_col_name name of the latitude column in the location_points
#' data.frame.
#' @param verbose logical flag indicating whether amount of clusters processed
#' should be counted.
#' @inheritParams load_cluster_file
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
                        keep_unix_time = FALSE,
                        path_to_cluster_file_local=NULL,
                        country_filter=NULL,
                        path_to_file_url = paste0(
                          "https://data.humdata.org/dataset/",
                          "b7aaa3d7-cca2-4364-b7ce-afe3134194a2",
                          "/resource/3c068b51-5f0d-4ead-80ba-97312ec034e4/download/",
                          "data-for-good-at-meta-commuting-zones-march-2023.csv"),
                        longitude_col_name = "Longitude",
                        latitude_col_name = "Latitude",
                        verbose=FALSE) {
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

  # Remove commas from locations
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
    date_format <- gsub(sep, "", format)
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
    date_format <- gsub(sep, "", format)
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

  if (!is.null(path_to_cluster_file_local)){
    X <- cbind(X, c(longitude_col_name, latitude_col_name))
  }
  
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
  
  if (!is.null(path_to_cluster_file_local)){
    msg <- 'Running Community Zone cluster matching to city'
    if (is.null(country_filter)){
      msg <- paste0(msg, '. No country filter detected.',
                    'We suggest you specify the parameter',
                    'country_filter to avoid unnecessary preprocessing.')
    } else {
      msg <- paste0(msg, ' in ', country_filter, '.')
    }
    message(msg)
    cluster_data <- load_cluster_file(
      path_to_cluster_file_local,
      path_to_file_url=path_to_file_url,
      country_filter=country_filter
    )
    
    data <- location_to_cluster_matching(
      data, 
      cluster_data, 
      verbose=verbose,
      X=X)
    data[, longitude_col_name] <- NULL
    data[, latitude_col_name] <- NULL
  }

  # Print summary of Data Reading
  if (summary == TRUE) {
    msg <- paste0(
      "##################################",
      "\n#####       Summary       #####\n",
      "##################################\n",
      "\n* Raw Number of Locations: ", initial_locations,
      "\n* Time Periods: ", total_periods
    )
    if (!is.null(path_to_cluster_file_local)){
      msg <- paste0(
        msg,
        "\n* Clustered locations using Community Zones Dataset.",
        "\n* Grouped ", initial_locations,
        " locations into ", 
        length(unique(data$location)), " clusters.")
    } else {
      msg <- paste0(
        msg,
        "\n* Final Number of Locations (Complete): ", length(unique(data$location))
      )
    }
    message(msg)
    
  }

  return(as.data.frame(data))
}


#' GeoLift fit for each Treatment location within Treatment group.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `SplitTreatmentEstimation` fits a control group to each location within a 
#' Treatment group and calculates their imbalance metrics.
#' @param treatment_locations Vector of locations where the treatment was applied.
#' @param data DataFrame that GeoLfit will use to determine a result.
#' Should be the output of `GeoDataRead`.
#' @param treatment_start_time Time index of the start of the treatment.
#' @param treatment_end_time Time index of the end of the treatment.
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
#' @param verbose boolean that determines if processing messages will be shown.
#' @param Y_id Name of the outcome variable (String).
#' @param time_id Name of the time variable (String).
#' @param location_id Name of the location variable (String).
#' @param X Vector with covariates names.
#' @param fixed_effects A logic flag indicating whether to include unit fixed
#' effects in the model. Set to TRUE by default.
#' 
#' @return Dataframe with L2 imbalance ranking and these columns:
#'          \itemize{
#'          \item{"treatment_location":}{ Single Treatment location being considered.}
#'          \item{"l2_imbalance":}{ L2 imbalance for treatment_location estimation.}
#'          \item{"scaled_l2_imbalance":}{ Scaled L2 imbalance for treatment_location estimation.}
#'          \item{"treatment_group_size":}{ Size of treatment group for each iteration.}
#'          \item{"model":}{ Outcome model being used for Augmented Synthetic Control.}
#'        }
#' 
#' @export
SplitTreatmentEstimation <- function(
    treatment_locations,
    data,
    treatment_start_time,
    treatment_end_time,
    model,
    verbose=FALSE,
    Y_id = "Y",
    time_id = "time",
    location_id = "location",
    X = c(),
    fixed_effects = TRUE
){
  if (verbose){
    message(
      "Estimating control for each treatment location within treatment group.")
  }
  l2_imbalance_df <- data.frame()
  for (i in 1:length(treatment_locations)){
    treated_location <- treatment_locations[i]
    data_treated <- data[
      !data$location %in% treatment_locations[
        !treatment_locations %in% treated_location], ]
    
    augsynth_result_list <- ASCMExecution(
      data = data_treated,
      treatment_locations = treated_location,
      treatment_start_time = treatment_start_time,
      treatment_end_time = treatment_end_time,
      Y_id = Y_id,
      time_id = time_id,
      location_id = location_id,
      X = X,
      model = model,
      fixed_effects = fixed_effects)
    
    augsynth_model <- augsynth_result_list$augsynth_model
    
    y_hat <- predict(augsynth_model, att=FALSE)
    sum_pre_treatment_y_hat <- sum(y_hat[1:augsynth_model$t_int])
    
    treatment_df <- data.frame(
      treatment_location = treated_location,
      l2_imbalance = augsynth_model$l2_imbalance,
      l2_imbalance_to_y_hat = augsynth_model$l2_imbalance / sum_pre_treatment_y_hat,
      scaled_l2_imbalance = augsynth_model$scaled_l2_imbalance,
      treatment_group_size = length(treatment_locations),
      model = model
    )
    
    l2_imbalance_df <- rbind(l2_imbalance_df, treatment_df)
  }
  return(l2_imbalance_df)
}


#' Replace Treatment locations to be able to use them for continuous GeoLift studies.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `ReplaceTreatmentSplit` chooses the best treatment location to replace with their
#' control, given the L2 imbalance that each individual treatment has. Then 
#' re-estimates the remaining treatment locations using the replaced treatment as
#' part of the control donor pool.
#' @param treatment_locations Vector of locations where the treatment was applied.
#' @param data DataFrame that GeoLfit will use to determine a result.
#' Should be the output of `GeoDataRead`.
#' @param treatment_start_time Time index of the start of the treatment.
#' @param treatment_end_time Time index of the end of the treatment.
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
#' @param verbose boolean that determines if processing messages will be shown.
#' @param Y_id Name of the outcome variable (String).
#' @param time_id Name of the time variable (String).
#' @param location_id Name of the location variable (String).
#' @param X Vector with covariates names.
#' @param fixed_effects A logic flag indicating whether to include unit fixed
#' effects in the model. Set to TRUE by default.
#' 
#' @return
#' list that contains:
#'          \itemize{
#'          \item{"data":}{ Data with replaced values for treatment locations during treatment period.}
#'          \item{"l2_imbalance_df":}{ Ranking of treatment locations based on L2 imbalance for each iteration.}
#'        }
#' @export
ReplaceTreatmentSplit <- function(
    treatment_locations,
    data,
    treatment_start_time,
    treatment_end_time,
    model,
    verbose=FALSE,
    Y_id = "Y",
    time_id = "time",
    location_id = "location",
    X = c(),
    fixed_effects = TRUE){
  geo_data <- data[data$time <= treatment_end_time, ]
  data_after_treatment <- data[data$time > treatment_end_time, ]
  
  treatment_locations <- tolower(treatment_locations)
  l2_imbalance_df <- data.frame()
  problematic_treatments <- c()
  
  for (i in 1:length(treatment_locations)){
    iter_l2_imbalance_df <- SplitTreatmentEstimation(
      treatment_locations = treatment_locations,
      data = geo_data,
      treatment_start_time = treatment_start_time,
      treatment_end_time = treatment_end_time,
      model = model,
      verbose = verbose
    )
    l2_imbalance_df <- rbind(l2_imbalance_df, iter_l2_imbalance_df)
    
    treatment_to_replace <- iter_l2_imbalance_df[
      iter_l2_imbalance_df$l2_imbalance == min(iter_l2_imbalance_df$l2_imbalance), "treatment_location"]
    
    if (verbose){
      message("Replacing treatment location with lowest imbalance: ", 
              treatment_to_replace)
    }
    
    if (
      iter_l2_imbalance_df[
        iter_l2_imbalance_df$treatment_location == treatment_to_replace, 
        "l2_imbalance_to_y_hat"
      ] > 0.1){
      problematic_treatments <- c(problematic_treatments, treatment_to_replace)
    }
    geo_data_treated <- geo_data[
      !geo_data$location %in% treatment_locations[
        !treatment_locations %in% treatment_to_replace], ]
    
    augsynth_result_list <- ASCMExecution(
      data = geo_data_treated,
      treatment_locations = treatment_to_replace,
      treatment_start_time = treatment_start_time,
      treatment_end_time = treatment_end_time,
      Y_id = Y_id,
      time_id = time_id,
      location_id = location_id,
      X = X,
      model = model,
      fixed_effects = fixed_effects)
    
    augsynth_model <- augsynth_result_list$augsynth_model
    
    y_hat <- predict(augsynth_model, att=FALSE)
    geo_data[
      geo_data$location == treatment_to_replace &
        geo_data$time >= treatment_start_time, "Y"] <- y_hat[treatment_start_time:treatment_end_time]
    treatment_locations <- treatment_locations[treatment_locations != treatment_to_replace]
  }
  
  geo_data <- geo_data %>% dplyr::mutate(D = NULL)
  data <- rbind(geo_data, data_after_treatment)
  
  if (length(problematic_treatments) != 0){
    warning(
      paste0(
        "The following treatment locations could be problematic to replace:\n",
        " - ", paste0(problematic_treatments, collapse="\n - "),
        "\n Consider using an alternative replacement method for these series."))
  }
  
  return(list(data = data, l2_imbalance_df = l2_imbalance_df))
}


#' Helper to trim controls for shorter run-times with minimal loss
#' of precision.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' As the number of controls for a Geo test increases, the model
#' complexity grows as does the algorithm's run-time. However,
#' there are diminishing marginal returns in adding too many control
#' locations, especially if their time-series are very similar.
#' `TrimControls` provides a method to trim the number of controls
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


#' Define if location is in cluster, via lat and long.
#' 
#' @description
#' Match all location points to a single cluster defined by user movement between
#' cities.
#' @param location_points data.frame object that holds the location name,
#' latitude and longitude for all locations.
#' @param cluster_polygon sf::POLYGON object that has the points that are joined
#' to form a single cluster.
#' @param longitude_col_name name of the longitude column in the location_points
#' data.frame.
#' @param latitude_col_name name of the latitude column in the location_points
#' data.frame.
#' 
#' @return
#' A data frame holding a match between each location and the specific cluster.
#' 
#' @export
is_location_in_cluster <- function(
    location_points, 
    cluster_polygon,
    longitude_col_name='Longitude',
    latitude_col_name='Latitude'
){
  if (!all(c('region',
             'country',
             'fbcz_id',
             'fbcz_id_num') %in% colnames(cluster_polygon))){
    stop('Missing necessary columns in cluster_polygon.
         Include region, country, fbcz_id & fbcz_id_num.')
  }
  sf::st_crs(cluster_polygon) <- 4326
  spdf <- sf::as_Spatial(cluster_polygon)
  sp::coordinates(location_points) <- c(longitude_col_name, latitude_col_name)
  sp::proj4string(location_points) <- sf::st_crs(cluster_polygon)$proj4string
  
  overlap <- sp::over(location_points, spdf) %>%
    data.frame()
  overlap$included_in_cluster <- ifelse(is.na(overlap$country), FALSE, TRUE)
  overlap <- cbind(overlap, location_points)
  return(overlap[, c(
    'region',
    'country',
    'fbcz_id',
    'fbcz_id_num',
    longitude_col_name,
    latitude_col_name,
    'included_in_cluster',
    'location'
  )])
}


#' Match all locations to the cluster they belong.
#' 
#' @description 
#' Use clusters defined by how users move between cities to decide how cities 
#' should be aggregated to reduce contamination.
#' @param location_points data.frame object that holds the location name,
#' latitude and longitude for all locations.
#' @param clusters data.frame object with all polygons.  Each row represents a 
#' different cluster with an sf::POLYGON object that has the points that are joined
#' to form that cluster.
#' @param longitude_col_name name of the longitude column in the location_points
#' data.frame.
#' @param latitude_col_name name of the latitude column in the location_points
#' data.frame.
#' @param verbose logical flag indicating whether amount of clusters processed
#' should be counted.
#' @param X List of names of the covariates.
#' 
#' @return
#' A data frame holding a match between each location and all clusters.
#' 
#' @export
location_to_cluster_matching <- function(
    location_points, 
    clusters,
    longitude_col_name='Longitude',
    latitude_col_name='Latitude',
    verbose=FALSE,
    X=c()){
  
  all_point_match_to_cluster <- data.frame()
  max_fbcz_id_num <- max(clusters$fbcz_id_num)
  
  total_clusters <- nrow(clusters)
  for (row in 1:total_clusters){
    if (row %% 100 == 0 & verbose){
      message(paste0(row, ' out of ', total_clusters, ' clusters processed.'))
    }
    cluster_df <- clusters[row, ]
    point_match_to_cluster <- is_location_in_cluster(
      location_points, cluster_df)
    point_match_to_cluster <- point_match_to_cluster[
      point_match_to_cluster$included_in_cluster == TRUE, 
      !colnames(point_match_to_cluster) %in% c('location')]
    all_point_match_to_cluster <- rbind(
      all_point_match_to_cluster,
      point_match_to_cluster
    )
  }
  
  all_point_match_to_cluster <- all_point_match_to_cluster %>%
    merge(
      location_points, 
      by=c(longitude_col_name, latitude_col_name), 
      all.y=TRUE) %>%
    dplyr::distinct()
  all_point_match_to_cluster$included_in_cluster <- ifelse(
    is.na(all_point_match_to_cluster$included_in_cluster),
    FALSE,
    all_point_match_to_cluster$included_in_cluster
  )
  
  locations_without_cluster <- unique(all_point_match_to_cluster[
    all_point_match_to_cluster$included_in_cluster == FALSE, "location"])
  all_point_match_to_cluster <- all_point_match_to_cluster %>%
    merge(data.frame(
      location = locations_without_cluster,
      null_obs = 1:length(locations_without_cluster)
    ), 
    all.x=TRUE)
  
  all_point_match_to_cluster$fbcz_id_num = ifelse(
    all_point_match_to_cluster$included_in_cluster==FALSE,
    max_fbcz_id_num + all_point_match_to_cluster$null_obs,
    all_point_match_to_cluster$fbcz_id_num)
  all_point_match_to_cluster$null_obs <- NULL
  
  new_geo_data <- all_point_match_to_cluster %>%
    dplyr::group_by(fbcz_id_num, time) %>%
    dplyr::summarize(
      location_in_cluster = toString(unique(location)),
      Y = sum(Y),
      .groups='drop') %>%
    data.frame() %>%
    dplyr::ungroup()
  
  if (length(X) != 0){
    for (var in X){
      new_geo_data <- all_point_match_to_cluster %>%
        dplyr::group_by(fbcz_id_num, time) %>%
        dplyr::summarize(
          !!var := sum(!!sym(var)),
          .groups='drop') %>%
        dplyr::left_join(
          new_geo_data, 
          by = c("fbcz_id_num", "time")) %>%
        data.frame()
    }
  }
  
  new_geo_data <- new_geo_data %>%
    dplyr::mutate(
      location = as.character(fbcz_id_num),
      fbcz_id_num=NULL)
  return(new_geo_data)
}


#' Load Community Zones Clusters from Data For Good.
#' 
#' @description This method downloads the Community Zones Clusters CSV to local
#' and imports the file in a format compatible with the Shapefile library.
#' 
#' @param path_to_cluster_file_local Complete path where the downloaded file will 
#' be stored in local.
#' @param path_to_file_url default url to look for Community Zones Clusters.
#' @param country_filter specific country in which clusters should be located. 
#' Default is NULL.
#' 
#' @return shapefile data.frame that holds values for region, country, 
#' fbcz_id (cluster id), fbcz_id_num (cluster number), name, cz_gen_ds (date of 
#' update), win_population, win_roads_km, area, geography (polygon with points 
#' that form the cluster).
#' 
#' @export 
load_cluster_file <- function(
    path_to_cluster_file_local,
    path_to_file_url = paste0(
      'https://data.humdata.org/dataset/',
      'b7aaa3d7-cca2-4364-b7ce-afe3134194a2',
      '/resource/3c068b51-5f0d-4ead-80ba-97312ec034e4/download/',
      'data-for-good-at-meta-commuting-zones-march-2023.csv'),
    country_filter = NULL
){
  if (!file.exists(path_to_cluster_file_local)){
    message('File does not exist in local path. Downloading.')
    utils::download.file(path_to_file_url, path_to_cluster_file_local)
  } else {
    message(paste0('File was found in local path: ', path_to_cluster_file_local))
  }
  
  df <- read.csv(path_to_cluster_file_local)
  s_df <- sf::st_as_sf(df, wkt='geography')
  
  if (!is.null(country_filter)){
    country_filter <- tolower(country_filter)
    s_df <- s_df[tolower(s_df$country) == country_filter, ]
  }
  
  return(s_df)
}