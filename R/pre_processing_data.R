# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function GeoDataRead, TrimControls.


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
