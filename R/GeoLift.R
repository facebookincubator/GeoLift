# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Suppress 'no visible binding for global variable' warnings
utils::globalVariables(
  c(
    'ATT',
    "Time",
    "lower",
    "upper",
    "diff_lower",
    "diff_upper",
    "conf_level",
    "conf.level",
    "date_unix",
    "ID",
    "time",
    "Y",
    "Level",
    "sim",
    "mean_p",
    "Units",
    "Type",
    "BestControl",
    "pow",
    "duration",
    "investment",
    "Level",
    "location",
    "ScaledL2Imbalance",
    "mean_pow",
    "mean_scaled_l2_imbalance",
    "mean_L2ScaledImbalance",
    "ProportionTotal_Y",
    "Total_Y",
    "Locs",
    "AvgCost",
    "main",
    "test",
    "significant",
    "lift",
    "treatment_start",
    "summ",
    "pvalue",
    "effect_size",
    "AvgScaledL2Imbalance",
    "Investment",
    "Holdout"
  )
)

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
  valid_formats_day <- c("mm/dd/yyyy", "mm-dd-yyyy", "mm.dd.yyyy", "mmddyyyy",
                         "dd/mm/yyyy", "dd-mm-yyyy", "dd.mm.yyyy", "ddmmyyyy",
                         "yyyy/mm/dd", "yyyy-mm-dd", "yyyy.mm.dd", "yyyymmdd")
  valid_formats_week <- c("ww/yyyy", "ww-yyyy", "ww.yyyy", "wwyyyy",
                          "yyyy/ww", "yyyy-ww", "yyyy.ww", "yyyyww")
  valid_formats_month <- c("mm/yyyy", "mm-yyyy", "mm.yyyy", "mmyyyy",
                           "yyyy/mm", "yyyy-mm", "yyyy.mm", "yyyymm")
  valid_formats <- c(valid_formats_day, valid_formats_week, valid_formats_month)

  if (!(format %in% valid_formats) ){
    message("Error: Please enter a valid date format. Valid formats are:")
    print(valid_formats)
    return(NULL)
  }

  # Rename variables to standard names used by GeoLift
  data <- data %>% dplyr::rename(date = date_id,
                                 Y = Y_id,
                                 location = location_id)

  # Remove white spaces in date variable
  data$date <- as.character(data$date)
  data$date <- trimws(data$date)

  # Location in lower-case for compatibility with GeoLift
  data$location <- tolower(data$location)
  initial_locations <- length(unique(data$location))

  #NEWCHANGE: Remove commas from locations
  data$location <- gsub(",", "", gsub("([a-zA-Z]),", "\\1", data$location))

  # Determine the separator
  if (str_count(format, pattern = fixed("/")) > 0){
    sep <- "/"
  } else if(str_count(format, pattern = fixed("-")) > 0) {
    sep <- "-"
  } else if(str_count(format, pattern = fixed(".")) > 0) {
    sep = "."
  } else {
    sep <- ""
  }

  # Make sure the data is complete for formats without sep
  if ( sep == "" & min(nchar(data$date)) != nchar(format)) {
    message("Error: The length of the date is incorrect.")
    message("Make sure the entries have trailig zeroes (1/1/2012 -> 01/01/2012)")
    return(NULL)
  }

  # Remove separators

  if (format %in% valid_formats_day){
    date_format <- gsub(sep,"",format) #Remove sep
    date_format <- gsub("yyyy","Y",date_format)
    date_format <- gsub("mm","m",date_format)
    date_format <- gsub("dd","d",date_format)
    date_format <- unlist(strsplit(date_format, split = ""))

    reformat <- paste0("%",date_format[1], sep, "%", date_format[2], sep, "%", date_format[3])

    data$date_unix <- data$date
    # Create date in unix time
    data$date_unix <- data$date_unix %>%
      as.Date(reformat) %>%
      as.POSIXct(format="%Y-%m-%d") %>%
      as.numeric()

  } else if (format %in% valid_formats_week) {
    date_format <- gsub(sep,"",format) #Remove sep
    date_format <- gsub("yyyy","Y",date_format)
    date_format <- gsub("ww","W",date_format)
    date_format <- unlist(strsplit(date_format, split = ""))

    reformat <- paste0("%",date_format[1], sep, "%", date_format[2], sep, "%w")

    data$date_unix <- data$date
    data$date_unix <- paste0(data$date_unix, sep, "0")
    # Create date in unix time
    data$date_unix <- data$date_unix %>%
      as.Date(reformat) %>%
      as.POSIXct(format="%Y-%m-%d") %>%
      as.numeric()

  } else if (format %in% valid_formats_month) {
    date_format <- gsub(sep,"",format) #Remove sep
    date_format <- gsub("yyyy","Y",date_format)
    date_format <- gsub("mm","m",date_format)
    date_format <- unlist(strsplit(date_format, split = ""))

    reformat <- paste0("%",date_format[1], sep, "%", date_format[2], sep, "%d")

    data$date_unix <- data$date
    data$date_unix <- paste0(data$date_unix, sep, "1")
    # Create date in unix time
    data$date_unix <- data$date_unix %>%
      as.Date(reformat) %>%
      as.POSIXct(format="%Y-%m-%d") %>%
      as.numeric()
  }

  #Remove NAs from date conversion
  if (sum(is.na(data$date_unix)) > 0){
    message(paste0(sum(is.na(data$date_unix)), " rows dropped due to inconsistent time format."))
    data <- data[is.na(data$date_unix) == FALSE,]
  }

  # Find the time increments
  time_increments <- unique(sort(data$date_unix, FALSE))[2] -
    unique(sort(data$date_unix, FALSE))[1]

  data$time <- (data$date_unix-min(data$date_unix))/
    as.numeric(time_increments) + 1

  #Recode to avoid missing weeks (time increases always by 1)
  TimePeriods <- data.frame(time = sort(unique(data$time)))
  TimePeriods$ID <- seq.int(nrow(TimePeriods))

  data <- data %>% dplyr::left_join(TimePeriods, by = "time")
  data$time <- data$ID

  if (keep_unix_time == FALSE){
    data <- subset(data, select = -c(date_unix, ID))
  } else {
    data <- subset(data, select = -c(ID))
  }


  #Remove revenue with zeroes
  #data <- filter(data, Y > 0)

  # Remove null conversion values
  data <- data[!is.na(data$Y), ]

  # Remove cities with missing time periods
  total_periods <- max(data$time)
  complete_cases <- table(data$location, data$time)
  complete_cases[complete_cases > 0] <- 1
  complete <- rowSums(complete_cases) == total_periods
  incomplete_locations <- length(complete) - length(complete[complete==TRUE])
  complete <- complete[complete==TRUE]
  data <- data %>% dplyr::filter(location %in% names(complete))


  # Aggregate Outcomes by time and location
  data_raw <- data

  if(keep_unix_time == FALSE){
    data <- data_raw %>% dplyr::group_by(location, time) %>% dplyr::summarize(Y = sum(Y))
    for (var in X){
      data_aux <- data_raw %>% dplyr::group_by(location, time) %>% dplyr::summarize(!!var := sum(!!sym(var)))
      data <- data %>% dplyr::left_join(data_aux, by = c("location", "time"))
    }
  } else {
    data <- data_raw %>% dplyr::group_by(location, time, date_unix) %>% dplyr::summarize(Y = sum(Y))
    for (var in X){
      data_aux <- data_raw %>% dplyr::group_by(location, time, date_unix) %>% dplyr::summarize(!!var := sum(!!sym(var)))
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
      "\n* Final Number of Locations (Complete): ", length(unique(data$location))  ) )
  }

  return(as.data.frame(data))

}


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
                    treatment_start = 0){

  if (treatment_start == 0){
    size_vline <- 0
  } else{
    size_vline <- 0.8
  }

  p <- ggplot(data,aes(y = !!sym(Y_id),x = !!sym(time_id), colour = !!sym(location_id), label = !!sym(location_id) )) +
    geom_line(show.legend = FALSE) +
    geom_vline(xintercept = treatment_start, linetype="dashed", size = size_vline, color = "grey35") +
    geom_dl(aes(label = !!sym(location_id)), method = list(dl.combine("last.points"), cex = 0.8)) +
    xlim(1,1.15*(max(data[[time_id]])))+
    ylab("") +
    theme_minimal()

  print(p)
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
                         forced_control_locations = c()){


  data <- data %>% dplyr::rename(time = time_id,
                                 Y = Y_id,
                                 location = location_id)

  if (max_controls > length(unique(data$location))){
    print("Error: There can't be more controls than total locations.")
    return(NULL)
  }

  # Calculate the Average Time-Series
  avg_Y <- data %>% dplyr::group_by(time) %>% dplyr::summarize(Y_mean = mean(Y))

  # Append it to the data
  data_aux <- data %>% dplyr::left_join(avg_Y, by = "time")

  # Compute the difference for each time/location
  data_aux$diff <- data_aux$Y - data_aux$Y_mean

  # Calculate the average difference
  data_aux <- data_aux %>% dplyr::group_by(location) %>% dplyr::summarize(mean_diff = mean(diff))

  # Calculate the percentiles for stratified sampling
  perc <- quantile(data_aux$mean_diff, probs = seq(0, 1, 0.2))
  perc <- unname(perc)

  data_aux$percentile <- 0

  for (location in 1:nrow(data_aux)){
    for(percentile in 1:(length(perc)-1)){
      if (data_aux$mean_diff[location] > perc[percentile] &
          data_aux$mean_diff[location] <= perc[percentile+1]){

        data_aux$percentile[location] <- percentile

      }
    }
  }

  data_locs <- data_aux %>%
    dplyr::filter(!(location %in% test_locations)) %>%
    dplyr::group_by(percentile) %>%
    dplyr::sample_n(round(max_controls/length(perc)), replace = TRUE) %>%
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

  df <- df %>% dplyr::filter(time <= treatment_end_time) #Remove periods after treatment

  return(df)

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
                       sim, #Iterator: Sim number
                       max_time, #test end
                       tp, #treatment periods
                       es, #effect size
                       locations, #test locations to try out
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

  if (normalize == TRUE){ # NEWCHANGE: Normalize by the sd
    factor <- sd(as.matrix(data$Y))
    data$Y <- data$Y/factor
  }

  data_aux <- fn_treatment(data, locations=locations,
                           treatment_start_time,
                           treatment_end_time)

  data_aux$Y_inc <- data_aux$Y
  data_aux$Y_inc[data_aux$D==1] <- data_aux$Y_inc[data_aux$D==1]*(1+es)


  if (length(X) == 0){
    PowerCalc <- augsynth::augsynth(Y_inc ~ D,
                                    unit = location,
                                    time = time,
                                    data = data_aux,
                                    t_int = treatment_start_time,
                                    progfunc = model,
                                    scm = T,
                                    fixedeff = fixed_effects)
  }
  else if (length(X) > 0){
    fmla <- as.formula(paste("Y_inc ~ D |",
                             sapply(list(X),
                                    paste, collapse = "+")))

    PowerCalc <- augsynth::augsynth(fmla,
                                    unit = location,
                                    time = time,
                                    data = data_aux,
                                    t_int = treatment_start_time,
                                    progfunc = "GSYN",
                                    scm = T,
                                    fixedeff = fixed_effects)
  }


  #NEWCHANGES: Option for quick imbalance-based calcs or p-value
  if (type == "pValue") {
    #pVal <- summary(PowerCalc)$average_att$p_val

    wide_data <- PowerCalc$data
    new_wide_data <- wide_data
    new_wide_data$X <- cbind(wide_data$X, wide_data$y)
    new_wide_data$y <- matrix(1, nrow = nrow(wide_data$X), ncol = 1)
    pVal <- augsynth:::compute_permute_pval(wide_data = new_wide_data,
                                            ascm = PowerCalc,
                                            h0 = 0,
                                            post_length = ncol(wide_data$y),
                                            type = "iid",
                                            q = 1,
                                            ns = 1000,
                                            stat_func = stat_func)
    ScaledL2Imbalance <-  PowerCalc$scaled_l2_imbalance
  }
  else if (type == "Imbalance") {
    pVal <- NA
    ScaledL2Imbalance <- PowerCalc$scaled_l2_imbalance
  }
  else {
    message(paste0("ERROR: Please input a valid type: pValue or Imbalance."))
    pVal <- NA
    ScaledL2Imbalance <- NA
  }

  investment <- cpic*sum(data_aux$Y[data_aux$D==1])*(es)

  return (
    c(paste(locations, collapse=", "),
      pVal,
      tp,
      es,
      treatment_start_time,
      investment,
      ScaledL2Imbalance)
  )
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
                          import_augsynth_from){
  message("Setting up cluster.")
  if (parallel_setup == "sequential"){
    cl <- parallel::makeCluster(parallel::detectCores() - 1, setup_strategy = parallel_setup)
  } else if (parallel_setup == "parallel"){
    cl <- parallel::makeCluster(parallel::detectCores() - 1, setup_strategy = parallel_setup, setup_timeout = 0.5)
  } else {
    stop('Please specify a valid set-up. Can be one of "sequential" or "parallel".')
  }

  doParallel::registerDoParallel(cl)

  message("Importing functions into cluster.")
  parallel::clusterCall(cl, function()
    eval(parse(text=import_augsynth_from)))

  parallel::clusterCall(cl, function()
    attachNamespace('dplyr'))
  parallel::clusterCall(cl, function()
    attachNamespace('tidyr'))

  parallel::clusterExport(
    cl,
    c('fn_treatment','pvalueCalc', 'MarketSelection'),
    envir=environment()
  )
  return(cl)
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
                         effect_size = seq(0,1,0.05),
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
                         import_augsynth_from = "library(augsynth)"){

  if (parallel == TRUE){
    cl <- build_cluster(
      parallel_setup = parallel_setup, import_augsynth_from = import_augsynth_from)
  }

  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)

  # Small Pre-treatment Periods
  if (max_time/max(treatment_periods) < 4){
    message(paste0("Caution: Small pre-treatment period!.
                   \nIt's recommended to have at least 4x pre-treatment periods for each treatment period.\n"))
  }

  results <- data.frame(matrix(ncol=8,nrow=0))
  colnames(results) <- c("location",
                         "pvalue",
                         "duration",
                         "lift",
                         "treatment_start",
                         "investment",
                         "cpic",
                         "ScaledL2Imbalance")

  if (horizon < 0){ #NEWCHANGE
    horizon = max(treatment_periods)
  }

  num_sim <- length(effect_size)*length(treatment_periods)

  if(ProgressBar == TRUE) {
    pb <- progress::progress_bar$new(format = "  Running Simulations [:bar] :percent",
                                     total = num_sim,
                                     clear = FALSE,
                                     width= 60)
  }

  for (es in effect_size){ #iterate through lift %

    stat_func <- type_of_test(side_of_test = side_of_test,
                              alternative_hypothesis = ifelse(es > 0, "Positive", "Negative"))

    for (tp in treatment_periods){ #lifts
      t_n <- max(data$time) - tp + 1 #Number of simulations without extrapolation

      if(ProgressBar == TRUE){
        pb$tick()
      }

      if (parallel == TRUE){
        a <- foreach(sim = 1:(t_n - horizon + 1), #NEWCHANGE: Horizon = earliest start time for simulations
                     .combine=cbind,
                     .errorhandling = 'stop') %dopar% {
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
                         stat_func = stat_func)

                     }

        for (i in 1:ncol(a)) {
          results <- rbind(results, data.frame(location = a[[1,i]],
                                               pvalue = as.numeric(a[[2,i]]),
                                               duration = as.numeric(a[[3,i]]),
                                               lift = as.numeric(a[[4,i]]),
                                               treatment_start = as.numeric(a[[5,i]]),
                                               investment = as.numeric(a[[6,i]]),
                                               cpic = cpic,
                                               ScaledL2Imbalance = as.numeric(a[[7,i]]) ) )
        }
      } else{
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
            stat_func = stat_func))

          results <- rbind(results, data.frame(location=aux[1],
                                               pvalue = as.numeric(aux[2]),
                                               duration = as.numeric(aux[3]),
                                               lift = as.numeric(aux[4]),
                                               treatment_start = as.numeric(aux[5]),
                                               investment = as.numeric(aux[6]),
                                               cpic = cpic,
                                               ScaledL2Imbalance = as.numeric(aux[7]) ) )
        }
      }
    }
  }

  if (parallel == TRUE){
    parallel::stopCluster(cl)
  }

  class(results) <- c("GeoLiftPower", class(results))

  results$pow <- 0
  results$pow[results$pvalue < alpha] <- 1

  return(results)
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

  if (!inherits(x, 'GeoLiftPower')) {
    stop('object must be class GeoLiftPower')
  }

  treatment_periods <- unique(x$duration)
  lift <- unique(x$lift)

  #NewChange: Standardize Plots
  PowerPlot <- x %>%
    dplyr::group_by(duration, lift) %>%
    #dplyr::mutate(power = 1 - pvalue) %>%
    #dplyr::summarise(power = mean(power))
    dplyr::summarise(power = mean(pow))

  spending <- x %>% dplyr::group_by(duration, lift) %>% dplyr::summarize(inv = mean(investment))

  if (table == TRUE){
    print(as.data.frame(PowerPlot))
  }


  if(actual_values == FALSE){

    if (sum(spending$inv > 0)) {

      for(dur in unique(PowerPlot$duration)){

        PowerPlot_aux <- as.data.frame(PowerPlot %>% dplyr::filter(duration == dur))

        CostPerLift <- as.numeric(x %>%
                                    dplyr::filter(duration == dur, lift > 0) %>%
                                    dplyr::mutate(AvgCost = investment / lift) %>%
                                    dplyr::summarise(mean(AvgCost)))

        PowerPlot_graph <- ggplot(PowerPlot_aux, aes(x = lift, y = power)) +
          geom_smooth(formula = y ~ x, color = "indianred3", method = "loess", se = FALSE)  +
          scale_x_continuous(sec.axis = sec_axis(~. *CostPerLift, name = "Estimated Investment"))  +
          ylim(0,1) +
          geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey") +
          labs(title = paste0("Treatment Periods: ", dur), x = "Effect Size", y = "Power") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))


        plot(PowerPlot_graph)
      }

    } else {

      for(dur in unique(PowerPlot$duration)){

        PowerPlot_aux <- as.data.frame(PowerPlot %>% dplyr::filter(duration == dur))

        PowerPlot_graph <- ggplot(PowerPlot_aux, aes(x = lift, y = power)) +
          geom_smooth(formula = y ~ x, color = "indianred3", method = "loess", se = FALSE)  +
          ylim(0,1) +
          geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey") +
          labs(title = paste0("Treatment Periods: ", dur), x = "Effect Size", y = "Power") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))


        plot(PowerPlot_graph)
      }

    }

  } else if(actual_values == TRUE){

    if (sum(spending$inv > 0)) {

      for(dur in unique(PowerPlot$duration)){

        PowerPlot_aux <- as.data.frame(PowerPlot %>% dplyr::filter(duration == dur))

        CostPerLift <- as.numeric(x %>%
                                    dplyr::filter(duration == dur, lift > 0) %>%
                                    dplyr::mutate(AvgCost = investment / lift) %>%
                                    dplyr::summarise(mean(AvgCost)))

        PowerPlot_graph <- ggplot(PowerPlot_aux, aes(x = lift, y = power)) +
          geom_smooth(formula = y ~ x, color = "indianred3", method = "loess", se = FALSE)  +
          geom_line(color="gray80", size = 0.62, alpha = 0.8) +
          scale_x_continuous(sec.axis = sec_axis(~. *CostPerLift, name = "Estimated Investment"))  +
          ylim(0,1) +
          geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey") +
          labs(title = paste0("Treatment Periods: ", dur), x = "Effect Size", y = "Power") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))


        plot(PowerPlot_graph)
      }

    } else {

      for(dur in unique(PowerPlot$duration)){

        PowerPlot_aux <- as.data.frame(PowerPlot %>% dplyr::filter(duration == dur))

        PowerPlot_graph <- ggplot(PowerPlot_aux, aes(x = lift, y = power)) +
          geom_smooth(formula = y ~ x, color = "indianred3", method = "loess", se = FALSE)  +
          geom_line(color="gray80", size = 0.62, alpha = 0.8) +
          ylim(0,1) +
          geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey") +
          labs(title = paste0("Treatment Periods: ", dur), x = "Effect Size", y = "Power") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))


        plot(PowerPlot_graph)
      }

    }

  }

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
                            import_augsynth_from = "library(augsynth)"){

  if (parallel == TRUE){
    cl <- build_cluster(
      parallel_setup = parallel_setup, import_augsynth_from = import_augsynth_from)
  }

  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)
  locs <- unique(as.character(data$location))

  # Small Pre-treatment Periods
  if (max_time/max(treatment_periods) < 4){
    message(paste0("Caution: Small pre-treatment period!.
                   \nIt's recommended to have at least 4x pre-treatment periods for each treatment period.\n"))
  }

  #times <- trunc(quantile(data$time, probs = c(0.5, 0.75, 1), names = FALSE ))

  results <- data.frame(matrix(ncol=5, nrow=0)) #NEWCHANGE: Add Imbalance
  colnames(results) <- c("location","pvalue", "n", "treatment_start", "ScaledL2Imbalance")

  if (length(number_locations) == 0) {
    number_locations <- unique(round(quantile(c(1:length(unique(data$location))),
                                              probs = seq(0,0.5,0.05),
                                              type = 1,
                                              names = FALSE)))
  }

  num_sim <- length(number_locations)

  if(ProgressBar == TRUE) {
    pb <- progress::progress_bar$new(format = "  Running Simulations [:bar] :percent",
                                     total = num_sim,
                                     clear = FALSE,
                                     width= 60)
  }

  for(n in number_locations){
    #for (t in times){

    if(ProgressBar == TRUE){
      pb$tick()
    }

    if (parallel == TRUE){
      a <- foreach(sim = 1:n_sim,
                   .combine=cbind,
                   .errorhandling = 'stop') %dopar% {
                     pvalueCalc(
                       data = data,
                       sim = 1,
                       max_time = max_time,#max_time,
                       tp = treatment_periods,
                       es = 0,
                       locations = as.list(sample(locs,n, replace = FALSE )),
                       cpic = 0,
                       X = c(),
                       type = type,
                       normalize = normalize,
                       fixed_effects = fixed_effects,
                       model = model,
                       stat_func = stat_func)

                   }

      for (i in 1:ncol(a)) {
        results <- rbind(results, data.frame(location = a[[1,i]],
                                             pvalue = as.numeric(a[[2,i]]),
                                             n = n,
                                             treatment_start = as.numeric(a[[5,i]]),
                                             ScaledL2Imbalance = as.numeric(a[[7,i]]) ) )
      }
    } else {
      for (sim in 1:n_sim){
        aux <- NULL
        aux <- suppressMessages(pvalueCalc(data = data,
                                           sim = 1,
                                           max_time = max_time,#max_time,
                                           tp = treatment_periods,
                                           es = 0,
                                           locations = as.list(sample(locs,n, replace = FALSE )),
                                           cpic = 0,
                                           X = c(),
                                           type = type,
                                           normalize = normalize,
                                           fixed_effects = fixed_effects,
                                           model = model,
                                           stat_func = stat_func))

        results <- rbind(results, data.frame(location = aux[1],
                                             pvalue = as.numeric(aux[2]),
                                             n = n,
                                             treatment_start = as.numeric(aux[5]),
                                             ScaledL2Imbalance = as.numeric(aux[7])))
      }
    }
  }

  if (parallel == TRUE){
    parallel::stopCluster(cl)
  }

  if(plot == TRUE){
    results$pow <- 0
    results$pow[results$pvalue > alpha] <- 1

    resultsM <- results %>% dplyr::group_by(n) %>%  dplyr::summarize(mean_pow = mean(pow), mean_L2ScaledImbalance = mean(ScaledL2Imbalance))
    resultsM <- tibble::add_row(resultsM, n = 0, mean_pow = 0, mean_L2ScaledImbalance = 1, .before = 1)

    #print("Average Power By Number of Locations")
    #print(resultsM)

    if(type == "pValue") {

      print("Average Power By Number of Locations")
      print(resultsM)

      powerplot <- ggplot(resultsM,aes(y = mean_pow,x = n)) +
        geom_line(color="indianred3", size = 0.95) +
        ylim(0,1) +
        ggtitle("Average Power By Number of Locations") +
        xlab("Number of Locations") +
        ylab("Average Power") +
        geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey") +
        theme_minimal()

      imbalanceplot <- ggplot(resultsM,aes(y = mean_L2ScaledImbalance,x = n)) +
        geom_line(color="steelblue4", size = 0.95) +
        ylim(0,1) +
        ggtitle("Average Scaled L2 Imbalance By Number of Locations") +
        xlab("Number of Locations") +
        ylab("Average Scaled L2 Imbalance") +
        theme_minimal()

      gridExtra::grid.arrange(powerplot,imbalanceplot, nrow = 1)

    } else if (type == "Imbalance") {

      print("Average Scaled L2 Imbalance By Number of Locations")
      print(resultsM[,-2])

      imbalanceplot <- ggplot(resultsM,aes(y = mean_L2ScaledImbalance,x = n)) +
        geom_line(color="steelblue4", size = 0.95) +
        ylim(0,1) +
        ggtitle("Average Scaled L2 Imbalance By Number of Locations") +
        xlab("Number of Locations") +
        ylab("Average Scaled L2 Imbalance") +
        theme_minimal()
      plot(imbalanceplot)
    }


  }

  return(results)

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
                            dtw = 0){

  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  data$location <- tolower(data$location)
  astime <- seq(as.Date("2000/1/1"), by = "day", length.out = max(data$time))
  data$astime <- astime[data$time]


  # Find the best matches based on DTW
  mm <- MarketMatching::best_matches( data = data,
                                      id_variable = "location",
                                      date_variable = "astime",
                                      matching_variable = "Y",
                                      parallel = TRUE,
                                      warping_limit = 1,
                                      dtw_emphasis = dtw,
                                      start_match_period = min(data$astime),
                                      end_match_period = max(data$astime),
                                      matches = length(unique(data$location)) - 1)

  # Create a matrix with each row being the raked best controls for each location
  best_controls <- mm$BestMatches %>% tidyr::pivot_wider(id_cols = location,
                                                         names_from = rank,
                                                         values_from = BestControl)

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
stochastic_market_selector <- function(
  treatment_size,
  similarity_matrix,
  run_stochastic_process = FALSE
){
  if (!run_stochastic_process){
    #message("Deterministic setup with ", treatment_size, " locations in treatment.")
    return(similarity_matrix[, 1:treatment_size])
  } else {
    message("Random setup with ", treatment_size, " locations in treatment.")
    if (treatment_size > 0.5*ncol(similarity_matrix)){
      stop(paste0(
        "Treatment size (",
        treatment_size,
        ") should be <= to half the amount of units: ",
        ncol(similarity_matrix)))
    }
    sample_size <- round(ncol(similarity_matrix) / treatment_size)
    sample_matrix <- c()
    i <- 0
    while ( (i / 2) < treatment_size){
      sampled_number <- sample(1:2, 1) + i
      sample_matrix <- cbind(sample_matrix, similarity_matrix[, sampled_number])
      i <- i + 2
    }
    for (row in 1:nrow(sample_matrix)){ #Sort rows.
      sample_matrix[row,] <- sort(sample_matrix[row,])
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
type_of_test <- function(side_of_test="two_sided", alternative_hypothesis=NULL){
  if (side_of_test == "two_sided"){
    stat_func <- function(x) sum(abs(x))
  } else if (side_of_test == "one_sided"){
    if (is.null(alternative_hypothesis)){
      stop("If running a one sided test, please define alternative_hypotehsis parameter.
  Either 'positive' or 'negative'")
    }
    if (tolower(alternative_hypothesis) == "negative"){
      stat_func <- function(x) -sum(x)
    } else if (tolower(alternative_hypothesis) == "positive"){
      stat_func <- function(x) sum(x)
    } else {
      stop("Please define a valid alternative_hypothesis. Can be either {'Negative', 'Positive'}.")
    }
  } else {
    stop("Please define a valid side_of_test. Can be either {'one_sided', 'two_sided'}.")
  }
  return(stat_func)
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
                                import_augsynth_from = "library(augsynth)"){

  if (parallel == TRUE){
    cl <- build_cluster(
      parallel_setup = parallel_setup, import_augsynth_from = import_augsynth_from)
  }

  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)

  # Small Pre-treatment Periods
  if (max_time/max(treatment_periods) < 4){
    message(paste0("Caution: Small pre-treatment period!.
                   \nIt's recommended to have at least 4x pre-treatment periods for each treatment period.\n"))
  }

  results <- data.frame(matrix(ncol=5,nrow=0))
  colnames(results) <- c("location",
                         "pvalue",
                         "duration",
                         "treatment_start",
                         "ScaledL2Imbalance")

  BestMarkets <- MarketSelection(data,
                                 location_id = "location",
                                 time_id = "time",
                                 Y_id = "Y",
                                 dtw = dtw)

  if (horizon < 0){ #NEWCHANGE
    horizon = max(treatment_periods)
  }

  # Aggregated Y Per Location
  AggYperLoc <- data %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(Total_Y = sum(Y))

  #NEWCHANGE: Progress Bar
  num_sim <- length(N) * length(treatment_periods) * nrow(BestMarkets)
  if(ProgressBar == TRUE) {
    pb <- progress::progress_bar$new(format = "  Running Simulations [:bar] :percent",
                                     total = num_sim,
                                     clear = FALSE,
                                     width= 60)
  }

  for (n in N){
    BestMarkets_aux <- stochastic_market_selector(
      n,
      BestMarkets,
      run_stochastic_process=run_stochastic_process)
    for (test in 1:nrow(as.matrix(BestMarkets_aux))){ #iterate through lift %
      for (tp in treatment_periods){ #lifts

        if(ProgressBar == TRUE){
          pb$tick()
        }

        t_n <- max(data$time) - tp + 1 #Number of simulations without extrapolation (latest start time possible for #tp)

        if (parallel == TRUE){
          a <- foreach(sim = 1:(t_n - horizon + 1), #NEWCHANGE: Horizon = earliest start time for simulations
                       .combine=cbind,
                       .errorhandling = 'stop') %dopar% {
                         pvalueCalc(
                           data = data,
                           sim = sim,
                           max_time = max_time,
                           tp = tp,
                           es = 0,
                           locations = as.list(as.matrix(BestMarkets_aux)[test,]),
                           cpic = 0,
                           X,
                           type = type,
                           normalize = normalize,
                           fixed_effects = fixed_effects,
                           model = model,
                           stat_func = stat_func)

                       }

          for (i in 1:ncol(a)) {
            results <- rbind(results, data.frame(location=a[[1,i]],
                                                 pvalue = as.numeric(a[[2,i]]),
                                                 duration = as.numeric(a[[3,i]]),
                                                 treatment_start = as.numeric(a[[5,i]]),
                                                 ScaledL2Imbalance = as.numeric(a[[7,i]]) ) )
          }
        } else{

          for (sim in 1:(t_n - horizon + 1)){
            aux <- NULL
            aux <- suppressMessages(pvalueCalc(data = data,
                                               sim = sim,
                                               max_time = max_time,
                                               tp = tp,
                                               es = 0,
                                               locations = as.list(as.matrix(BestMarkets_aux)[test,]),
                                               cpic = 0,
                                               X,
                                               type = type,
                                               normalize = normalize,
                                               fixed_effects = fixed_effects,
                                               model = model,
                                               stat_func = stat_func))

            results <- rbind(results, data.frame(location=aux[1],
                                                 pvalue = as.numeric(aux[2]),
                                                 duration = as.numeric(aux[3]),
                                                 treatment_start = as.numeric(aux[5]),
                                                 ScaledL2Imbalance = as.numeric(aux[7]) ) )

          }
        }
      }
    }
  }

  if (parallel == TRUE){
    parallel::stopCluster(cl)
  }

  # Sort Locations alphabetically
  results$location <- strsplit(stringr::str_replace_all(results$location, ", ", ","),split = ",")
  results$location <- lapply(results$location, sort)
  results$location <- lapply(results$location, function(x) paste(x, collapse = ", "))
  results$location <- unlist(results$location)

  if(type == "pValue") {

    results$pow <- 0
    results$pow[results$pvalue > alpha] <- 1

    resultsM <- results %>%
      dplyr::group_by(location) %>%
      dplyr::summarize(mean_pow = mean(pow), mean_scaled_l2_imbalance = mean(ScaledL2Imbalance)) %>%
      #dplyr::arrange(dplyr::desc(mean_pow)) %>%
      dplyr::distinct()

  } else if (type == "Imbalance") {

    resultsM <- results %>%
      dplyr::group_by(location) %>%
      dplyr::summarize(mean_scaled_l2_imbalance = mean(ScaledL2Imbalance)) %>%
      #dplyr::arrange(mean_scaled_l2_imbalance) %>%
      dplyr::distinct()

  }

  # Add Percent of Y in test markets
  resultsM$ProportionTotal_Y <- 1
  resultsM$Locs <- strsplit(stringr::str_replace_all(resultsM$location, ", ", ","),split = ",")

  for (row in 1:nrow(resultsM)) {
    resultsM$ProportionTotal_Y[row] <- as.numeric(AggYperLoc %>%
                                                    dplyr::filter (location %in% resultsM$Locs[[row]]) %>%
                                                    dplyr::summarize(total = sum(Total_Y))) /
      sum(AggYperLoc$Total_Y)
  }

  # Sort Before Ranking
  if(type == "pValue") {

    resultsM <- resultsM %>%
      dplyr::arrange(dplyr::desc(mean_pow),
                     mean_scaled_l2_imbalance,
                     dplyr::desc(ProportionTotal_Y))

  } else if (type == "Imbalance") {

    resultsM <- resultsM %>%
      dplyr::arrange(mean_scaled_l2_imbalance,
                     dplyr::desc(ProportionTotal_Y))

  }


  #Remove the Locs column
  resultsM <- dplyr::select (resultsM, -c(Locs))

  class(results) <- c("GeoLift.search", class(resultsM))

  resultsM$rank <- 1:nrow(resultsM)

  if (top_results > nrow(resultsM)){
    top_results = nrow(resultsM)
  }

  print(paste0("Best ", top_results, " test markets:"))
  print(head(resultsM[,1], top_results))


  return(as.data.frame(resultsM))

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
                               effect_size = seq(0,0.25,0.05),
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
                               import_augsynth_from = "library(augsynth)"){

  if (parallel == TRUE){
    cl <- build_cluster(
      parallel_setup = parallel_setup, import_augsynth_from = import_augsynth_from)
  }

  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)

  # Small Pre-treatment Periods
  if (max_time/max(treatment_periods) < 4){
    message(paste0("Caution: Small pre-treatment period!.
                   \nIt's recommended to have at least 4x pre-treatment periods for each treatment period.\n"))
  }

  results <- data.frame(matrix(ncol=6,nrow=0))
  colnames(results) <- c("location",
                         "pvalue",
                         "duration",
                         "lift",
                         "treatment_start",
                         "ScaledL2Imbalance")


  BestMarkets <- MarketSelection(data,
                                 location_id = "location",
                                 time_id = "time",
                                 Y_id = "Y",
                                 dtw = dtw)

  # Aggregated Y Per Location
  AggYperLoc <- data %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(Total_Y = sum(Y))

  #NEWCHANGE: Progress Bar
  num_sim <- length(N) * length(treatment_periods) * length(effect_size)
  if(ProgressBar == TRUE) {
    pb <- progress::progress_bar$new(format = "  Running Simulations [:bar] :percent",
                                     total = num_sim,
                                     clear = FALSE,
                                     width= 60)
  }



  for (n in N){
    BestMarkets_aux <- stochastic_market_selector(
      n,
      BestMarkets,
      run_stochastic_process = run_stochastic_process)
    for (es in effect_size){ #iterate through lift %

      stat_func <- type_of_test(side_of_test = side_of_test,
                                alternative_hypothesis = ifelse(es > 0, "positive", "negative"))

      for (tp in treatment_periods){ #lifts

        if(ProgressBar == TRUE){
          pb$tick()
        }

        if (parallel == TRUE){
          a <- foreach(test = 1:nrow(as.matrix(BestMarkets_aux)), #NEWCHANGE: Horizon = earliest start time for simulations
                       .combine=cbind,
                       .errorhandling = 'stop') %dopar% {
                         pvalueCalc(
                           data = data,
                           sim = 1,
                           max_time = max_time,
                           tp = tp,
                           es = es,
                           locations = as.list(as.matrix(BestMarkets_aux)[test,]),
                           cpic = 0,
                           X,
                           type = "pValue",
                           normalize = normalize,
                           fixed_effects = fixed_effects,
                           model = model,
                           stat_func = stat_func)

                       }

          for (i in 1:ncol(a)) {
            results <- rbind(results, data.frame(location = a[[1,i]],
                                                 pvalue = as.numeric(a[[2,i]]),
                                                 duration = as.numeric(a[[3,i]]),
                                                 lift = as.numeric(a[[4,i]]),
                                                 treatment_start = as.numeric(a[[5,i]]),
                                                 ScaledL2Imbalance = as.numeric(a[[7,i]]) ) )
          }
        } else{
          for (test in 1:nrow(as.matrix(BestMarkets_aux))) {
            aux <- NULL
            aux <- suppressMessages(pvalueCalc(
              data = data,
              sim = 1,
              max_time = max_time,
              tp = tp,
              es = es,
              locations = as.list(as.matrix(BestMarkets_aux)[test,]),
              cpic = 0,
              X,
              type = "pValue",
              normalize = normalize,
              fixed_effects = fixed_effects,
              model = model,
              stat_func = stat_func))

            results <- rbind(results, data.frame(location=aux[1],
                                                 pvalue = as.numeric(aux[2]),
                                                 duration = as.numeric(aux[3]),
                                                 lift = as.numeric(aux[4]),
                                                 treatment_start = as.numeric(aux[5]),
                                                 ScaledL2Imbalance = as.numeric(aux[7]) ) )
          }
        }

      }
    }
  }

  if (parallel == TRUE){
    parallel::stopCluster(cl)
  }

  # Sort Locations alphabetically
  results$location <- strsplit(stringr::str_replace_all(results$location, ", ", ","),split = ",")
  results$location <- lapply(results$location, sort)
  results$location <- lapply(results$location, function(x) paste(x, collapse = ", "))
  results$location <- unlist(results$location)

  results <- results %>%
    dplyr::mutate(significant = ifelse(pvalue < alpha, 1, 0)) %>%
    #dplyr::filter(significant > 0 & lift > 0) %>%
    dplyr::filter(significant > 0) %>%
    dplyr::distinct()

  resultsM <- NULL


  for (locs in unique(results$location)){
    for(ts in treatment_periods) {
      resultsFindAux <- results %>% dplyr::filter(location  == locs & duration == ts)

      if ( min(effect_size) < 0){
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

  # Add Percent of Y in test markets
  resultsM$ProportionTotal_Y <- 1
  resultsM$Locs <- strsplit(stringr::str_replace_all(resultsM$location, ", ", ","),split = ",")

  for (row in 1:nrow(resultsM)) {
    resultsM$ProportionTotal_Y[row] <- as.numeric(AggYperLoc %>%
                                                    dplyr::filter (location %in% resultsM$Locs[[row]]) %>%
                                                    dplyr::summarize(total = sum(Total_Y))) /
      sum(AggYperLoc$Total_Y)
  }

  # Remove any duplicates
  resultsM <- resultsM %>% dplyr::group_by(location, duration) %>% dplyr::slice_min(order_by = pvalue, n = 1)

  # Sort Before Ranking

  resultsM <- resultsM %>%
    dplyr::arrange(pvalue,
                   ScaledL2Imbalance,
                   lift,
                   dplyr::desc(ProportionTotal_Y))


  #Remove the Locs column
  resultsM <- dplyr::select (resultsM, -c(Locs, treatment_start, significant))

  class(results) <- c("GeoLift.search", class(resultsM))

  resultsM$rank <- 1:nrow(resultsM)

  if (top_results > nrow(resultsM)){
    top_results = nrow(resultsM)
  }

  if (plot_best == TRUE){
    for(tp in treatment_periods){
      BestResults <- resultsM %>% dplyr::filter(duration == tp) %>%
        dplyr::arrange(rank)

      bestmodels <- list()
      for (i in 1:4){

        locs_aux <- unlist(strsplit(stringr::str_replace_all(BestResults$location[i], ", ", ","),split = ","))

        data_lifted <- data

        data_lifted$Y[data_lifted$location %in% locs_aux &
                        data_lifted$time >= max_time - tp + 1] <-
          data_lifted$Y[data_lifted$location %in% locs_aux &
                          data_lifted$time >= max_time - tp + 1]*(1+BestResults$lift[i])

        bestmodels[[i]] <- GeoLift::GeoLift(Y_id = "Y",
                                            time_id = "time",
                                            location_id = "location",
                                            data = data_lifted,
                                            locations = locs_aux,
                                            treatment_start_time = max_time - tp + 1,
                                            treatment_end_time = max_time,
                                            model = model,
                                            fixed_effects = fixed_effects,
                                            print = FALSE)

      }
      gridExtra::grid.arrange(plot(bestmodels[[1]], notes = paste("locations:", BestResults$location[1],
                                                                  "\n Treatment Periods:", tp, "\n Minimum Detectable Effect: ",
                                                                  BestResults$lift[1],
                                                                  "\n Proportion Total Y: ", 100*round(BestResults$ProportionTotal_Y[1],3),"%")),
                              plot(bestmodels[[2]], notes = paste("locations:", BestResults$location[2],
                                                                  "\n Treatment Periods:", tp, "\n Minimum Detectable Effect: ",
                                                                  BestResults$lift[2],
                                                                  "\n Proportion Total Y: ", 100*round(BestResults$ProportionTotal_Y[2],3),"%")),
                              plot(bestmodels[[3]], notes = paste("locations:", BestResults$location[3],
                                                                  "\n Treatment Periods:", tp, "\n Minimum Detectable Effect: ",
                                                                  BestResults$lift[3],
                                                                  "\n Proportion Total Y: ", 100*round(BestResults$ProportionTotal_Y[3],3),"%")),
                              plot(bestmodels[[4]], notes = paste("locations:", BestResults$location[4],
                                                                  "\n Treatment Periods:", tp, "\n Minimum Detectable Effect: ",
                                                                  BestResults$lift[4],
                                                                  "\n Proportion Total Y: ", 100*round(BestResults$ProportionTotal_Y[4],3),"%")),
                              ncol = 2)
    }
  }

  #NEWCHANGE: Rename Lift to MDE
  resultsM <- resultsM %>% dplyr::rename(MinDetectableEffect = lift)

  return(as.data.frame(resultsM))

}

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
                                   effect_size = seq(0,0.25,0.05),
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
                                   import_augsynth_from = "library(augsynth)"
){

  if (parallel == TRUE){
    cl <- build_cluster(
      parallel_setup = parallel_setup, import_augsynth_from = import_augsynth_from)
  }

  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)
  include_markets <- tolower(include_markets)
  exclude_markets <- tolower(exclude_markets)

  # Data Checks

  # Small Pre-treatment Periods
  if (max_time/max(treatment_periods) < 4){
    message(paste0("Caution: Small pre-treatment period!.
                   \nIt's recommended to have at least 4x pre-treatment periods for each treatment period.\n"))
  }

  # Populate N if it's not provided
  if (length(N) == 0) {
    N <- unique(round(quantile(c(1:length(unique(data$location))),
                               probs = seq(0,0.5,0.1),
                               type = 1,
                               names = FALSE)))

    if (length(include_markets) > 0){
      #Keep only those equal or larger than included markets
      N <- append(length(include_markets), N[length(include_markets) <= N])
    }
  }

  # More include_markets than N
  if(length(include_markets) > 0 & min(N) < length(include_markets)){
    message(paste0("Error: More forced markets than total test ones.",
                   " Consider increasing the values of N."))
    return(NULL)
  }

  #Check that the provided markets exist in the data.
  if(!all(tolower(include_markets) %in% tolower(unique(data$location)))){
    message(paste0("Error: One or more markets in include_markets were not",
                   " found in the data. Check the provided list and try again."))
    return(NULL)
  }

  #Check that the provided markets exist in the data.
  if(!all(tolower(exclude_markets) %in% tolower(unique(data$location)))){
    message(paste0("Error: One or more markets in exclude_markets were not",
                   " found in the data. Check the provided list and try again."))
    return(NULL)
  }

  #Make sure all simulated effect sizes have the same sign.
  if(min(effect_size < 0 & max(effect_size) > 0)){
    message(paste0("Error: The specified simulated effect sizes are not all of the same ",
                   " sign. \nTry again with a vector of all positive or negative effects",
                   " sizes that includes zero."))
    return(NULL)
  }

  #  Check the holdout parameter
  if(length(holdout) > 1){
    if(length(holdout) > 2){
      message("Error: Too many arguments in holdout. Provide the min and max holdout sizes.")
      return(NULL)
    } else if(holdout[1] >= holdout[2]){
      message("Error: The first argument in holdout should be strictly smaller than the second.")
      return(NULL)
    } else if(min(holdout) < 0 | max(holdout > 1)){
      message("Error: Please specify valid values for holdouts (values from 0 to 1)")
      return(NULL)
    }
  } else if(length(holdout) == 1){
    message("Error: Too few arguments in holdout. Provide the min and max holdout sizes.")
    return(NULL)
  }

  results <- data.frame(matrix(ncol=8,nrow=0))
  colnames(results) <- c("location",
                         "pvalue",
                         "duration",
                         "lift",
                         "treatment_start",
                         "investment",
                         "cpic",
                         "ScaledL2Imbalance")

  # Setting the lookback window to the smallest length of treatment if not provided.
  if (lookback_window < 0){
    lookback_window <- min(treatment_periods)
  }

  #Exclude markets input by user by filter them out from the uploaded file data
  if(length(exclude_markets) > 0){
    data <- data[!(data$location %in% exclude_markets),]
  }

  BestMarkets <- MarketSelection(data,
                                 location_id = "location",
                                 time_id = "time",
                                 Y_id = "Y",
                                 dtw = dtw)

  # Aggregated Y Per Location
  AggYperLoc <- data %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(Total_Y = sum(Y))

  #NEWCHANGE: Progress Bar
  num_sim <- length(N) * length(treatment_periods) * length(effect_size)
  if(ProgressBar == TRUE) {
    pb <- progress::progress_bar$new(format = "  Running Simulations [:bar] :percent",
                                     total = num_sim,
                                     clear = FALSE,
                                     width= 60)
  }


  for (n in N){
    BestMarkets_aux <- stochastic_market_selector(
      n,
      BestMarkets,
      run_stochastic_process = run_stochastic_process)

    # Force included markets into the selection
    if(length(include_markets) > 0){
      temp_Markets <- NULL
      for (row in 1:nrow(BestMarkets_aux)){
        if(all(include_markets %in% BestMarkets_aux[row,])){
          temp_Markets <- rbind(temp_Markets, BestMarkets_aux[row,])
        }
      }
      BestMarkets_aux <- temp_Markets
    }

    #Skip iteration if no Markets are feasible
    if(is.null(BestMarkets_aux)){
      next
    }

    for (es in effect_size){ #iterate through lift %

      stat_func <- type_of_test(side_of_test = side_of_test,
                                alternative_hypothesis = ifelse(es > 0, "positive", "negative"))

      for (tp in treatment_periods){ #lifts

        if(ProgressBar == TRUE){
          pb$tick()
        }

        t_n <- max(data$time) - tp + 1 #Number of simulations without extrapolation (latest start time possible for #tp)

        for(sim in 1:(lookback_window)){

          if (parallel == TRUE){
            a <- foreach(test = 1:nrow(as.matrix(BestMarkets_aux)),
                         .combine=cbind,
                         .errorhandling = 'stop',
                         .verbose = FALSE) %dopar% {
                           suppressMessages(pvalueCalc(
                             data = data,
                             sim = sim,
                             max_time = max_time,
                             tp = tp,
                             es = es,
                             locations = as.list(as.matrix(BestMarkets_aux)[test,]),
                             cpic = cpic,
                             X,
                             type = "pValue",
                             normalize = normalize,
                             fixed_effects = fixed_effects,
                             model = model,
                             stat_func = stat_func))

                         }


            if(!is.null(dim(a))){
              for (i in 1:ncol(a)) {
                results <- rbind(results, data.frame(location = a[[1,i]],
                                                     pvalue = as.numeric(a[[2,i]]),
                                                     duration = as.numeric(a[[3,i]]),
                                                     lift = as.numeric(a[[4,i]]),
                                                     treatment_start = as.numeric(a[[5,i]]),
                                                     investment = as.numeric(a[[6,i]]),
                                                     cpic = cpic,
                                                     ScaledL2Imbalance = as.numeric(a[[7,i]]) ) )
              }
            } else if (length(a) >0){
              results <- rbind(results, data.frame(location = a[1],
                                                   pvalue = as.numeric(a[2]),
                                                   duration = as.numeric(a[3]),
                                                   lift = as.numeric(a[4]),
                                                   treatment_start = as.numeric(a[5]),
                                                   investment = as.numeric(a[6]),
                                                   cpic = cpic,
                                                   ScaledL2Imbalance = as.numeric(a[7]) ) )
            }


          } else{
            for (test in 1:nrow(as.matrix(BestMarkets_aux))) {
              aux <- NULL
              aux <- suppressMessages(pvalueCalc(
                data = data,
                sim = sim,
                max_time = max_time,
                tp = tp,
                es = es,
                locations = as.list(as.matrix(BestMarkets_aux)[test,]),
                cpic = cpic,
                X,
                type = "pValue",
                normalize = normalize,
                fixed_effects = fixed_effects,
                model = model,
                stat_func = stat_func))


              results <- rbind(results, data.frame(location=aux[1],
                                                   pvalue = as.numeric(aux[2]),
                                                   duration = as.numeric(aux[3]),
                                                   lift = as.numeric(aux[4]),
                                                   treatment_start = as.numeric(aux[5]),
                                                   investment = as.numeric(aux[6]),
                                                   cpic = cpic,
                                                   ScaledL2Imbalance = as.numeric(aux[7]) ) )
            }
          }
        }
      } #tp
    }
  }

  if (parallel == TRUE){
    parallel::stopCluster(cl)
  }

  # Step 1 - Sort
  results$location <- strsplit(stringr::str_replace_all(results$location, ", ", ","),split = ",")
  results$location <- lapply(results$location, sort)
  results$location <- lapply(results$location, function(x) paste(x, collapse = ", "))
  results$location <- unlist(results$location)

  #Step 2 - Compute Significant
  results <- results %>%
    dplyr::mutate(pow = ifelse(pvalue < 0.1, 1, 0)) %>%
    #   dplyr::filter(significant > 0) %>%
    dplyr::distinct()

  #Step 3 - Compute average Metrics
  results <- results %>%
    dplyr::group_by(location, duration, lift) %>%
    dplyr::summarise(power = mean(pow),
                     AvgScaledL2Imbalance = mean(ScaledL2Imbalance),
                     Investment = mean(investment), .groups = "keep")

  # Step 4 - Find the MDE that achieved power
  resultsM <- NULL

  for (locs in unique(results$location)){
    for(ts in treatment_periods) { #for(ts in treatment_periods)
      resultsFindAux <- results %>% dplyr::filter(location  == locs & duration == ts & power > 0.8)

      if ( min(effect_size) < 0){ #if ( min(effect_size) < 0){
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
  AggYperLoc <- data %>% #data %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(Total_Y = sum(Y))

  # Step 5.2 - Attach to Table
  resultsM$ProportionTotal_Y <- 1
  resultsM$Locs <- strsplit(stringr::str_replace_all(resultsM$location, ", ", ","),split = ",")

  for (row in 1:nrow(resultsM)) {
    resultsM$ProportionTotal_Y[row] <- as.numeric(AggYperLoc %>%
                                                    dplyr::filter (location %in% resultsM$Locs[[row]]) %>%
                                                    dplyr::summarize(total = sum(Total_Y))) /
      sum(AggYperLoc$Total_Y)
  }

  # Step 6 - Remove any duplicates
  resultsM <- resultsM %>% dplyr::group_by(location, duration) %>% dplyr::slice_max(order_by = power, n = 1)

  # Step 7 - Sort Before Ranking
  resultsM <- resultsM %>%
    dplyr::arrange(dplyr::desc(power),
                   AvgScaledL2Imbalance,
                   lift,
                   dplyr::desc(ProportionTotal_Y))

  # Step 8 - Remove the Locs column
  resultsM <- dplyr::select (resultsM, -c(Locs))

  # Step 9 - Rename columns
  resultsM <- dplyr::rename(resultsM,
                            Average_MDE = lift,
                            Power = power)

  # Step 10: Adjust signs if Negative Lift
  if (min(effect_size) < 0){
    resultsM$Investment <- -1*resultsM$Investment
    results$Investment <- -1*results$Investment
  }

  # Step 11 - Remove tests out of budget (if aplicable)
  if (!is.null(budget)){
    resultsM <- resultsM %>% dplyr::filter(abs(budget) > abs(Investment))
  }

  # Step 12: Holdout Size
  if (min(effect_size) < 0){
    resultsM$Holdout <- resultsM$ProportionTotal_Y
  } else {
    resultsM$Holdout <- 1 - resultsM$ProportionTotal_Y
  }

  # Step 13: Test Size
  if (length(holdout) > 0){
    resultsM <- resultsM %>% dplyr::filter(holdout[1] <= Holdout,
                                           holdout[2] >= Holdout)
  }

  # Step 14 - Create Ranking
  # Make sure there are viable options
  if(nrow(resultsM) > 0){
    resultsM$rank <- 1:nrow(resultsM)
  } else{
    message("\nWarning: No markets meet the criteria you provided. Consider modifying
            the input parameters")
  }


  class(results) <- c("GeoLift.MarketSelection", class(resultsM))

  return(list(BestMarkets = as.data.frame(resultsM), PowerCurves = as.data.frame(results)))

}


#' GeoLift inference calculation.
#'
#' @description
#'
#' \code{GeoLift} performs inference for a geo-test.
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
#' @param ConfidenceIntervals A logic flag indicating whether to estimate confidence intervals
#' through jackknifing techniques.  Set to FALSE by default.
#' @param stat_test A string indicating the test statistic.
#' \itemize{
#'          \item{"Total":}{ The test statistic is the sum of all treatment effects, i.e. sum(abs(x)). Default.}
#'          \item{"Negative":}{ One-sided test against positive effects i.e. -sum(x).
#'          Recommended for Negative Lift tests.}
#'          \item{"Positive":}{ One-sided test against negative effects i.e. sum(x).
#'          Recommended for Positive Lift tests.}
#' }
#' @param print A logic flag indicating whether to print the results or not.
#' Set to TRUE by default.
#'
#' @return
#' GeoLift object that contains:
#'          \itemize{
#'          \item{"results":}{ Generalized Augmented Sunthetic Controls results.}
#'          \item{"inference":}{ Data frame with inference statistics (ATT, Lift, p-value, and Confidence Interval.)}
#'          \item{"data":}{ Input data.}
#'          \item{"y_obs":}{ Observed outcome metric.}
#'          \item{"y_hat":}{ Counterfactual outcome metric.}
#'          \item{"ATT":}{ ATT estimate.}
#'          \item{"ATT_se":}{ Standrd Error of the ATT estimate.}
#'          \item{"TreatmentStart":}{ Time id of treatment start.}
#'          \item{"TreatmentEnd":}{ Time id of treatment end.}
#'          \item{"test_id":}{ List of names of the test locations.}
#'          \item{"incremental":}{ Incremental outcome units (Obersved - Counterfactual).}
#'          \item{"Y_id":}{ Name of the outcome variable.}
#'          \item{"summary":}{ Model's Summary.}
#'          \item{"ConfidenceIntervals":}{ Flag indicating whether CI will be included.}
#'          \item{"lower_bound":}{ Lower confidence interval.}
#'          \item{"upper_bound":}{ Upper confidence interval.}
#'          }
#'
#' @export
GeoLift <- function(Y_id = "Y",
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
                    ConfidenceIntervals = FALSE,
                    stat_test = "Total",
                    print = TRUE){

  # Rename variables to standard names used by GeoLift
  data <- data %>% dplyr::rename(time = time_id,
                                 Y = Y_id,
                                 location = location_id)

  data$location <- tolower(data$location)

  data_aux <- fn_treatment(data, locations = locations,
                           treatment_start_time,
                           treatment_end_time)


  if (length(X) == 0){
    fmla <- as.formula("Y ~ D")
  }
  else if (length(X) > 0){
    fmla <- as.formula(paste("Y ~ D |",
                             sapply(list(X),
                                    paste, collapse = "+")))

  }


  # Optimizing model based on Scaled L2 Score
  if (model == "best" & length(locations) > 1){
    scaled_l2_none <- tryCatch(expr = {augsynth::augsynth(fmla, unit = location, time = time,
                                                          data = data_aux,
                                                          t_int = treatment_start_time,
                                                          progfunc = "none",
                                                          scm = T,
                                                          fixedeff = fixed_effects)$scaled_l2_imbalance},
                               error = function(e){
                                 1
                               })
    scaled_l2_ridge <- tryCatch(expr = {augsynth::augsynth(fmla, unit = location, time = time,
                                                           data = data_aux,
                                                           t_int = treatment_start_time,
                                                           progfunc = "ridge",
                                                           scm = T,
                                                           fixedeff = fixed_effects)$scaled_l2_imbalance},
                                error = function(e){
                                  1
                                })
    scaled_l2_gsyn <- tryCatch(expr = {augsynth::augsynth(fmla, unit = location, time = time,
                                                          data = data_aux,
                                                          t_int = treatment_start_time,
                                                          progfunc = "GSYN",
                                                          scm = T,
                                                          fixedeff = fixed_effects)$scaled_l2_imbalance},
                               error = function(e){
                                 1
                               })

    if (round(scaled_l2_none, 3) > round(scaled_l2_gsyn, 3) & round(scaled_l2_ridge, 3) > round(scaled_l2_gsyn, 3)){
      model <- "GSYN"
    } else if(round(scaled_l2_none, 3) > round(scaled_l2_ridge, 3) & round(scaled_l2_gsyn, 3) > round(scaled_l2_ridge, 3)){
      model <- "ridge"
    } else {
      model <- "None"
    }

  } else if (model == "best" & length(locations) == 1) {
    scaled_l2_none <- augsynth::augsynth(fmla, unit = location, time = time,
                                         data = data_aux,
                                         t_int = treatment_start_time,
                                         progfunc = "none",
                                         scm = T,
                                         fixedeff = fixed_effects)$scaled_l2_imbalance
    scaled_l2_ridge <- augsynth::augsynth(fmla, unit = location, time = time,
                                          data = data_aux,
                                          t_int = treatment_start_time,
                                          progfunc = "ridge",
                                          scm = T,
                                          fixedeff = fixed_effects)$scaled_l2_imbalance
    if (round(scaled_l2_none, 3) > round(scaled_l2_ridge, 3)){
      model <- "ridge"
    } else {
      model <- "none"
    }
  }

  #Single Augsynth
  augsyn <- augsynth::augsynth(fmla, unit = location, time = time,
                               data = data_aux,
                               t_int = treatment_start_time,
                               progfunc = model,
                               scm = T,
                               fixedeff = fixed_effects)

  inference_df <- data.frame(matrix(ncol=5, nrow=0))
  colnames(inference_df) <- c("ATT",
                              "Perc.Lift",
                              "pvalue",
                              "Lower.Conf.Int",
                              "Upper.Conf.Int")

  #NEWCHANGE: To avoid running the time-consuming summary process, create and store the object for re-use
  if (tolower(stat_test) == "total"){
    side_of_test <- "two_sided"
    alternative_hypothesis <- NULL
  } else if (tolower(stat_test) == "negative" | tolower(stat_test) == "positive"){
    side_of_test <- "one_sided"
    alternative_hypothesis <- stat_test
  } else {
    stop("stat_test must be one of {'total', 'negative', 'positive'}.")
  }
  stat_func <- type_of_test(side_of_test = side_of_test,
                            alternative_hypothesis = alternative_hypothesis)

  sum_augsyn <- summary(augsyn, alpha = alpha, stat_func = stat_func)

  if(paste(augsyn$call)[1] == "single_augsynth"){
    mean <- sum_augsyn[['average_att']][['Estimate']] #Use summary object
    se <- sum_augsyn[['average_att']][['Std.Error']] #Use summary object

    loc_id <- c(which(augsyn$data$trt == 1))
    locs_id <- as.data.frame(loc_id, nrow = length(loc_id))
    #locs_id$name <- unlist(locations) #OLD
    locs_id$name <- unlist(unique(data_aux$location)[c(which(augsyn$data$trt == 1))]) #NEWCHANGE: Make sure we keep order

    y_obs <- c(augsyn$data$X[loc_id,], augsyn$data$y[loc_id,])
    y_hat <- predict(augsyn, att = FALSE)
    ATT <- predict(augsyn, att = TRUE)
    ATT_se <- sum_augsyn$att$Std.Error #Use summary object

    pred_conversions <- predict(augsyn)[treatment_start_time:treatment_end_time] #NEWCHANGE: Store object to avoid re-processing

    if (length(locations) == 1){
      lift <- (sum(augsyn$data$y[loc_id,]) - sum(pred_conversions)) /
        abs(sum(pred_conversions))
    } else if (length(locations) > 1){
      lift <- (sum(colMeans(augsyn$data$y[loc_id,]))-
                 sum(pred_conversions)) /
        abs(sum(pred_conversions))  #NEWCHANGE: Avoid errors with signs
    }

    incremental <- sum(augsyn$data$y[loc_id,]) - (sum(pred_conversions) * length(loc_id))

    inference_df <- inference_df %>% tibble::add_row(ATT = mean,
                                                     Perc.Lift = 100 * round(lift, 3),
                                                     pvalue = sum_augsyn$average_att$p_val, #pvalue(augsyn),
                                                     Lower.Conf.Int = sum_augsyn$average_att$lower_bound,
                                                     Upper.Conf.Int = sum_augsyn$average_att$upper_bound)
  }


  if(inference_df$pvalue < 0.05){
    significant <- "The results are significant at a 95% level."
  } else if(inference_df$pvalue < 0.10){
    significant <- "The results are significant at a 90% level."
  } else if(inference_df$pvalue < 0.20){
    significant <- "The results are significant at a 80% level."
  } else {
    significant <- "The results are not statistically significant."
  }

  res <- list("results" = augsyn,
              "inference" = inference_df,
              "data" = data_aux,
              "y_obs" = y_obs,
              "y_hat" = y_hat,
              "ATT" = ATT,
              "ATT_se" = ATT_se,
              "TreatmentStart" = treatment_start_time,
              "TreatmentEnd" = treatment_end_time,
              "test_id" = locs_id,
              "incremental" = incremental,
              "Y_id" = Y_id,
              "summary" = sum_augsyn,
              "ConfidenceIntervals" = ConfidenceIntervals,
              "lower_bound" = summary(augsyn, alpha = alpha, inf_type = "jackknife+")$average_att$lower_bound,
              "upper_bound" = summary(augsyn, alpha = alpha, inf_type = "jackknife+")$average_att$upper_bound)

  res$results$weights <- data.frame(
    location = dimnames(res$results$weights)[[1]],
    weight = unname(res$results$weights[,1])
  )

  if (print == TRUE){
    message(paste0(
      paste0("\nGeoLift Output\n\n"),
      paste0("Test results for ", (treatment_end_time - treatment_start_time + 1),
             " treatment periods, from time-stamp ",
             treatment_start_time, " to ", treatment_end_time,
             " for test markets:")))
    #paste(toupper(locations), collapse = ", ")
    for (i in 1:length(locations)){
      message(paste(i, toupper(locations[i])))
    }
    message(paste0(
      "##################################",
      "\n#####     Test Statistics    #####\n",
      "##################################\n",
      "\nPercent Lift: ",
      100 * round(lift, 3), "%\n\n",
      "Incremental ", paste(Y_id), ": ", round(incremental,0), "\n\n",
      "Average Estimated Treatment Effect (ATT): ", round(mean, 3),
      "\n\n",significant, " (", toupper(stat_test), ")",
      "\n\nThere is a ", round(100 * inference_df$pvalue, 2),
      "% chance of observing an effect this large or larger assuming treatment effect is zero.",
      sep="")
    )
  }


  class(res) <- c("GeoLift", class(res))
  return(res)

}


#' Plot for GeoLift.
#'
#' @description
#'
#' Plot for GeoLift objects.
#'
#' @param x GeoLift object.
#' @param type Type of plot. By default "Lift" which plots the
#' incrementality on the outcome variable. If type is set to "ATT",
#' the average ATT is plotted. If type is set to "Incrementality",
#' daily incremental values are plotted.
#' @param treatment_end_date Character that represents a date in year-month=day format.
#' @param frequency Character that represents periodicity of time stamps. Can be either
#' weekly or daily. Defaults to daily.
#' @param title String for the title of the plot. Empty by default.
#' @param plot_start_date Character that represents initial date of plot in year-month-day format.
#' @param subtitle String for the subtitle of the plot. Empty by default.
#' @param notes String to add notes to the plot. Empty by default.
#' @param ... additional arguments
#'
#' @return
#' GeoLift plot.
#'
#' @export
plot.GeoLift <- function(x,
                         type="Lift",
                         treatment_end_date = NULL,
                         frequency = "daily",
                         plot_start_date = NULL,
                         title = "",
                         subtitle = "",
                         notes = "",
                         ...) {


  if (!inherits(x, 'GeoLift')) {
    stop('object must be class GeoLift')
  }

  if (type == "TreatmentSchedule"){
    panelView(Y ~ D, data = x$data, index = c("location", "time"), pre.post = TRUE)

  } else if (tolower(type) %in% c("att", "incrementality")){
    absolute_value.plot(GeoLift = x,
                        treatment_end_date = treatment_end_date,
                        frequency = frequency,
                        plot_start_date = plot_start_date,
                        plot_type = type,
                        title = title,
                        subtitle = subtitle,
                        notes = notes,
                        ...)

  } else if (tolower(type) == "lift"){
    Lift.plot(GeoLift = x,
              treatment_end_date = treatment_end_date,
              frequency = frequency,
              plot_start_date = plot_start_date,
              title = title,
              subtitle = subtitle,
              notes = notes,
              ...)
  } else {
    message("Error: Please select a correct plot type: TreatmentSchedule/Lift/ATT/Incrementality")
  }

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
get_date_from_test_periods <- function(GeoLift, treatment_end_date, frequency="daily"){
  treatment_end_period <- GeoLift$TreatmentEnd
  treatment_start_period <- GeoLift$TreatmentStart
  if (tolower(frequency) == "daily"){
    date_vector <- seq(
      as.Date(treatment_end_date) - treatment_end_period + 1,
      as.Date(treatment_end_date),
      by="day")
  } else if (tolower(frequency) == "weekly"){
    date_vector <- seq(
      as.Date(treatment_end_date) - treatment_end_period * 7 + 7,
      as.Date(treatment_end_date),
      by="week")
  } else {
    stop("If converting time periods to dates, specify frequency param. Can be 'daily' or 'weekly'.")
  }

  treatment_start_date <- date_vector[treatment_start_period]

  return(list(
    date_vector = as.Date(date_vector),
    treatment_start = as.Date(treatment_start_date),
    treatment_end = as.Date(treatment_end_date))
  )
}


#' Aggregate Lift plot function for GeoLift.
#'
#' @description
#'
#' Aggregate Lift plot function for GeoLift.
#'
#' @param GeoLift GeoLift object.
#' @param treatment_end_date Character that represents a date in year-month=day format.
#' @param frequency Character that represents periodicity of time stamps. Can be either
#' weekly or daily. Defaults to daily.
#' @param plot_start_date Character that represents initial date of plot in year-month-day format.
#' @param title String for the title of the plot. Empty by default.
#' @param subtitle String for the subtitle of the plot. Empty by default.
#' @param notes String to add notes to the plot. Empty by default.
#' @param ... additional arguments
#'
#' @return
#' Aggregate Lift plot.
#'
#' @export
Lift.plot <- function(GeoLift,
                      treatment_end_date = NULL,
                      frequency = "daily",
                      plot_start_date = NULL,
                      title = "",
                      subtitle = "",
                      notes = "",
                      ...) {
  c_obs <- c_obs_lower_bound <- c_obs_upper_bound <- t_obs <- NULL
  treatment_obs <- as.data.frame(
    colMeans(
      matrix(
        GeoLift$y_obs,
        nrow = nrow(GeoLift$test_id),
        ncol = GeoLift$TreatmentEnd
      )
    )
  ) * nrow(GeoLift$test_id)
  colnames(treatment_obs) <- c("t_obs")

  q_treatment_locations <- length(GeoLift$test_id$name)
  df <- data.frame(
    t_obs = treatment_obs$t_obs,
    c_obs = GeoLift$y_hat * q_treatment_locations,
    c_obs_lower_bound = treatment_obs$t_obs - GeoLift$summary$att$upper_bound * q_treatment_locations,
    c_obs_upper_bound = treatment_obs$t_obs - GeoLift$summary$att$lower_bound * q_treatment_locations,
    Time = 1:length(treatment_obs$t_obs)
  )

  if (!is.null(treatment_end_date)){
    plot_dates <- get_date_from_test_periods(GeoLift, treatment_end_date, frequency=frequency)
    df$Time <- plot_dates$date_vector
  } else {
    message(
      "You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter.")
    plot_dates <- list(
      treatment_start = GeoLift$TreatmentStart,
      treatment_end = GeoLift$TreatmentEnd
    )
  }

  if (!is.null(plot_start_date)){
    if (is.null(treatment_end_date)){
      stop("If you want to filter your dataset on a date, please specify treatment_end_date param so periods are converted to dates.")
    } else {
      df <- df[df$Time >= plot_start_date,]
    }
  }

  if (nchar(title) == 0){
    title <- "Observations per Timestamp and Test Group"
  }

  colors <- c("Treatment" = "#52854C", "Control" = "#7030A0")

  ggplot(df, aes(x=Time)) +
    geom_line(
      aes(y=c_obs, color="Control"),
      linetype="dashed", alpha=1.5) +
    geom_ribbon(aes(ymin=c_obs_lower_bound, ymax=c_obs_upper_bound), alpha=0.2, fill="#4B4196") +
    geom_line(
      aes(y=t_obs, color="Treatment")) +
    theme_minimal() +
    labs(y = "Actual values",
         x = "Periods",
         title = title,
         subtitle = subtitle,
         caption = notes,
         color="Test group") +
    theme(text = element_text(size=20),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    geom_vline(xintercept=plot_dates$treatment_start, linetype="dashed", alpha=0.3) +
    scale_color_manual(values = colors)
}


#' Daily Incrementality or ATT plot function for GeoLift output.
#'
#' @description
#'
#' \code{absolute_value.plot} returns chart for daily absolute values using GeoLift output.
#'
#' @param GeoLift GeoLift object.
#' @param plot_type Can be either ATT or Incrementality.  Defaults to ATT.
#' @param treatment_end_date Character that represents a date in year-month-day format.
#' @param frequency Character that represents periodicity of time stamps. Can be either
#' weekly or daily. Defaults to daily.
#' @param plot_start_date Character that represents initial date of plot in year-month-day format.
#' @param title Character for the title of the plot. NULL by default.
#' @param subtitle Character for the subtitle of the plot. NULL by default.
#' @param notes String to add notes to the plot. Empty by default.
#' @param ... additional arguments
#'
#' @return
#' Daily Incremental or ATT plot.
#'
#' @export
absolute_value.plot <- function(
  GeoLift,
  plot_type = "ATT",
  treatment_end_date = NULL,
  frequency = "daily",
  plot_start_date = NULL,
  title = "",
  subtitle = "",
  notes = "",
  ...){
  Estimate <- lower_bound <- upper_bound <- NULL
  df <- GeoLift$summary$att
  df <- df[, c("Time", "Estimate", "lower_bound", "upper_bound")]

  if (tolower(plot_type) == "incrementality"){
    q_treatment_locations <- length(GeoLift$test_id$name)
    df$Estimate <- df$Estimate * q_treatment_locations
    df$lower_bound <- df$lower_bound * q_treatment_locations
    df$upper_bound <- df$upper_bound * q_treatment_locations
    ylab <- "Incremental values"
    if (nchar(title) == 0){
      title <- "Incremental Value per Timestamp"
    }
    if (nchar(subtitle) == 0){
      subtitle <- "GeoLift Analysis"
    }
  } else if (tolower(plot_type) == "att"){
    ylab <- "Average ATT"
    if (nchar(title) == 0){
      title <- "Average Effect on the Treated"
    }
    if (nchar(subtitle) == 0){
      subtitle <- "Average Effect per Timestamp per Location in Treatment"
    }
  } else {stop("Please specify which plot type you would like: ATT or Incrementality.")}

  if (!is.null(treatment_end_date)){
    plot_dates <- get_date_from_test_periods(GeoLift, treatment_end_date, frequency = frequency)
    df$Time <- plot_dates$date_vector
  } else {
    message(
      "You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter.")
    plot_dates <- list(
      treatment_start = GeoLift$TreatmentStart,
      treatment_end = GeoLift$TreatmentEnd
    )
  }
  if (!is.null(plot_start_date)){
    if (is.null(treatment_end_date)){
      stop("If you want to filter your dataset on a date, please specify treatment_end_date param so periods are converted to dates.")
    } else {
      df <- df[df$Time >= plot_start_date,]
    }
  }

  ggplot(df, aes(x = Time, y = Estimate)) +
    geom_line(linetype="dashed", color="#373472", size=0.75) +
    geom_vline(xintercept=plot_dates$treatment_start, linetype="dashed", alpha=0.3) +
    geom_hline(yintercept=0, alpha=0.5) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha=0.2, fill="#4B4196") +
    theme_minimal() +
    labs(y = ylab,
         x = "Periods",
         title = title,
         subtitle = subtitle,
         caption = notes) +
    theme(
      text = element_text(size=20),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5))
}


#' Calculate cumulative lift
#'
#' @description
#' This method will calculate the cumulative lift with each passing day.
#'
#' @param data DataFrame that GeoLfit will use to determine a result.
#' Should be the output of \code{GeoDataRead}.
#' @param treatment_locations Vector of locations where the treatment was applied.
#' @param treatment_start_period Integer representing period where test started.
#' @param treatment_end_period Integer representing period where test finished.
#' @param Y_id Name of the outcome variable (String).
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
#'
#' @return
#' A dataframe that holds the accumulated lift effect throughout the entire treatment period.
#'
#' @export
cumulative_lift <- function(
  data,
  treatment_locations,
  treatment_start_period,
  treatment_end_period,
  location_id = "location",
  time_id = "time",
  Y_id = "Y"
){
  max_test_period <- treatment_start_period + 1
  cumulative_list <- list()
  message("Starting to run iterations of GeoLift to capture cumulative effect.")
  while (max_test_period <= treatment_end_period){
    if (max_test_period %% 5== 0){
      message(paste0(
        "Currently missing ", treatment_end_period - max_test_period, " iterations."))
    }
    filtered_data <- data[data$time <= max_test_period, ]

    gl_output <- suppressMessages(GeoLift(
      data = data,
      locations = treatment_locations,
      treatment_start_time = treatment_start_period,
      treatment_end_time = max_test_period,
      location_id = location_id,
      time_id = time_id,
      Y_id = Y_id,
      ConfidenceIntervals = TRUE))

    att <- gl_output$summary$average_att$Estimate
    att_lb <- gl_output$lower_bound
    att_ub <- gl_output$upper_bound

    incremental_factor <- length(treatment_locations) * (max_test_period - treatment_start_period)

    cumulative_list[[max_test_period - treatment_start_period]] <- list(
      Time = max_test_period,
      att = att,
      att_lb = att_lb,
      att_ub = att_ub,
      incremental = att * incremental_factor,
      incremental_lb = att_lb * incremental_factor,
      incremental_ub = att_ub * incremental_factor
    )
    max_test_period <- max_test_period + 1
  }
  cumulative_lift_df <- do.call(rbind.data.frame, cumulative_list)

  rest_of_df <- data.frame(
    Time = 1:(min(cumulative_lift_df$Time)-1),
    att = 0,
    att_lb = 0,
    att_ub = 0,
    incremental = 0,
    incremental_lb = 0,
    incremental_ub = 0
  )

  cumulative_lift_df <- rbind(rest_of_df, cumulative_lift_df)

  return(cumulative_lift_df)
}

#' Plot the accumulated lift effect.
#'
#' @description
#' Plot the accumulated lift effect.
#'
#' @param data DataFrame that GeoLfit will use to determine a result.
#' Should be the output of \code{GeoDataRead}.
#' @param treatment_locations Vector of locations where the treatment was applied.
#' @param treatment_start_period Integer representing period where test started.
#' @param treatment_end_period Integer representing period where test finished.
#' @param Y_id Name of the outcome variable (String).
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
#' @param treatment_end_date Character that represents a date in year-month-day format.
#' @param frequency Character that represents periodicity of time stamps. Can be either
#' weekly or daily. Defaults to daily.
#' @param plot_start_date Character that represents initial date of plot in year-month-day format.
#' @param title Character for the title of the plot. NULL by default.
#' @param subtitle Character for the subtitle of the plot. NULL by default.
#' @param notes String to add notes to the plot. Empty by default.
#' @param ... additional arguments
#'
#' @return
#' A ggplot object that shows the accumulated lift per time period.
#'
#' @export
cumulative_value.plot <- function(data,
                                  treatment_locations,
                                  treatment_start_period,
                                  treatment_end_period,
                                  location_id = "location",
                                  time_id = "time",
                                  Y_id = "Y",
                                  treatment_end_date = NULL,
                                  frequency = "daily",
                                  plot_start_date = NULL,
                                  title = "",
                                  subtitle = "",
                                  notes = "",
                                  ...){
  incremental <- incremental_lb <- incremental_ub <- NULL
  cumulative_lift_df <- cumulative_lift(
    data = data,
    treatment_locations = treatment_locations,
    treatment_start_period = treatment_start_period,
    treatment_end_period = treatment_end_period,
    location_id = location_id,
    time_id = time_id,
    Y_id = Y_id)

  if (nchar(title) == 0){
    title <- "Accumulated Incremental Value"
  }
  GeoLift <- list(TreatmentEnd = treatment_end_period, TreatmentStart = treatment_start_period)
  if (!is.null(treatment_end_date)){
    plot_dates <- get_date_from_test_periods(GeoLift, treatment_end_date, frequency = frequency)
    cumulative_lift_df$Time <- plot_dates$date_vector
  } else {
    message(
      "You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter.")
    plot_dates <- list(
      treatment_start = GeoLift$TreatmentStart,
      treatment_end = GeoLift$TreatmentEnd)
  }
  if (!is.null(plot_start_date)){
    if (is.null(treatment_end_date)){
      stop("If you want to filter your dataset on a date, please specify treatment_end_date param so periods are converted to dates.")
    } else {
      cumulative_lift_df <- cumulative_lift_df[cumulative_lift_df$Time >= plot_start_date,]
    }
  }
  ggplot(cumulative_lift_df,
         aes(x=Time, y=incremental, group=1)) +
    geom_line(linetype="dashed", color="#373472") +
    geom_ribbon(aes(ymin=incremental_lb, ymax=incremental_ub), alpha=0.2, fill="#4B4196") +
    theme_minimal() +
    labs(y = "Incremental Values",
         x = "Date",
         title = title,
         subtitle = subtitle,
         caption = notes) +
    theme(text = element_text(size=20),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    geom_vline(xintercept=plot_dates$treatment_start, linetype="dashed", alpha=0.3)
}

#' Summary method for GeoLift.
#'
#' @description
#'
#' GeoLift summary output with additional information about the
#' test.
#'
#' @param object GeoLift object.
#' @param ... Optional arguments
#'
#' @return
#' GeoLift summary object that contains:
#'      \itemize{
#'          \item{"ATT":}{ ATT estimate.}
#'          \item{"PercLift":}{ Lift estimate}
#'          \item{"pvalue":}{ Experiment p-value.}
#'          \item{"LowerCI":}{ Lower Confidence Interval.}
#'          \item{"UpperCI":}{ Upper Confidence Interval.}
#'          \item{"GlobalL2Imbalance":}{ Global L2 Imbalance.}
#'          \item{"GlobalL2ImbalanceScaled":}{ Scaled Global L2 Imbalance.}
#'          \item{"IndL2Imbalance":}{ Individual L2 Imbalance.}
#'          \item{"IndL2ImbalanceScaled":}{ Scaled Individual L2 Imbalance.}
#'          \item{"ATT":}{ ATT.}
#'          \item{"start":}{ Treatment start.}
#'          \item{"end":}{ Treatment end.}
#'          \item{"type":}{ Single or Multiple test locations.}
#'          \item{"Y_id":}{ Name of the outcome variable.}
#'          \item{"incremental":}{ Incremental outcome units.}
#'          \item{"bias":}{ Estimated corrected bias.}
#'          \item{"weights":}{ Synthetic Control Weights.}
#'          \item{"CI":}{ Flag indicating whether to include Confidence Intervals.}
#'          \item{"alpha":}{ Significance level.}
#'          \item{"lower":}{ Lower Bound Confidence Interval.}
#'          \item{"upper":}{ Upper Bound Confidence Interval.}
#'       }
#'
#' @export
summary.GeoLift <- function(object, ...){

  if (!inherits(object, 'GeoLift')) {
    stop('object must be class GeoLift')
  }

  summ <- list()

  if(paste(object$results$call)[1] == "single_augsynth"){
    summ$ATT_est <- object$inference$ATT
    summ$PercLift <- object$inference$Perc.Lift
    summ$pvalue <- object$inference$pvalue
    summ$LowerCI <- object$inference$Lower.Conf.Int
    summ$UpperCI <- object$inference$Upper.Conf.Int
    summ$L2Imbalance <- object$summary$l2_imbalance #NEWCHANGE
    summ$L2ImbalanceScaled <- object$summary$scaled_l2_imbalance #NEWCHANGE
    #summ$PercentImprove <- round(1 - GeoLift$summary$scaled_l2_imbalance,3)*100 #NEWCHANGE
    summ$ATT <- object$summary$att
    summ$start <- object$TreatmentStart
    summ$end <- object$TreatmentEnd
    summ$type <- "single"
    summ$Y_id <- object$Y_id
    summ$incremental <- object$incremental
    summ$bias <- mean(object$summary$bias_est)
    summ$weights <- object$results$weights
    summ$CI <- object$ConfidenceIntervals
    summ$alpha <- object$summary$alpha
    summ$lower <- object$lower_bound
    summ$upper <- object$upper_bound
    summ$factor <- ncol(object$results$data$y) * nrow(object$test_id)
    summ$progfunc <- object$results$progfunc
  }

  class(summ) <- "summary.GeoLift"

  return(summ)

}


#' Summary plotting method for GeoLift.
#'
#' @description
#'
#' Summary plotting method for GeoLift.
#'
#' @param x GeoLift summary object.
#' @param ... Optional arguments.
#'
#' @return
#'
#' Plot GeoLift summary.
#'
#' @export
print.summary.GeoLift <- function(x, ...){

  if (!inherits(x, 'summary.GeoLift')) {
    stop('object must be class summary.GeoLift')
  }

  message("\nGeoLift Results Summary\n")

  if(x$type == "single" & x$CI == FALSE){
    message(paste0(
      "##################################",
      "\n#####     Test Statistics    #####\n",
      "##################################\n",
      "\n* Average ATT: ", round(x$ATT_est,3),
      "\n* Percent Lift: ", round(x$PercLift,2),"%",
      "\n* Incremental ", paste(x$Y_id), ": ",round(x$incremental,0),
      "\n* P-value: ", round(x$pvalue,2),
      #"\n* 90% Confidence Interval: (", round(summ$LowerCI,3),  NEWCHANGE: Need to fix
      #", ", round(summ$UpperCI,3), ")",

      "\n\n##################################",
      "\n#####   Balance Statistics   #####\n",
      "##################################\n",
      "\n* L2 Imbalance: ", round(x$L2Imbalance,3),
      "\n* Scaled L2 Imbalance: ", round(x$L2ImbalanceScaled,4),
      "\n* Percent improvement from naive model: ", round(1 - x$L2ImbalanceScaled,4)*100,"%",
      "\n* Average Estimated Bias: ", round(x$bias,3),

      "\n\n##################################",
      "\n#####     Model Weights      #####\n",
      "##################################\n",
      "\n* Prognostic Function: ", toupper(x$progfunc), "\n",
      "\n* Model Weights:"
    )
    )
    for (row in 1:nrow(x$weights)){
      if (abs(round(as.double(x$weights$weight[row]), 4)) >= 0.0001){
        message(paste0(" * ", x$weights$location[row], ": ", round(x$weights$weight[row], 4)))
      }

    }
  }
  else if (x$type == "single" & x$CI == TRUE){
    message(paste0(
      "##################################",
      "\n#####     Test Statistics    #####\n",
      "##################################\n",
      "\n* Average ATT: ", round(x$ATT_est,3),
      "\n* Percent Lift: ", round(x$PercLift,2),"%",
      "\n* Incremental ", paste(x$Y_id), ": ",round(x$incremental,0),
      "\n* P-value: ", round(x$pvalue,2),
      "\n* ", (1 - x$alpha) * 100, "% Confidence Interval: (", round(x$lower * x$factor,3),", ", round(x$upper * x$factor,3), ")",

      "\n\n##################################",
      "\n#####   Balance Statistics   #####\n",
      "##################################\n",
      "\n* L2 Imbalance: ", round(x$L2Imbalance,3),
      "\n* Scaled L2 Imbalance: ", round(x$L2ImbalanceScaled,4),
      "\n* Percent improvement from naive model: ", round(1 - x$L2ImbalanceScaled,4)*100,"%",
      "\n* Average Estimated Bias: ", round(x$bias,3),

      "\n\n##################################",
      "\n#####     Model Weights      #####\n",
      "##################################\n",
      "\n* Model Weights:"
    )
    )
    for (row in 1:nrow(x$weights)){
      if (abs(round(as.double(x$weights$weight[row]), 4)) >= 0.0001){
        message(paste0(" * ", x$weights$location[row], ": ", round(x$weights$weight[row], 4)))
      }
      
    }
  }


}


#' GeoLift
#'
#' @description A package implementing the Augmented Synthetic Controls Method
#' @docType package
#' @name GeoLift-package
#' @importFrom magrittr "%>%"
#' @import augsynth
#' @import gsynth
#' @import panelView
#' @import doParallel
#' @import foreach
#' @import ggrepel
#' @import MarketMatching
#' @import stringr
#' @import directlabels
#' @import ggplot2
#' @import tibble
#' @import tidyr
#' @import parallel
#' @import graphics
#' @import stats
#' @import utils
#' @import rlang
NULL
