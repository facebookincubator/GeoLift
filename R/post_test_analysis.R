# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function GeoLift, cumulative_lift, summary.GeoLift,
# print.summary.GeoLift.


#' GeoLift inference calculation.
#'
#' @description
#'
#' `GeoLift` performs inference for a geo-test.
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
                    print = TRUE) {

  # Rename variables to standard names used by GeoLift
  data <- data %>% dplyr::rename(
    time = time_id,
    Y = Y_id,
    location = location_id
  )

  data$location <- tolower(data$location)
  locations <- tolower(locations)

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

  # Optimizing model based on Scaled L2 Score
  if (model == "best") {
    all_ascms <- list()
    for (progfunc in c("none", "ridge", "GSYN")) {
      if (length(locations) == 1 & progfunc == "GSYN") {
        all_ascms[[progfunc]] <- list("scaled_l2_imbalance" = 1)
      } else {
        message(
          "Running model with ",
          ifelse(progfunc == "none", "no", progfunc),
          " prognostic function.\n"
        )
        ascm <- tryCatch(
          expr = {
            suppressMessages(augsynth::augsynth(fmla,
              unit = location, time = time,
              data = data_aux,
              t_int = treatment_start_time,
              progfunc = progfunc,
              scm = T,
              fixedeff = fixed_effects
            ))
          },
          error = function(e) {
            list("scaled_l2_imbalance" = 1)
          }
        )
        all_ascms[[eval(progfunc)]] <- ascm
      }
    }

    scaled_l2_none <- round(all_ascms$none$scaled_l2_imbalance, 3)
    scaled_l2_ridge <- round(all_ascms$ridge$scaled_l2_imbalance, 3)
    scaled_l2_gsyn <- round(all_ascms$GSYN$scaled_l2_imbalance, 3)

    if (scaled_l2_none > scaled_l2_gsyn & scaled_l2_ridge > scaled_l2_gsyn) {
      message("Selected GSYN as best model.")
      augsyn <- all_ascms$GSYN
    } else if (scaled_l2_none > scaled_l2_ridge & scaled_l2_gsyn > scaled_l2_ridge) {
      message("Selected Ridge as best model.")
      augsyn <- all_ascms$ridge
    } else {
      message("Selected model without prognostic function as best model.")
      augsyn <- all_ascms$none
    }
  } else {
    message(
      "Running model with ",
      ifelse(progfunc == "none", "no", progfunc),
      " prognostic function.\n"
    )
    augsyn <- suppressMessages(augsynth::augsynth(fmla,
      unit = location, time = time,
      data = data_aux,
      t_int = treatment_start_time,
      progfunc = model,
      scm = T,
      fixedeff = fixed_effects
    ))
  }

  inference_df <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(inference_df) <- c(
    "ATT",
    "Perc.Lift",
    "pvalue",
    "Lower.Conf.Int",
    "Upper.Conf.Int"
  )

  if (tolower(stat_test) == "total") {
    side_of_test <- "two_sided"
    alternative_hypothesis <- NULL
  } else if (tolower(stat_test) == "negative" | tolower(stat_test) == "positive") {
    side_of_test <- "one_sided"
    alternative_hypothesis <- stat_test
  } else {
    stop("stat_test must be one of {'total', 'negative', 'positive'}.")
  }
  stat_func <- type_of_test(
    side_of_test = side_of_test,
    alternative_hypothesis = alternative_hypothesis
  )

  sum_augsyn <- summary(augsyn, alpha = alpha, stat_func = stat_func)

  if (paste(augsyn$call)[1] == "single_augsynth") {
    mean <- sum_augsyn[["average_att"]][["Estimate"]]
    se <- sum_augsyn[["average_att"]][["Std.Error"]]

    loc_id <- c(which(augsyn$data$trt == 1))
    locs_id <- as.data.frame(loc_id, nrow = length(loc_id))
    locs_id$name <- unlist(unique(data_aux$location)[c(which(augsyn$data$trt == 1))])

    y_obs <- c(augsyn$data$X[loc_id, ], augsyn$data$y[loc_id, ])
    y_hat <- predict(augsyn, att = FALSE)
    ATT <- predict(augsyn, att = TRUE)
    ATT_se <- sum_augsyn$att$Std.Error

    pred_conversions <- predict(augsyn)[treatment_start_time:treatment_end_time]

    if (length(locations) == 1) {
      lift <- (sum(augsyn$data$y[loc_id, ]) - sum(pred_conversions)) /
        abs(sum(pred_conversions))
    } else if (length(locations) > 1) {
      lift <- (sum(colMeans(augsyn$data$y[loc_id, ])) -
        sum(pred_conversions)) /
        abs(sum(pred_conversions))
    }

    incremental <- sum(augsyn$data$y[loc_id, ]) - (sum(pred_conversions) * length(loc_id))

    inference_df <- inference_df %>% tibble::add_row(
      ATT = mean,
      Perc.Lift = 100 * round(lift, 3),
      pvalue = sum_augsyn$average_att$p_val,
      Lower.Conf.Int = sum_augsyn$average_att$lower_bound,
      Upper.Conf.Int = sum_augsyn$average_att$upper_bound
    )
  }

  res <- list(
    "results" = augsyn,
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
    "upper_bound" = summary(augsyn, alpha = alpha, inf_type = "jackknife+")$average_att$upper_bound,
    "df_weights" = data.frame(
      location = dimnames(augsyn$weights)[[1]],
      weight = unname(augsyn$weights[, 1])
    ),
    "stat_test" = stat_test
  )

  class(res) <- c("GeoLift", class(res))

  return(res)
}

#' Print pretty GeoLift output.
#'
#' @description
#'
#' Print GeoLift output.
#'
#' @param x GeoLift object.
#' @param ... Optional arguments
#'
#' @return
#' GeoLift output message
#'
#' @export
print.GeoLift <- function(x, ...) {
  if (!inherits(x, "GeoLift")) {
    stop("object must be class GeoLift")
  }

  if (x$inference$pvalue < 0.05) {
    is_significant <- "The results are significant at a 95% level."
  } else if (x$inference$pvalue < 0.10) {
    is_significant <- "The results are significant at a 90% level."
  } else if (x$inference$pvalue < 0.20) {
    is_significant <- "The results are significant at a 80% level."
  } else {
    is_significant <- "The results are not statistically significant."
  }

  if (toupper(x$stat_test) == "TOTAL") {
    test_type <- "TWO-SIDED LIFT TEST)"
  } else if (toupper(x$stat_test) == "POSITIVE") {
    test_type <- "ONE-SIDED POSITIVE LIFT TEST)"
  } else {
    test_type <- "ONE-SIDED NEGATIVE LIFT TEST)"
  }

  message(paste0(
    paste0("\nGeoLift Output\n\n"),
    paste0(
      "Test results for ", (x$TreatmentEnd - x$TreatmentStart + 1),
      " treatment periods, from time-stamp ",
      x$TreatmentStart, " to ", x$TreatmentEnd,
      " for test markets:"
    )
  ))
  for (i in 1:length(x$test_id$name)) {
    message(paste(i, toupper(x$test_id$name[i])))
  }
  message(paste0(
    "##################################",
    "\n#####     Test Statistics    #####\n",
    "##################################\n",
    "\nPercent Lift: ",
    round(x$inference$Perc.Lift, 3), "%\n\n",
    "Incremental ", paste(x$Y_id), ": ", round(x$incremental, 0), "\n\n",
    "Average Estimated Treatment Effect (ATT): ", round(x$inference$ATT, 3),
    "\n\n", is_significant, " (", test_type,
    "\n\nThere is a ", round(100 * x$inference$pvalue, 2),
    "% chance of observing an effect this large or larger assuming treatment effect is zero.",
    sep = ""
  ))
}

#' Calculate cumulative lift
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This method will calculate the cumulative lift with each passing day.
#'
#' @param data DataFrame that GeoLfit will use to determine a result.
#' Should be the output of `GeoDataRead`.
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
cumulative_lift <- function(data,
                            treatment_locations,
                            treatment_start_period,
                            treatment_end_period,
                            location_id = "location",
                            time_id = "time",
                            Y_id = "Y") {
  max_test_period <- treatment_start_period + 1
  cumulative_list <- list()
  message("Starting to run iterations of GeoLift to capture cumulative effect.")
  while (max_test_period <= treatment_end_period) {
    if (max_test_period %% 5 == 0) {
      message(paste0(
        "Currently missing ", treatment_end_period - max_test_period, " iterations."
      ))
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
      ConfidenceIntervals = TRUE
    ))

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
    Time = 1:(min(cumulative_lift_df$Time) - 1),
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
summary.GeoLift <- function(object, ...) {
  if (!inherits(object, "GeoLift")) {
    stop("object must be class GeoLift")
  }

  summ <- list()

  if (paste(object$results$call)[1] == "single_augsynth") {
    summ$ATT_est <- object$inference$ATT
    summ$PercLift <- object$inference$Perc.Lift
    summ$pvalue <- object$inference$pvalue
    summ$LowerCI <- object$inference$Lower.Conf.Int
    summ$UpperCI <- object$inference$Upper.Conf.Int
    summ$L2Imbalance <- object$summary$l2_imbalance
    summ$L2ImbalanceScaled <- object$summary$scaled_l2_imbalance
    summ$ATT <- object$summary$att
    summ$start <- object$TreatmentStart
    summ$end <- object$TreatmentEnd
    summ$type <- "single"
    summ$Y_id <- object$Y_id
    summ$incremental <- object$incremental
    summ$bias <- mean(object$summary$bias_est)
    summ$weights <- object$df_weights
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

print.summary.GeoLift <- function(x, ...) {
  if (!inherits(x, "summary.GeoLift")) {
    stop("object must be class summary.GeoLift")
  }

  message("\nGeoLift Results Summary\n")

  if (x$type == "single") {
    test_statistics <- paste0(
      "##################################",
      "\n#####     Test Statistics    #####\n",
      "##################################\n",
      "\n* Average ATT: ", round(x$ATT_est, 3),
      "\n* Percent Lift: ", round(x$PercLift, 2), "%",
      "\n* Incremental ", paste(x$Y_id), ": ", round(x$incremental, 0),
      "\n* P-value: ", round(x$pvalue, 2)
    )

    if (x$CI == TRUE) {
      test_statistics <- paste0(
        test_statistics,
        "\n* ", (1 - x$alpha) * 100, "% Confidence Interval: (", round(x$lower * x$factor, 3), ", ", round(x$upper * x$factor, 3), ")"
      )
    }
    balance_statistics <- paste0(
      "\n\n##################################",
      "\n#####   Balance Statistics   #####\n",
      "##################################\n",
      "\n* L2 Imbalance: ", round(x$L2Imbalance, 3),
      "\n* Scaled L2 Imbalance: ", round(x$L2ImbalanceScaled, 4),
      "\n* Percent improvement from naive model: ", round(1 - x$L2ImbalanceScaled, 4) * 100, "%",
      "\n* Average Estimated Bias: ", round(x$bias, 3),
      "\n\n##################################",
      "\n#####     Model Weights      #####\n",
      "##################################\n",
      "\n* Prognostic Function: ", toupper(x$progfunc), "\n",
      "\n* Model Weights:"
    )
    message(test_statistics, balance_statistics)
  }

  sorted_weights <- dplyr::arrange(x$weights, dplyr::desc(abs(x$weights$weight)))
  for (row in 1:nrow(sorted_weights)) {
    if (abs(round(as.double(sorted_weights$weight[row]), 4)) >= 0.0001) {
      message(paste0(" * ", sorted_weights$location[row], ": ", round(sorted_weights$weight[row], 4)))
    }
  }
}
