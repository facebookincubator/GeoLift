# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#' GeoLift
#'
#' @description GeoLift is an end-to-end geo-experimental methodology based on
#' Synthetic Control Methods used to measure the true incremental effect (Lift)
#' of an ad campaign.
#' @docType package
#' @name GeoLift
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
"_PACKAGE"

# Suppress 'no visible binding for global variable' warnings
utils::globalVariables(
  c(
    "abs_lift_in_zero",
    "ATT",
    "att_estimator",
    "AvgCost",
    "AvgDetectedLift",
    "AvgScaledL2Imbalance",
    "base_lift",
    "BestControl",
    "correlation",
    "c_obs",
    "c_obs_lower_bound",
    "c_obs_upper_bound",
    "cell",
    "conf_level",
    "conf.level",
    "date_unix",
    "detected_lift",
    "DID",
    "diff_lower",
    "diff_upper",
    "duration",
    "effect_size",
    "EffectSize",
    "Estimate",
    "first_day",
    "Holdout",
    "ID",
    "incremental",
    "incremental_lb",
    "incremental_ub",
    "investment",
    "Investment",
    "Level",
    "Level",
    "lift",
    "location",
    "location_2",
    "Locs",
    "lower",
    "lower_bound",
    "main",
    "mean_L2ScaledImbalance",
    "mean_p",
    "mean_pow",
    "mean_scaled_l2_imbalance",
    "name_vble",
    "post_treatment",
    "pow",
    "ProportionTotal_Y",
    "pvalue",
    "resultsM",
    "ScaledL2Imbalance",
    "significant",
    "sim",
    "simulation_results",
    "summ",
    "sum_y",
    "t_obs",
    "temp_Markets",
    "test",
    "time",
    "Time",
    "Total_Y",
    "treatment_group_size",
    "treatment_location",
    "treatment_start",
    "true_lift",
    "Type",
    "Units",
    "upper",
    "upper_bound",
    "var1",
    "var2",
    "Winner",
    "Y"
  )
)
