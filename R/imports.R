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
#' @import augsynth
#' @importFrom CommutingZones filter_cluster_file
#' location_to_cluster_match
#' @importFrom directlabels geom_dl dl.combine
#' @importFrom doParallel registerDoParallel
#' @importFrom dplyr add_row arrange bind_rows coalesce dense_rank desc distinct
#' filter group_by left_join mutate rename row_number sample_n select slice
#' slice_max slice_min summarise summarize tibble "%>%"
#' @importFrom foreach foreach
#' @importFrom ggplot2 aes coord_cartesian element_blank element_text geom_hline 
#' geom_line geom_ribbon geom_segment geom_smooth geom_vline ggplot ggtitle guides 
#' guide_legend labs scale_color_manual scale_colour_manual scale_fill_manual 
#' scale_x_continuous scale_y_continuous sec_axis theme theme_minimal
#' xlab xlim ylab ylim
#' @importFrom gridExtra grid.arrange
#' @importFrom MarketMatching best_matches
#' @importFrom panelView panelview
#' @importFrom progress progress_bar
#' @importFrom rlang := sym
#' @importFrom scales percent_format pretty_breaks
#' @importFrom stats as.formula cor power predict quantile sd
#' @importFrom stringr fixed str_count str_replace_all str_split
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils combn flush.console head setTxtProgressBar txtProgressBar
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
    "fbcz_id_num",
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
    "treatment_duration",
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
