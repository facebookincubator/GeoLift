# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

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
"_PACKAGE"

# Suppress 'no visible binding for global variable' warnings
utils::globalVariables(
  c("ATT", "AvgCost", "AvgScaledL2Imbalance", "BestControl", "conf_level",
    "conf.level", "date_unix", "diff_lower", "diff_upper", "duration",
    "effect_size", "Holdout", "ID", "investment", "Investment", "Level",
    "Level", "lift", "location", "Locs", "lower", "main",
    "mean_L2ScaledImbalance", "mean_p", "mean_pow", "mean_scaled_l2_imbalance",
    "pow", "ProportionTotal_Y", "pvalue", "ScaledL2Imbalance", "significant",
    "sim", "summ", "test", "time", "Time", "Total_Y", "treatment_start", "Type",
    "Units", "upper", "Y")
)