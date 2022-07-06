# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Includes function GeoSampling.


#' Multi-Cell Sampling Method.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `GeoSampling` performs a sampling methodology to split the
#' data set into k similar partitions.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit. It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param k Number of partitions or cells. k = 2 by default.
#' @param sampling_method Sampling Method used to create the k partitions.
#' \itemize{
#'          \item{"systematic":}{ Systematic sampling based on the KPI (Y).
#'          Defualt.}
#'          }
#'
#' @return A data frame designating the cell per location. The resulting object
#' contains two columns: location and cell.
#'
#' @export

GeoSampling <- function(data, k = 2, sampling_method = "systematic"){
  # Create ranking by KPI
  rank_by_loc <- data %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(sum_y = base::sum(Y)) %>%
    dplyr::arrange(dplyr::desc(sum_y)) %>%
    dplyr::mutate(rank = dplyr::row_number())

  N <- nrow(rank_by_loc)
  r <- sample(1:k, k)

  rank_by_loc$cell <- 0

  for (i in 1:length(r)){
    rank_by_loc$cell[seq(r[i], N, k)] <- i
  }

  return(rank_by_loc)

}


