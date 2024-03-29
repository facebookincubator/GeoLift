# Copyright (c) Meta Platforms, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#' GeoLift Example Data - PreTest
#'
#' Simulated data of daily conversions for 40 US cities for a period of 90 days.
#' This data provides historica pre-intervention information to run power
#' calculations for GeoLift.
#'
#' @format A data frame with historical information to run GeoLift power calculations:
#' \describe{
#'    \item{date}{Date in yyyy-mm-dd format}
#'    \item{location}{Name of the city}
#'    \item{Y}{Number of conversions}
#' }
#'
"GeoLift_PreTest"


#' GeoLift Example Data - Test
#'
#' Simulated data of daily conversions for 40 US cities for a period of 90
#' pre-intervention days followed by a 15-day campaign on the cities of chicago
#' and portland. This dataset provides information to run a GeoLift test for the
#' two markets that received an intervention.
#'
#' @format A data frame with historical information to run a GeoLift analysis:
#' \describe{
#'    \item{date}{Date in yyyy-mm-dd format}
#'    \item{location}{Name of the city}
#'    \item{Y}{Number of conversions}
#' }
#'
"GeoLift_Test"


#' GeoLift Multi-Cell Example Data - Test
#'
#' Simulated Multi-Cell data of daily conversions for 40 US cities for a period of 90
#' pre-intervention days followed by a 15-day campaign on two cells: Cell 1 chicago
#' & cincinnati and Cell B honolulu and indianapolis.
#' This dataset provides information to run a Multi-Cell GeoLift test for the
#' two cells that received an intervention.
#'
#' @format A data frame with historical information to run a GeoLift analysis:
#' \describe{
#'    \item{date}{Date in yyyy-mm-dd format}
#'    \item{location}{Name of the city}
#'    \item{Y}{Number of conversions}
#' }
#'
"GeoLift_Test_MultiCell"
