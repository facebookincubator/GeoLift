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
