% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pre_processing_data.R
\name{SplitTreatmentEstimation}
\alias{SplitTreatmentEstimation}
\title{GeoLift fit for each Treatment location within Treatment group.}
\usage{
SplitTreatmentEstimation(
  treatment_locations,
  data,
  treatment_start_time,
  treatment_end_time,
  model,
  verbose = FALSE,
  Y_id = "Y",
  time_id = "time",
  location_id = "location",
  X = c(),
  fixed_effects = TRUE
)
}
\arguments{
\item{treatment_locations}{Vector of locations where the treatment was applied.}

\item{data}{DataFrame that GeoLfit will use to determine a result.
Should be the output of \code{GeoDataRead}.}

\item{treatment_start_time}{Time index of the start of the treatment.}

\item{treatment_end_time}{Time index of the end of the treatment.}

\item{model}{A string indicating the outcome model used to augment the Augmented
Synthetic Control Method. Augmentation through a prognostic function can improve
fit and reduce L2 imbalance metrics.
\itemize{
\item{"None":}{ ASCM is not augmented by a prognostic function. Defualt.}
\item{"Ridge":}{ Augments with a Ridge regression. Recommended to improve fit
for smaller panels (less than 40 locations and 100 time-stamps.))}
\item{"GSYN":}{ Augments with a Generalized Synthetic Control Method. Recommended
to improve fit for larger panels (more than 40 locations and 100
time-stamps. }
}}

\item{verbose}{boolean that determines if processing messages will be shown.}

\item{Y_id}{Name of the outcome variable (String).}

\item{time_id}{Name of the time variable (String).}

\item{location_id}{Name of the location variable (String).}

\item{X}{Vector with covariates names.}

\item{fixed_effects}{A logic flag indicating whether to include unit fixed
effects in the model. Set to TRUE by default.}
}
\value{
Dataframe with L2 imbalance ranking and these columns:
\itemize{
\item{"treatment_location":}{ Single Treatment location being considered.}
\item{"l2_imbalance":}{ L2 imbalance for treatment_location estimation.}
\item{"scaled_l2_imbalance":}{ Scaled L2 imbalance for treatment_location estimation.}
\item{"treatment_group_size":}{ Size of treatment group for each iteration.}
\item{"model":}{ Outcome model being used for Augmented Synthetic Control.}
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\code{SplitTreatmentEstimation} fits a control group to each location within a
Treatment group and calculates their imbalance metrics.
}
