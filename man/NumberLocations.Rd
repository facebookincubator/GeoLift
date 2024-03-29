% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pre_test_power.R
\name{NumberLocations}
\alias{NumberLocations}
\title{Power Calculation to determine the number of test periods
with unknown test locations.}
\usage{
NumberLocations(
  data,
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
  import_augsynth_from = "library(augsynth)",
  import_tidyr_from = "library(tidyr)"
)
}
\arguments{
\item{data}{A data.frame containing the historical conversions by
geographic unit. It requires a "locations" column with the geo name,
a "Y" column with the outcome data (units), a time column with the indicator
of the time period (starting at 1), and covariates.}

\item{number_locations}{List of number of locations to test. If not specified,
the number of locations will be computed by percentiles up to half of the
total number of locations.}

\item{treatment_periods}{Number of treatment periods.}

\item{n_sim}{Number of simulations.}

\item{X}{List of covariate names.}

\item{Y_id}{Name of the outcome variable (String).}

\item{location_id}{Name of the location variable (String).}

\item{time_id}{Name of the time variable (String).}

\item{plot}{Plots results when TRUE.}

\item{power}{Power level. By default 0.8.}

\item{alpha}{Significance Level. By default 0.1.}

\item{type}{Method of inference used in the analysis.
pValue=Provides conformal inference to provide the aggregate
p-value for the null hypothesis of no effect from the intervention.
The Default type is pValue.
Imbalance=Uses the model's Scaled L2 Imbalance metric.}

\item{normalize}{A logic flag indicating whether to scale the outcome which is
useful to accelerate computing speed when the magnitude of the data is large. The
default is FALSE.}

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

\item{fixed_effects}{A logic flag indicating whether to include unit fixed
effects in the model. Set to TRUE by default.}

\item{stat_func}{Function to compute test statistic. NULL by default.}

\item{ProgressBar}{A logic flag indicating whether to display a progress bar
to track progress. Set to FALSE by default.}

\item{parallel}{A logic flag indicating whether to use parallel computing to
speed up calculations. Set to TRUE by default.}

\item{parallel_setup}{A string indicating parallel workers set-up.
Set to "sequential" by default.}

\item{import_augsynth_from}{Points to where the augsynth package
should be imported from to send to the nodes.}

\item{import_tidyr_from}{Points to where the tidyr package
should be imported from to send to the nodes.}
}
\value{
Table of average power by number of locations.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}

Development on \code{NumberLocations()} is complete.
We recommend switching to \code{GeoLiftMarketSelection()}
for new code, which is easier to use, more featureful,
and still under active development.#'

\code{NumberLocations} calculates power to determine the
number of test periods with unknown test locations.
}
