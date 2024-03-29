% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MultiCell.R
\name{MultiCellWinner}
\alias{MultiCellWinner}
\alias{print.MultiCellWinner}
\title{Multi-Cell Winner Declaration Method for Market Selection.}
\usage{
MultiCellWinner(
  multicell_power_obj,
  effect_size = NULL,
  geolift_type = "standard",
  ROAS = seq(0, 5, 0.05),
  alpha = 0.1,
  method = "conformal",
  stat_test = "Total"
)

\method{print}{MultiCellWinner}(x, ...)
}
\arguments{
\item{multicell_power_obj}{A \code{MultiCellPower} object.}

\item{effect_size}{A numeric value representing the lift to be simulated
across all cells. If not specified (default), the algorithm will use the
largest lift needed to obtain a well-powered test across all cells.}

\item{geolift_type}{String that specifies the type of GeoLift test to be performed:
\itemize{
\item{"standard":}{ Standard GeoLift test where ads are shown to test regions. Defualt.}
\item{"inverse":}{ Inverse or Negative GeoLift test where the test group is holded-out
from the treatment.}
}}

\item{ROAS}{Vector of incremental Return on Ad Spend (iROAS) values to assess. Set to
\code{seq(0,5,0.05)} by default.}

\item{alpha}{Significance Level. By default 0.1.}

\item{method}{A string indicating the method used to calculate the
aggregate ATT Confidence Intervals.
\itemize{
\item{"conformal":}{ Conformal Inference. Defualt.}
\item{"jackknife+":}{ Jackknife+ (exclusively for stat_test = "Total").}
}}

\item{stat_test}{A string indicating the test statistic.
\itemize{
\item{"Total":}{ The test statistic is the sum of all treatment effects, i.e. sum(abs(x)). Default.}
\item{"Negative":}{ One-sided test against positive effects i.e. -sum(x).
Recommended for Negative Lift tests.}
\item{"Positive":}{ One-sided test against negative effects i.e. sum(x).
Recommended for Positive Lift tests.}
}}

\item{x}{\code{MultiCellWinner()}}

\item{...}{Optional arguments}
}
\value{
A list with two objects:
\itemize{
\item{"results":}{ Data frame with all pairwise comparisons and required
iROAS needed to declare a winner for the multi-cell test.}
\item{"simulations":}{ The complete data frame of all simulations for all
pairwise comparisons. }
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\code{MultiCellWinner} explores the likelihood of observe a winning cell for a
given Multi-Cell Market Selection. This method analyzes all pairwise comparisons to
determine how much larger the incremental ROAS (iROAS) must be for a cell to be
declared the winner based on a statistical significance test.
}
