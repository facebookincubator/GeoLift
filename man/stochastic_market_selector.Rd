% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pre_test_power.R
\name{stochastic_market_selector}
\alias{stochastic_market_selector}
\title{Stochastic Market Selector.}
\usage{
stochastic_market_selector(
  treatment_size,
  similarity_matrix,
  run_stochastic_process = FALSE
)
}
\arguments{
\item{treatment_size}{Is the amount of location units within the
treatment group.}

\item{similarity_matrix}{Matrix that sorts each location in terms
of descending correlation.}

\item{run_stochastic_process}{A logic flag indicating whether to select test
markets through random sampling of the the similarity matrix. Given that
interpolation biases may be relevant if the synthetic control matches
the characteristics of the test unit by averaging away large discrepancies
between the characteristics of the test and the units in the synthetic controls,
it is recommended to only use random sampling after making sure all units are
similar. This parameter is set by default to FALSE.}
}
\value{
Returns a matrix of sampled combinations of treatments.
Each row represents a different treatment.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

\code{stochastic_market_selector} selects the markets to be tested
by randomly sampling from the \code{similarity_matrix}.
It gets groups of 2 elements and samples one of them. It repeats
this process until the \code{treatment_size} is equal to the sample.
}
