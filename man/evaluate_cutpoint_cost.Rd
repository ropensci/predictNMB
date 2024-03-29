% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_cutpoint.R
\name{evaluate_cutpoint_cost}
\alias{evaluate_cutpoint_cost}
\title{Evaluates a cutpoint by returning the mean treatment cost per sample.}
\usage{
evaluate_cutpoint_cost(predicted, actual, pt, nmb)
}
\arguments{
\item{predicted}{A vector of predicted probabilities.}

\item{actual}{A vector of actual outcomes.}

\item{pt}{The probability threshold to be evaluated.}

\item{nmb}{A named vector containing NMB assigned to each classification and
the treatment costs.}
}
\value{
Returns a \code{numeric} value representing the mean cost for that
cutpoint and data.
}
\description{
Evaluates a cutpoint by returning the mean treatment cost per sample.
}
\examples{
evaluate_cutpoint_cost(
  predicted = runif(1000),
  actual = sample(c(0, 1), size = 1000, replace = TRUE),
  pt = 0.1,
  nmb = c(
    "qalys_lost" = 5,
    "low_risk_group_treatment_cost" = 0,
    "high_risk_group_treatment_cost" = 1,
    "low_risk_group_treatment_effect" = 0,
    "high_risk_group_treatment_effect" = 0.3,
    "outcome_cost" = 10
  )
)
}
