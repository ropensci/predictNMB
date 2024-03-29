% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_nmb_sim.R
\name{print.predictNMBsim}
\alias{print.predictNMBsim}
\title{Print a summary of a predictNMBsim object}
\usage{
\method{print}{predictNMBsim}(x, ...)
}
\arguments{
\item{x}{A \code{predictNMBsim} object.}

\item{...}{Optional, ignored arguments.}
}
\value{
\code{print(x)} returns \code{x} invisibly.
}
\description{
Print a summary of a predictNMBsim object
}
\examples{
\donttest{
get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
sim_obj <- do_nmb_sim(
  sample_size = 200, n_sims = 50, n_valid = 10000, sim_auc = 0.7,
  event_rate = 0.1, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
)
print(sim_obj)
}
}
