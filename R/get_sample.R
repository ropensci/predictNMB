#' Samples data for a prediction model with a specified AUC and prevalence.
#'
#' @param auc The Area Under the (receiver operating characteristic) Curve
#' @param n_samples Number of samples to draw
#' @param prevalence Prevalence or event rate of the binary outcome as a proportion (0.1 = 10\%)
#' @param min_events Minimum number of events required in the sample
#'
#' @return data.frame
#' @export
#'
#' @examples get_sample(0.7, 1000, 0.1)
get_sample <- function(auc, n_samples, prevalence, min_events = 0) {
  # http://dx.doi.org/10.5093/ejpalc2018a5
  t <- sqrt(log(1 / (1 - auc)**2))
  z <- t - ((2.515517 + 0.802853 * t + 0.0103328 * t**2) /
    (1 + 1.432788 * t + 0.189269 * t**2 + 0.001308 * t**3))
  d <- z * sqrt(2)

  n_pos <- sum(sample(c(0, 1), n_samples, replace = TRUE, prob = c(1 - prevalence, prevalence)))

  # if n_pos < min_events, add a new random value to the sample until n_pos == min_events
  while (n_pos < min_events) {
    added_sample <- sample(c(0, 1), size = 1, replace = TRUE, prob = c(1 - prevalence, prevalence))
    if (added_sample == 1) {
      n_pos <- n_pos + 1
    }
    n_samples <- n_samples + 1
  }

  n_neg <- n_samples - n_pos

  # if by chance all samples are either positive or negative, repeat the sampling
  # almost all the cutpoint selection methods will fail if there's only 1 class.
  if ((n_pos == 0 | n_neg == 0) & min_events > 0) {
    return(get_sample(auc, n_samples, prevalence, min_events))
  }

  x <- c(stats::rnorm(n_neg, mean = 0), stats::rnorm(n_pos, mean = d))
  y <- c(rep(0, n_neg), rep(1, n_pos))

  return(data.frame(x = x, actual = y))
}
