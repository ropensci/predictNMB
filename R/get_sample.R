#' Samples data for a prediction model with a specified AUC and prevalence.
#'
#' @param auc The Area Under the (receiver operating characteristic) Curve
#' @param n_samples Number of samples to draw
#' @param prevalence Prevalence or event rate of the binary outcome as a proportion (0.1 = 10\%)
#'
#' @return data.frame
#' @export
#'
#' @examples get_sample(0.7, 1000, 0.1)
get_sample <- function(auc, n_samples, prevalence) {
  # http://dx.doi.org/10.5093/ejpalc2018a5
  t <- sqrt(log(1 / (1 - auc)**2))
  z <- t - ((2.515517 + 0.802853 * t + 0.0103328 * t**2) /
    (1 + 1.432788 * t + 0.189269 * t**2 + 0.001308 * t**3))
  d <- z * sqrt(2)

  n_pos <- sum(sample(c(0, 1), n_samples, replace = TRUE, prob = c(1 - prevalence, prevalence)))
  n_neg <- n_samples - n_pos

  x <- c(stats::rnorm(n_neg, mean = 0), stats::rnorm(n_pos, mean = d))
  y <- c(rep(0, n_neg), rep(1, n_pos))

  return(data.frame(x = x, actual = y))
}
