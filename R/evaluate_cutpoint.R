#' Evaluates a cutpoint by returning the mean NMB per sample.
#'
#' @param predicted A vector of predicted probabilities.
#' @param actual A vector of actual outcomes.
#' @param pt The probability threshold to be evaluated.
#' @param nmb A named vector containing NMB assigned to each classification.
#'
#' @return Returns a \code{numeric} value representing the NMB for that
#' cutpoint and data.
#' @export
#'
#' @examples
#' evaluate_cutpoint_nmb(
#'   predicted = runif(1000),
#'   actual = sample(c(0, 1), size = 1000, replace = TRUE),
#'   pt = 0.1,
#'   nmb = c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#' )
evaluate_cutpoint_nmb <- function(predicted, actual, pt, nmb) {
  d <- cbind(predicted, actual, NA)
  colnames(d) <- c("predicted", "actual", "nmb")

  d[d[, "predicted"] < pt & d[, "actual"] == 0, "nmb"] <- nmb["TN"]
  d[d[, "predicted"] < pt & d[, "actual"] == 1, "nmb"] <- nmb["FN"]
  d[d[, "predicted"] > pt & d[, "actual"] == 1, "nmb"] <- nmb["TP"]
  d[d[, "predicted"] > pt & d[, "actual"] == 0, "nmb"] <- nmb["FP"]

  mean(d[, "nmb"])
}


#' Evaluates a cutpoint by returning the mean QALYs lost per sample.
#'
#' @param predicted A vector of predicted probabilities.
#' @param actual A vector of actual outcomes.
#' @param pt The probability threshold to be evaluated.
#' @param nmb A named vector containing NMB assigned to each classification and
#' the treatment effects and QALYS lost due to the event of interest.
#'
#' @return Returns a \code{numeric} value representing the mean QALYs for that
#' cutpoint and data.
#' @export
#'
#' @examples
#' evaluate_cutpoint_qalys(
#'   predicted = runif(1000),
#'   actual = sample(c(0, 1), size = 1000, replace = TRUE),
#'   pt = 0.1,
#'   nmb = c(
#'     "qalys_lost" = 5,
#'     "low_risk_group_treatment_effect" = 0,
#'     "high_risk_group_treatment_effect" = 0.5
#'   )
#' )
evaluate_cutpoint_qalys <- function(predicted, actual, pt, nmb) {
  d <- cbind(predicted, actual, NA)
  colnames(d) <- c("predicted", "actual", "qalys")

  d[d[, "predicted"] < pt & d[, "actual"] == 0, "qalys"] <- 0 # no qalys lost when event doesn't occur
  d[d[, "predicted"] < pt & d[, "actual"] == 1, "qalys"] <- -nmb["qalys_lost"] * (1 - nmb["low_risk_group_treatment_effect"])
  d[d[, "predicted"] > pt & d[, "actual"] == 1, "qalys"] <- -nmb["qalys_lost"] * (1 - nmb["high_risk_group_treatment_effect"])
  d[d[, "predicted"] > pt & d[, "actual"] == 0, "qalys"] <- 0 # no qalys lost when event doesn't occur

  mean(d[, "qalys"])
}


#' Evaluates a cutpoint by returning the mean treatment cost per sample.
#'
#' @param predicted A vector of predicted probabilities.
#' @param actual A vector of actual outcomes.
#' @param pt The probability threshold to be evaluated.
#' @param nmb A named vector containing NMB assigned to each classification and
#' the treatment costs.
#'
#' @return Returns a \code{numeric} value representing the mean cost for that
#' cutpoint and data.
#' @export
#'
#' @examples
#' evaluate_cutpoint_cost(
#'   predicted = runif(1000),
#'   actual = sample(c(0, 1), size = 1000, replace = TRUE),
#'   pt = 0.1,
#'   nmb = c(
#'     "qalys_lost" = 5,
#'     "low_risk_group_treatment_cost" = 0,
#'     "high_risk_group_treatment_cost" = 0.5
#'   )
#' )
evaluate_cutpoint_cost <- function(predicted, actual, pt, nmb) {
  d <- cbind(predicted, actual, NA)
  colnames(d) <- c("predicted", "actual", "cost")

  d[d[, "predicted"] < pt, "cost"] <- nmb["low_risk_group_treatment_cost"]
  d[d[, "predicted"] > pt, "cost"] <- nmb["high_risk_group_treatment_cost"]

  mean(d[, "cost"])
}
