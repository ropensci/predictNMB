#' Make a NMB sampler for use in \code{do_nmb_sim()} or
#' \code{screen_simulation_inputs()}
#'
#' @param outcome_cost The cost of the outcome. Must be provided if \code{wtp}
#' and \code{qalys_lost} are not.
#' Or can be used in addition to these arguments to represent additional cost
#' to the health burden.
#' @param wtp Willingness-to-pay.
#' @param qalys_lost Quality-adjusted life years (QALYs) lost due to healthcare
#' event being predicted.
#' @param high_risk_group_treatment_effect The effect of the treatment provided
#' to patients given high risk prediction. Can be a number of a function.
#' Provide a function to incorporate uncertainty.
#' @param high_risk_group_treatment_cost The cost of the treatment provided to
#' patients given high risk prediction. Can be a number of a function.
#' Provide a function to incorporate uncertainty.
#' @param low_risk_group_treatment_effect The effect of the treatment provided
#' to patients given low risk prediction. Can be a number of a function.
#' Provide a function to incorporate uncertainty. Defaults to 0 (no treatment).
#' @param low_risk_group_treatment_cost The cost of the treatment provided to
#' patients given low risk prediction. Can be a number of a function.
#' Provide a function to incorporate uncertainty. Defaults to 0 (no treatment).
#' @param use_expected_values Logical. If \code{TRUE}, gets the mean of many
#' samples from the produced function and returns these every time. This is a
#' sensible choice when using the resulting function for selecting the cutpoint.
#' See \code{fx_nmb_training}. Defaults to \code{FALSE}.
#' @param nboot The number of samples to use when creating a function that
#' returns the expected values. Defaults to 10000.
#'
#' @return Returns a \code{NMBsampler} object.
#' @export
#'
#' @examples
#' get_nmb_training <- get_nmb_sampler(
#'   outcome_cost = 100,
#'   high_risk_group_treatment_effect = function() rbeta(1, 1, 2),
#'   high_risk_group_treatment_cost = 10,
#'   use_expected_values = TRUE
#' )
#' get_nmb_evaluation <- get_nmb_sampler(
#'   outcome_cost = 100,
#'   high_risk_group_treatment_effect = function() rbeta(1, 1, 2),
#'   high_risk_group_treatment_cost = 10
#' )
#'
#' get_nmb_training()
#' get_nmb_training()
#' get_nmb_training()
#' get_nmb_evaluation()
#' get_nmb_evaluation()
#' get_nmb_evaluation()
get_nmb_sampler <- function(outcome_cost,
                            wtp,
                            qalys_lost,
                            high_risk_group_treatment_effect,
                            high_risk_group_treatment_cost,
                            low_risk_group_treatment_effect = 0,
                            low_risk_group_treatment_cost = 0,
                            use_expected_values = FALSE,
                            nboot = 10000) {
  if ((missing(wtp) | missing(qalys_lost)) &
    !(missing(wtp) & missing(qalys_lost))) {
    stop(
      "willingness to pay (wtp) or QALYS lost (qalys_lost) ",
      "are provided but not both. Please provide both or neither."
    )
  }

  track_qalys <- !missing(qalys_lost) & !missing(wtp)

  if (missing(outcome_cost) & missing(wtp)) {
    stop(
      "no costs provided for making nmb function. Please provide either a",
      " willingness to pay and qalys lost,",
      " or the cost of the outcome (outcome_cost), or both."
    )
  }

  if (missing(outcome_cost)) {
    outcome_cost <- 0
  }
  if (missing(wtp)) {
    wtp <- 0
  }
  if (missing(qalys_lost)) {
    qalys_lost <- 0
  }
  .f <- function() {
    if (is.function(outcome_cost)) {
      outcome_cost <- outcome_cost()
    }
    if (is.function(wtp)) {
      wtp <- wtp()
    }
    if (is.function(qalys_lost)) {
      qalys_lost <- qalys_lost()
    }
    if (is.function(high_risk_group_treatment_effect)) {
      high_risk_group_treatment_effect <- high_risk_group_treatment_effect()
    }
    if (is.function(high_risk_group_treatment_cost)) {
      high_risk_group_treatment_cost <- high_risk_group_treatment_cost()
    }
    if (is.function(low_risk_group_treatment_effect)) {
      low_risk_group_treatment_effect <- low_risk_group_treatment_effect()
    }
    if (is.function(low_risk_group_treatment_cost)) {
      low_risk_group_treatment_cost <- low_risk_group_treatment_cost()
    }

    if (track_qalys) {
      c(
        "TP" = -(outcome_cost + wtp * qalys_lost) *
          (1 - high_risk_group_treatment_effect) - high_risk_group_treatment_cost,
        "FP" = -high_risk_group_treatment_cost,
        "TN" = -low_risk_group_treatment_cost,
        "FN" = -(outcome_cost + wtp * qalys_lost) *
          (1 - low_risk_group_treatment_effect) - low_risk_group_treatment_cost,
        "qalys_lost" = qalys_lost,
        "wtp" = wtp,
        "outcome_cost" = outcome_cost,
        "high_risk_group_treatment_effect" = high_risk_group_treatment_effect,
        "high_risk_group_treatment_cost" = high_risk_group_treatment_cost,
        "low_risk_group_treatment_effect" = low_risk_group_treatment_effect,
        "low_risk_group_treatment_cost" = low_risk_group_treatment_cost
      )
    } else {
      c(
        "TP" = -(outcome_cost + wtp * qalys_lost) *
          (1 - high_risk_group_treatment_effect) - high_risk_group_treatment_cost,
        "FP" = -high_risk_group_treatment_cost,
        "TN" = -low_risk_group_treatment_cost,
        "FN" = -(outcome_cost + wtp * qalys_lost) *
          (1 - low_risk_group_treatment_effect) - low_risk_group_treatment_cost
      )
    }
  }

  if (use_expected_values) {
    boots <- do.call("rbind", replicate(nboot, .f(), simplify = FALSE))
    expected_values <- colMeans(boots)
    .f <- function() {
      expected_values
    }
  }

  class(.f) <- "NMBsampler"
  attr(.f, "track_qalys") <- track_qalys
  attr(.f, "wtp") <- wtp

  .f
}
