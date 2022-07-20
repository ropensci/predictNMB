total_nmb <- function(tn, tp, fn, fp, utility_tp, utility_tn, cost_fp, cost_fn, ...) {
  total_nmb <- utility_tp * tp + utility_tn * tn + cost_fp * fp + cost_fn * fn
  total_nmb <- matrix(total_nmb, ncol = 1)
  colnames(total_nmb) <- "total_nmb"
  total_nmb
}

roc_iu <- function(tp, fp, tn, fn, .roc_curve, ...) {
  tempauc <- cutpointr::auc(.roc_curve)
  sens <- cutpointr::sensitivity(tp = tp, fn = fn)
  spec <- cutpointr::specificity(fp = fp, tn = tn)
  iu <- abs(sens - tempauc) + abs(spec - tempauc)
  iu <- matrix(iu, ncol = 1)
  colnames(iu) <- "roc_iu"
  return(iu)
}


#' Gets probability thresholds given predicted probabilities, outcomes and NMB.
#'
#' @param predicted vector of predicted probabilities
#' @param actual vector of actual outcomes
#' @param nmb named vector containing NMB assigned to true positives, true negatives, false positives and false negatives
#'
#' @return list
#' @export
#'
#' @examples
#' get_thresholds(
#'   predicted = runif(1000),
#'   actual = sample(c(0, 1), size = 1000, replace = TRUE),
#'   nmb = c("TP" = 1, "TN" = 2, "FP" = 3, "FN" = 4)
#' )
get_thresholds <- function(predicted, actual, nmb) {
  pt_er <- cutpointr::cutpointr(
    x = predicted, class = actual, method = cutpointr::minimize_metric, metric = cutpointr::roc01,
    silent = TRUE
  )[["optimal_cutpoint"]]
  if (length(pt_er) > 1) {
    pt_er <- stats::median(pt_er)
  }

  pt_youden <- cutpointr::cutpointr(
    x = predicted, class = actual, method = cutpointr::maximize_metric, metric = cutpointr::youden,
    silent = TRUE
  )[["optimal_cutpoint"]]
  if (pt_youden > 1) {
    pt_youden <- 1
  }

  pt_cost_effective <- cutpointr::cutpointr(
    x = predicted, class = actual, method = cutpointr::maximize_metric, metric = total_nmb,
    utility_tp = nmb["TP"], utility_tn = nmb["TN"],
    cost_fp = nmb["FP"], cost_fn = nmb["FN"],
    silent = TRUE
  )[["optimal_cutpoint"]]
  if (pt_cost_effective > 1) {
    pt_cost_effective <- 1
  }

  pt_cz <- cutpointr::cutpointr(
    x = predicted, class = actual, method = cutpointr::maximize_metric, metric = cutpointr::prod_sens_spec,
    silent = TRUE
  )[["optimal_cutpoint"]]
  if (pt_cz > 1) {
    pt_cz <- 1
  }

  pt_iu <- cutpointr::cutpointr(
    x = predicted, class = actual, method = cutpointr::minimize_metric, metric = roc_iu,
    silent = TRUE
  )[["optimal_cutpoint"]]
  if (pt_iu > 1) {
    pt_iu <- 1
  }

  costs <- -nmb

  pt_cost_minimising <-
    (costs["FP"] - costs["TN"]) /
      (costs["FP"] + costs["FN"] - costs["TP"] - costs["TN"])
  names(pt_cost_minimising) <- NULL

  list(
    treat_all = 0,
    treat_none = 1,
    cost_effective = pt_cost_effective,
    er = pt_er,
    youden = pt_youden,
    cz = pt_cz,
    iu = pt_iu,
    cost_minimising = pt_cost_minimising
  )
}
