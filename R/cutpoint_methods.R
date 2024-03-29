#' Function that calculates the total Net Monetary Benefit
#' (used by the "value_optimising" cutpoint method).
#'
#' @param tn Number of True Negatives
#' @param tp Number of True Positives
#' @param fn Number of False Negatives
#' @param fp Number of False Positives
#' @param utility_tp NMB associated with True Positives
#' @param utility_tn NMB associated with True Negatives
#' @param cost_fp NMB associated with False Positives
#' @param cost_fn NMB associated with False Negatives
#' @param ... Optional (ignored) arguments.
#'
#' @return Returns the total Net Monetary Benefit.
#' @noRd
total_nmb <- function(tn,
                      tp,
                      fn,
                      fp,
                      utility_tp,
                      utility_tn,
                      cost_fp,
                      cost_fn,
                      ...) {
  total_nmb <- utility_tp * tp + utility_tn * tn + cost_fp * fp + cost_fn * fn
  total_nmb <- matrix(total_nmb, ncol = 1)
  colnames(total_nmb) <- "total_nmb"
  total_nmb
}

#' Function that calculates the Index of Union
#' (used by the "index_of_union" cutpoint method).
#'
#' @param tn Number of True Negatives
#' @param tp Number of True Positives
#' @param fn Number of False Negatives
#' @param fp Number of False Positives
#' @param .roc_curve A Receiver Operating Characteristic curve.
#' @param ... Optional (ignored) arguments.
#'
#' @return Returns the value (to be minimised) for the Index of Union.
#' @noRd
roc_iu <- function(tp, fp, tn, fn, .roc_curve, ...) {
  tempauc <- cutpointr::auc(.roc_curve)
  sens <- cutpointr::sensitivity(tp = tp, fn = fn)
  spec <- cutpointr::specificity(fp = fp, tn = tn)
  iu <- abs(sens - tempauc) + abs(spec - tempauc)
  iu <- matrix(iu, ncol = 1)
  colnames(iu) <- "roc_iu"
  return(iu)
}

#' Get a vector of all the inbuilt cutpoint methods
#'
#' @return Returns a vector cutpoint methods that can be used in
#' \code{do_nmb_sim()}.
#' @export
#'
#' @examples
#' get_inbuilt_cutpoint_methods()
get_inbuilt_cutpoint_methods <- function() {
  c(
    "all",
    "none",
    "value_optimising",
    "youden",
    "cost_minimising",
    "prod_sens_spec",
    "roc01",
    "index_of_union"
  )
}

#' Get a cutpoint using the methods inbuilt to predictNMB
#'
#' @param predicted A vector of predicted probabilities
#' @param actual A vector of actual outcomes
#' @param nmb A named vector containing NMB assigned to each classification
#' @param method A cutpoint selection method to be used
#' methods that can be used as the method argument
#'
#' @return Returns a selected cutpoint (numeric).
#' @export
#'
#' @examples
#' ## get the list of available methods:
#' get_inbuilt_cutpoint_methods()
#'
#' ## get the cutpoint that maximises the Youden index for a given set of
#' ## probabilities and outcomes
#' get_inbuilt_cutpoint(
#'   predicted = runif(1000),
#'   actual = sample(c(0, 1), size = 1000, replace = TRUE),
#'   method = "youden"
#' )
#'
get_inbuilt_cutpoint <- function(predicted,
                                 actual,
                                 nmb,
                                 method) {
  inbuilt_methods <- get_inbuilt_cutpoint_methods()

  if (length(unique(actual)) != 2) {
    stop(
      paste0(
        "data should contain only two levels but had: ",
        paste0(unique(actual), collapse = ",")
      )
    )
  }

  if (!method %in% inbuilt_methods) {
    stop(
      paste0(
        "the method, '",
        method,
        "' is not within the available inbuilt methods:\n",
        paste0(paste0("'", inbuilt_methods, "'"), collapse = ", ")
      )
    )
  }

  if (method == "all") {
    return(0)
  }

  if (method == "none") {
    return(1)
  }

  if (method == "value_optimising") {
    pt_value_optimising <- cutpointr::cutpointr(
      x = predicted, class = actual, method = cutpointr::maximize_metric,
      metric = total_nmb,
      utility_tp = nmb["TP"], utility_tn = nmb["TN"],
      cost_fp = nmb["FP"], cost_fn = nmb["FN"],
      silent = TRUE
    )[["optimal_cutpoint"]]
    if (pt_value_optimising > 1) {
      pt_value_optimising <- 1
    }
    return(pt_value_optimising)
  }

  if (method == "youden") {
    pt_youden <- cutpointr::cutpointr(
      x = predicted, class = actual, method = cutpointr::maximize_metric,
      metric = cutpointr::youden,
      silent = TRUE
    )[["optimal_cutpoint"]]
    if (pt_youden > 1) {
      pt_youden <- 1
    }
    return(pt_youden)
  }

  if (method == "cost_minimising") {
    costs <- -nmb
    pt_cost_minimising <-
      (costs["FP"] - costs["TN"]) /
        (costs["FP"] + costs["FN"] - costs["TP"] - costs["TN"])
    names(pt_cost_minimising) <- NULL
    return(pt_cost_minimising)
  }

  if (method == "prod_sens_spec") {
    pt_cz <- cutpointr::cutpointr(
      x = predicted, class = actual, method = cutpointr::maximize_metric,
      metric = cutpointr::prod_sens_spec,
      silent = TRUE
    )[["optimal_cutpoint"]]
    if (pt_cz > 1) {
      pt_cz <- 1
    }
    return(pt_cz)
  }

  if (method == "roc01") {
    pt_er <- cutpointr::cutpointr(
      x = predicted, class = actual, method = cutpointr::minimize_metric,
      metric = cutpointr::roc01,
      silent = TRUE
    )[["optimal_cutpoint"]]
    if (length(pt_er) > 1) {
      pt_er <- stats::median(pt_er)
    }
    return(pt_er)
  }

  if (method == "index_of_union") {
    pt_iu <- cutpointr::cutpointr(
      x = predicted, class = actual, method = cutpointr::minimize_metric,
      metric = roc_iu,
      silent = TRUE
    )[["optimal_cutpoint"]]
    if (pt_iu > 1) {
      pt_iu <- 1
    }
    return(pt_iu)
  }
}


#' Gets probability thresholds given predicted probabilities, outcomes and NMB.
#'
#' @param predicted A vector of predicted probabilities.
#' @param actual A vector of actual outcomes.
#' @param nmb A named vector containing NMB assigned to true positives,
#' true negatives, false positives and false negatives
#' @param cutpoint_methods Which cutpoint method(s) to return.
#' The default (NULL) uses all the inbuilt methods.
#'
#' @return Returns a \code{list}.
#' @export
#'
#' @examples
#' \donttest{
#' # get thresholds using default (all inbuilt) cutpoint methods
#' get_thresholds(
#'   predicted = runif(1000),
#'   actual = sample(c(0, 1), size = 1000, replace = TRUE),
#'   nmb = c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#' )
#'
#'
#' # get cutpoints using user-defined functions
#' # These functions must take the \code{predicted} and \code{actual}
#' # as arguments. They can also take \code{nmb} (named vector containing NMB
#' # with values for TP, FP, TN, FN).
#' fx_roc01 <- function(predicted, actual, ...) {
#'   cutpointr::cutpointr(
#'     x = predicted, class = actual, method = cutpointr::minimize_metric,
#'     metric = cutpointr::roc01,
#'     silent = TRUE
#'   )[["optimal_cutpoint"]]
#' }
#'
#' fx_sum_sens_spec <- function(predicted, actual, ...) {
#'   cutpointr::cutpointr(
#'     x = predicted, class = actual, method = cutpointr::maximize_metric,
#'     metric = cutpointr::sum_sens_spec,
#'     silent = TRUE
#'   )[["optimal_cutpoint"]]
#' }
#'
#' get_thresholds(
#'   predicted = runif(1000),
#'   actual = sample(c(0, 1), size = 1000, replace = TRUE),
#'   cutpoint_methods = c("fx_roc01", "fx_sum_sens_spec"),
#'   nmb = c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#' )
#'
#' # get a combination of cutpoints from both user-defined functions and
#' # inbuilt methods
#' get_thresholds(
#'   predicted = runif(1000),
#'   actual = sample(c(0, 1), size = 1000, replace = TRUE),
#'   cutpoint_methods = c(
#'     "fx_roc01",
#'     "fx_sum_sens_spec",
#'     "youden",
#'     "all",
#'     "none"
#'   ),
#'   nmb = c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#' )
#' }
get_thresholds <- function(predicted, actual, nmb, cutpoint_methods = NULL) {
  inbuilt_methods <- get_inbuilt_cutpoint_methods()
  if (is.null(cutpoint_methods)) {
    cutpoint_methods <- inbuilt_methods
  }

  # get cutpoints using inbuilt methods
  inbuilt_methods <- cutpoint_methods[cutpoint_methods %in% inbuilt_methods]
  inbuilt_cutpoints <- lapply(
    inbuilt_methods,
    function(x) {
      get_inbuilt_cutpoint(
        actual = actual, predicted = predicted, nmb = nmb, method = x
      )
    }
  )

  names(inbuilt_cutpoints) <- inbuilt_methods
  inbuilt_cutpoints <- unlist(inbuilt_cutpoints)

  # get cutpoints using user-provided functions
  non_inbuilt_methods <- cutpoint_methods[
    !cutpoint_methods %in% inbuilt_methods
  ]
  if (length(non_inbuilt_methods) != 0) {
    non_inbuilt_cutpoints <- lapply(
      non_inbuilt_methods,
      function(x) {
        do.call(
          x,
          list(predicted = predicted, actual = actual, nmb = nmb)
        )
      }
    )

    names(non_inbuilt_cutpoints) <- non_inbuilt_methods
    non_inbuilt_cutpoints <- unlist(non_inbuilt_cutpoints)
  } else {
    non_inbuilt_cutpoints <- c()
  }

  return(c(inbuilt_cutpoints, non_inbuilt_cutpoints))
}
