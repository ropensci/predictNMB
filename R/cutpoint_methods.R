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


#' Get a cutpoint using the methods inbuilt to predictNMB
#'
#' @param predicted vector of predicted probabilities
#' @param actual vector of actual outcomes
#' @param nmb named vector containing NMB assigned to each classification
#' @param method cutpoint selection method to be used
#' @param return_all_methods logical: whether to return all available methods that can be used as the method argument
#'
#' @return a selected cutpoint (numeric)
#' @export
#'
#' @examples
#' ## get the list of available methods:
#' get_inbuilt_cutpoint(return_all_methods = TRUE)
#'
#' ## get the cutpoint that maximises the Youden index for a given set of probabilities and outcomes
#' get_inbuilt_cutpoint(
#'   predicted = runif(1000),
#'   actual = sample(c(0, 1), size = 1000, replace = TRUE),
#'   method = "youden"
#' )
#'
get_inbuilt_cutpoint <- function(predicted, actual, nmb, method, return_all_methods = FALSE) {
  inbuilt_methods <- c("all", "none", "value_optimising", "youden", "cost_minimising", "prod_sens_spec", "roc01", "index_of_union")

  if (return_all_methods) {
    return(inbuilt_methods)
  }
  # return(runif(1))
  if (length(unique(actual)) != 2) {
    stop(paste0("data were all ", unique(actual), "'s"))
  }

  if (!method %in% inbuilt_methods) {
    stop(
      paste0(
        "the method, '", method, "' is not within the available inbuilt methods:\n",
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
      x = predicted, class = actual, method = cutpointr::maximize_metric, metric = total_nmb,
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
      x = predicted, class = actual, method = cutpointr::maximize_metric, metric = cutpointr::youden,
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
      x = predicted, class = actual, method = cutpointr::maximize_metric, metric = cutpointr::prod_sens_spec,
      silent = TRUE
    )[["optimal_cutpoint"]]
    if (pt_cz > 1) {
      pt_cz <- 1
    }
    return(pt_cz)
  }

  if (method == "roc01") {
    pt_er <- cutpointr::cutpointr(
      x = predicted, class = actual, method = cutpointr::minimize_metric, metric = cutpointr::roc01,
      silent = TRUE
    )[["optimal_cutpoint"]]
    if (length(pt_er) > 1) {
      pt_er <- stats::median(pt_er)
    }
    return(pt_er)
  }

  if (method == "index_of_union") {
    pt_iu <- cutpointr::cutpointr(
      x = predicted, class = actual, method = cutpointr::minimize_metric, metric = roc_iu,
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
#' @param predicted vector of predicted probabilities
#' @param actual vector of actual outcomes
#' @param nmb named vector containing NMB assigned to true positives, true negatives, false positives and false negatives
#' @param cutpoint_methods which cutpoint method(s) to return. The default (NULL) uses all the inbuilt methods.
#'
#' @return list
#' @export
#'
#' @examples
#'
#' # get thresholds using default (all inbuilt) cutpoint methods
#' get_thresholds(
#'   predicted = runif(1000),
#'   actual = sample(c(0, 1), size = 1000, replace = TRUE),
#'   nmb = c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#' )
#'
#'
#' # get cutpoints using user-defined functions
#' # These functions must take the \code{predicted} and \code{actual} as arguments
#' # They can also take \code{nmb} (named vector containing NMB with values for TP, FP, TN, FN)
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
#' # get a combination of cutpoints from both user-defined functions and inbuilt methods
#' get_thresholds(
#'   predicted = runif(1000),
#'   actual = sample(c(0, 1), size = 1000, replace = TRUE),
#'   cutpoint_methods = c("fx_roc01", "fx_sum_sens_spec", "youden", "all", "none"),
#'   nmb = c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#' )
get_thresholds <- function(predicted, actual, nmb, cutpoint_methods = NULL) {
  if (is.null(cutpoint_methods)) {
    cutpoint_methods <- get_inbuilt_cutpoint(return_all_methods = TRUE)
  }

  # out <- runif(n=length(cutpoint_methods))
  # names(out) <- cutpoint_methods
  # return(out)
  # get cutpoints using inbuilt methods
  inbuilt_methods <- cutpoint_methods[cutpoint_methods %in% get_inbuilt_cutpoint(return_all_methods = TRUE)]
  inbuilt_cutpoints <- lapply(
    inbuilt_methods,
    function(x) get_inbuilt_cutpoint(actual = actual, predicted = predicted, nmb = nmb, method = x)
  )

  names(inbuilt_cutpoints) <- inbuilt_methods
  inbuilt_cutpoints <- unlist(inbuilt_cutpoints)

  # get cutpoints using user-provided functions
  non_inbuilt_methods <- cutpoint_methods[!cutpoint_methods %in% get_inbuilt_cutpoint(return_all_methods = TRUE)]
  if (length(non_inbuilt_methods) != 0) {
    non_inbuilt_cutpoints <- lapply(
      non_inbuilt_methods,
      function(x) do.call(x, list(predicted = predicted, actual = actual, nmb = nmb))
    )

    names(non_inbuilt_cutpoints) <- non_inbuilt_methods
    non_inbuilt_cutpoints <- unlist(non_inbuilt_cutpoints)
  } else {
    non_inbuilt_cutpoints <- c()
  }

  return(c(inbuilt_cutpoints, non_inbuilt_cutpoints))
}
