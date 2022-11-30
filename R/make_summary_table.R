#' Create table summaries of simulation objects.
#'
#' @param x \code{predictNMBscreen} or \code{predictNMBsim} object
#' @param what what to summarise: one of "nmb", "inb" or "cutpoints". Defaults to "nmb". See `details` for more.
#' @param inb_ref_col which cutpoint method to use as the reference strategy when calculating the incremental net monetary benefit. See \code{do_nmb_sim} for more information.
#' @param agg_functions named list of functions to use to aggregate the selected values. Defaults to the median and 95\% interval.
#' @param rename_vector a named vector for renaming the methods in the summary. The values of the vector are the default names and the names given are the desired names in the output.
#' @param ... additional, optional arguments.
#'
#' @details Table summaries will be based on the `what` argument. Using "nmb" returns the simulated values for NMB, with no reference group;
#' "inb" returns the difference between simulated values for NMB and a set strategy defined by `inb_ref_col`; "cutpoints" returns the cutpoints selected (0,1).
#' @export
#' @examples
#'
#' # perform screen with increasing values of model discimination (sim_auc)
#' if (FALSE) {
#'   get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#'   sim_screen_obj <- screen_simulation_inputs(
#'     n_sims = 50, n_valid = 10000, sim_auc = seq(0.7, 0.9, 0.1), event_rate = 0.1,
#'     fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
#'   )
#'   make_summary_table(sim_screen_obj)
#'   make_summary_table(sim_screen_obj$simulations[[1]])
#' }
make_summary_table <- function(x,
                               what = c("nmb", "inb", "cutpoints"),
                               inb_ref_col = NULL,
                               agg_functions = list(
                                 "median" = function(x) round(stats::median(x), digits = 2),
                                 "95% CI" = function(x) paste0(round(stats::quantile(x, probs = c(0.025, 0.975)), digits = 1), collapse = " to ")
                               ),
                               rename_vector,
                               ...) {
  UseMethod("make_summary_table")
}

get_sim_data <- function(x, what, inb_ref_col = NULL) {
  if (what %in% c("nmb", "inb")) {
    res <- x$df_result
    if (what == "inb") {
      res <- res %>%
        dplyr::mutate(dplyr::across(!n_sim, function(x) x - !!rlang::sym(inb_ref_col))) %>%
        dplyr::select(-dplyr::all_of(inb_ref_col))
    }
  } else {
    res <- x$df_thresholds
  }
  res
}


#' @export
make_summary_table.predictNMBscreen <- function(x,
                                                what = c("nmb", "inb", "cutpoints"),
                                                inb_ref_col = NULL,
                                                agg_functions = list(
                                                  "median" = function(x) round(stats::median(x), digits = 2),
                                                  "95% CI" = function(x) paste0(round(stats::quantile(x, probs = c(0.025, 0.975)), digits = 1), collapse = " to ")
                                                ),
                                                rename_vector,
                                                show_full_inputs = FALSE,
                                                ...) {
  rename_vector <- update_rename_vector(rename_vector)
  get_row_from_sim <- function(sim_idx) {
    get_sim_data(x$simulations[[sim_idx]], what = what[1], inb_ref_col = inb_ref_col) %>%
      dplyr::rename(dplyr::any_of(rename_vector)) %>%
      dplyr::summarize(dplyr::across(!n_sim, agg_functions))
  }

  sim_aggregations <- lapply(seq_len(length(x$simulations)), get_row_from_sim)
  sim_aggregations <- do.call("rbind", sim_aggregations)

  if (show_full_inputs) {
    inputs <- x$summary_grid
  } else {
    inputs <- x$summary_grid[, names(x$screen_meta)]
  }

  cbind(inputs, sim_aggregations)
}


#' @export
make_summary_table.predictNMBsim <- function(x,
                                             what = c("nmb", "inb", "cutpoints"),
                                             inb_ref_col = NULL,
                                             agg_functions = list(
                                               "median" = function(x) round(stats::median(x), digits = 2),
                                               "95% CI" = function(x) paste0(round(stats::quantile(x, probs = c(0.025, 0.975)), digits = 1), collapse = " to ")
                                             ),
                                             rename_vector,
                                             ...) {
  rename_vector <- update_rename_vector(rename_vector)
  get_sim_data(x, what = what[1], inb_ref_col = inb_ref_col) %>%
    dplyr::rename(dplyr::any_of(rename_vector)) %>%
    tidyr::pivot_longer(!n_sim, names_to = "method") %>%
    dplyr::group_by(method) %>%
    dplyr::summarize(dplyr::across(value, agg_functions, .names = "{.fn}"))
}
