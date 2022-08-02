#' @export
make_summary_table <- function(x, ...) {
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
make_summary_table.predictNMBscreen <- function(
    x,
    what = c("nmb", "inb", "cutpoints"),
    inb_ref_col = NULL,
    agg_functions = list(
      "median" = stats::median,
      "95% CI" = function(x) paste0(signif(quantile(x, probs = c(0.025, 0.975)), digits = 2), collapse = " to ")
    ),
    show_full_inputs = FALSE,
    ...) {
  get_row_from_sim <- function(sim_idx) {
    data <- get_sim_data(x$simulations[[sim_idx]], what = what[1], inb_ref_col = inb_ref_col)

    as.data.frame(sapply(agg_functions, mapply, dplyr::select(data, -n_sim))) %>%
      tibble::rownames_to_column(var = "method") %>%
      tidyr::pivot_wider(names_from = method, values_from = !method)
  }

  sim_aggregations <- lapply(1:length(x$simulations), get_row_from_sim)
  sim_aggregations <- do.call("rbind", sim_aggregations)

  if (show_full_inputs) {
    inputs <- x$summary_grid
  } else {
    inputs <- x$summary_grid[, names(x$screen_meta)]
  }

  cbind(inputs, sim_aggregations)
}
