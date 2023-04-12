#' Screen many simulation inputs: a parent function to \code{do_nmb_sim()}
#' @description Runs \code{do_nmb_sim()} with a range of inputs.
#'
#' @param sample_size A value (or vector of values): Sample size of training
#' set. If missing, a sample size calculation will be performed and the
#' calculated size will be used.
#' @param n_sims A value (or vector of values): Number of simulations to run.
#' @param n_valid A value (or vector of values): Sample size for evaluation set.
#' @param sim_auc A value (or vector of values): Simulated model discrimination
#' (AUC).
#' @param event_rate A value (or vector of values): simulated event rate of the
#' binary outcome being predicted.
#' @param cutpoint_methods cutpoint methods to include. Defaults to use the
#' inbuilt methods. This doesn't change across calls to \code{do_nmb_sim()}.
#' @param fx_nmb_training A function (or list of functions) that returns named
#' vector of NMB assigned to classifications use for obtaining cutpoint on
#' training set.
#' @param fx_nmb_evaluation A function (or list of functions) that returns
#' named vector of NMB assigned to classifications use for obtaining cutpoint
#' on evaluation set.
#' @param pair_nmb_train_and_evaluation_functions \code{logical}.
#' Whether or not to pair the lists of functions passed for
#' \code{fx_nmb_training} and \code{fx_nmb_evaluation}. If two treatment
#' strategies are being used, it may make more sense to pair these because
#' selecting a value-optimising or cost-minimising threshold using one strategy
#' but evaluating another is likely unwanted.
#' @param meet_min_events Whether or not to incrementally add samples until the
#' expected number of events (\code{sample_size * event_rate}) is met.
#' (Applies to sampling of training data only.)
#' @param min_events A value: the minimum number of events to include in the
#' training sample. If less than this number are included in sample of size
#' \code{sample_size}, additional samples are added until the min_events is met.
#' The default (\code{NA}) will use the expected value given the
#' \code{event_rate} and the \code{sample_size}.
#' @param show_progress Logical. Whether to display a progress bar.
#' @param cl A cluster made using \code{parallel::makeCluster()}. If a cluster
#' is provided, the simulation will be done in parallel.
#'
#' @srrstats {G2.1a} Data types for all inputs are documented.
#' @srrstats {G2.8} Appropriate conversions applied before passing of inputs.
#' @srrstats {G2.11} tibbles used for appropriate processing of columns of
#' functions.
#' @srrstats {EA2.0, EA2.1, EA2.2, EA2.2a, EA2.2b} Plotting of
#' 'predictNMBscreen' objects uses the index column from \code{input_grid} to
#' perform joins. All values within the input_grid are unique to the
#' combination of possible input values (created in
#' \code{screen_simulation_inputs()} using
#' \code{.sim_id = dplyr::row_number()}).
#' Attribute is set for index column name.
#' @srrstats {EA4.0} Types for inputs are not modified.
#' They are checked by \code{validate_inputs()} but not modified before being
#' used to create \code{input_grid} and passed to be used as simulation inputs.
#'
#' @return Returns a \code{predictNMBscreen} object.
#' @export
#'
#' @examples
#'
#' # Screen for optimal cutpoints given increasing values of
#' # model discrimination (sim_auc)
#' \donttest{
#' get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#' sim_screen_obj <- screen_simulation_inputs(
#'   n_sims = 50, n_valid = 10000, sim_auc = seq(0.7, 0.9, 0.1),
#'   event_rate = 0.1, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
#' )
#' }
screen_simulation_inputs <- function(sample_size,
                                     n_sims,
                                     n_valid,
                                     sim_auc,
                                     event_rate,
                                     cutpoint_methods = get_inbuilt_cutpoint_methods(),
                                     fx_nmb_training, fx_nmb_evaluation,
                                     pair_nmb_train_and_evaluation_functions = FALSE,
                                     meet_min_events = TRUE,
                                     min_events = NA,
                                     show_progress = FALSE,
                                     cl = NULL) {
  if (show_progress) {
    if (!requireNamespace("pbapply", quietly = TRUE)) {
      message(
        "The 'pbapply' package is required for displaying a progress bar ",
        "'show_progress' will be changed to FALSE."
      )
      show_progress <- FALSE
    }
  }

  if (missing(sample_size)) {
    sample_size <- NA
  }

  if (is.function(fx_nmb_training)) {
    fx_nmb_training <- list("unnamed-nmb-function-1" = fx_nmb_training)
  } else {
    names(fx_nmb_training) <- fill_fx_names(fx_nmb_training)
  }

  if (is.function(fx_nmb_evaluation)) {
    fx_nmb_evaluation <- list("unnamed-nmb-function-1" = fx_nmb_evaluation)
  } else {
    names(fx_nmb_evaluation) <- fill_fx_names(fx_nmb_evaluation)
  }

  small_grid <-
    tidyr::expand_grid(
      sample_size = sample_size,
      sim_auc = sim_auc,
      event_rate = event_rate,
      min_events = min_events,
      meet_min_events = meet_min_events
    ) %>%
    add_sample_size_calcs()

  small_grid$small_grid_id <- seq_len(nrow(small_grid))

  if (pair_nmb_train_and_evaluation_functions) {
    stopifnot(length(fx_nmb_training) == length(fx_nmb_training))

    input_grid <- tidyr::expand_grid(
      small_grid_id = seq_len(nrow(small_grid)),
      n_sims = n_sims,
      n_valid = n_valid,
      fx_nmb_both = mapply(
        c,
        fx_nmb_training,
        fx_nmb_evaluation,
        SIMPLIFY = FALSE
      )
    )

    train_fxs <-
      tibble::as_tibble(
        list(
          fx_nmb_training = lapply(input_grid$fx_nmb_both, "[[", 1)
        )
      )

    valid_fxs <-
      tibble::as_tibble(
        list(
          fx_nmb_evaluation = lapply(input_grid$fx_nmb_both, "[[", 2)
        )
      )

    functions_frame <- cbind(train_fxs, valid_fxs)

    input_grid <-
      input_grid %>%
      dplyr::select(-fx_nmb_both) %>%
      cbind(functions_frame) %>%
      tibble::as_tibble()
  } else {
    input_grid <- tidyr::expand_grid(
      small_grid_id = seq_len(nrow(small_grid)),
      n_sims = n_sims,
      n_valid = n_valid,
      fx_nmb_training = fx_nmb_training,
      fx_nmb_evaluation = fx_nmb_evaluation
    )
  }

  input_grid <-
    input_grid %>%
    dplyr::inner_join(small_grid, by = "small_grid_id") %>%
    dplyr::select(-small_grid_id) %>%
    dplyr::mutate(.sim_id = dplyr::row_number())

  summary_grid <- input_grid %>%
    dplyr::mutate(
      fx_nmb_training = names(fx_nmb_training),
      fx_nmb_evaluation = names(fx_nmb_evaluation)
    )

  if (nrow(input_grid) == 1) {
    stop(
      "it looks like you've only entered one possible value for each argument
      You'd be better off running the simulation directly through do_nmb_sim()"
    )
  }

  simulations <- lapply(
    seq_len(nrow(input_grid)),
    function(i) {
      if (show_progress) {
        message(
          "Running simulation: [", i, "/", nrow(input_grid), "]"
        )
      }
      do_nmb_sim(
        sample_size = input_grid$sample_size[i],
        n_sims = input_grid$n_sims[i],
        n_valid = input_grid$n_valid[i],
        sim_auc = input_grid$sim_auc[i],
        event_rate = input_grid$event_rate[i],
        cutpoint_methods = cutpoint_methods,
        fx_nmb_training = input_grid$fx_nmb_training[[i]],
        fx_nmb_evaluation = input_grid$fx_nmb_evaluation[[i]],
        meet_min_events = input_grid$meet_min_events[[i]],
        min_events = input_grid$min_events[i],
        show_progress = show_progress,
        cl = cl
      )
    }
  )

  # record which inputs are varying across screen
  screen_meta_vars <- c(
    "sample_size",
    "n_sims",
    "n_valid",
    "sim_auc",
    "event_rate",
    "fx_nmb_training",
    "fx_nmb_evaluation"
  )

  screen_meta_lengths <- lapply(screen_meta_vars, function(x) length(get(x)))
  screen_meta <- lapply(screen_meta_vars[screen_meta_lengths > 1], function(x) get(x))
  names(screen_meta) <- screen_meta_vars[screen_meta_lengths > 1]

  res <- list(
    input_grid = input_grid,
    summary_grid = summary_grid,
    screen_meta = screen_meta,
    simulations = simulations,
    pair_nmb_train_and_evaluation_functions = pair_nmb_train_and_evaluation_functions
  )

  class(res) <- "predictNMBscreen"

  attr(res, "index") <- ".sim_id"

  res
}


#' Fill names for a list of functions if no names are given.
#'
#' @param x a (potentially) named list of functions.
#' @noRd
fill_fx_names <- function(fx) {
  newnames <- names(fx)
  newnames[newnames == ""] <- paste0(
    "unnamed-nmb-function-",
    grep("^$", newnames[newnames == ""])
  )
  newnames
}


#' Add sample sizes to grid of inputs before running simulations.
#'
#' @param x A grid of inputs, including details required to perform sample
#' size calculations.
#' @noRd
add_sample_size_calcs <- function(x) {
  out <- lapply(
    seq_len(nrow(x)),
    function(i) {
      do_sample_size_calc(
        cstatistic = x$sim_auc[i],
        prevalence = x$event_rate[i],
        sample_size = x$sample_size[i],
        min_events = x$min_events[i],
        meet_min_events = x$meet_min_events[i]
      )
    }
  )

  ss_calculations <- data.frame(
    sample_size = do.call("c", lapply(out, "[[", "sample_size")),
    min_events = do.call("c", lapply(out, "[[", "min_events"))
  )

  x$sample_size <- ss_calculations$sample_size
  x$min_events <- ss_calculations$min_events
  x
}


#' Print a summary of a predictNMBscreen object
#'
#' @export
#' @param x A \code{predictNMBscreen} object.
#' @param ... Optional, ignored arguments.
#' @return `print(x)` returns `x` invisibly.
#'
#' @examples
#' \donttest{
#' get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#' sim_screen_obj <- screen_simulation_inputs(
#'   n_sims = 50, n_valid = 10000, sim_auc = seq(0.7, 0.9, 0.1),
#'   event_rate = 0.1,
#'   fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
#' )
#' print(sim_screen_obj)
#' }
print.predictNMBscreen <- function(x, ...) {
  cat("predictNMBscreen object\n\n")
  cat("There were", nrow(x$grid), "combinations screened\n\n")

  if (length(x$screen_meta) == 1) {
    cat(
      "There was only one input (",
      names(x$screen_meta),
      ") that was screened for multiple values:\n"
    )
    print(x$screen_meta)
  } else {
    cat(
      "There were multiple inputs (",
      paste0(names(x$screen_meta), collapse = ", "),
      ") that was screened for multiple values:\n"
    )
    print(x$screen_meta)
  }
  invisible(x)
}
