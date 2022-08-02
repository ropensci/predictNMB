#' Screen many simulation inputs: a parent function to \code{do_nmb_sim()}
#'
#' @param sample_size a value (or vector of values): training data sample size. If missing, a sample size calculation will be performed and the calculated size will be used.
#' @param n_sims a value (or vector of values): the number of simulations to run.
#' @param n_valid a value (or vector of values): sample size for evaluation set.
#' @param sim_auc a value (or vector of values): simulated model discrimination (AUC).
#' @param event_rate a value (or vector of values): simulated event rate of the binary outcome being predicted.
#' @param cutpoint_methods cutpoint methods to include. Defaults to use the inbuilt methods. This doesn't change across calls to \code{do_nmb_sim()}
#' @param fx_nmb_training a function (or list of functions) that returns named vector of NMB assigned to classifications use for obtaining cutpoint on training set
#' @param fx_nmb_evaluation a function (or list of functions) that returns named vector of NMB assigned to classifications use for obtaining cutpoint on evaluation set
#' @param meet_min_events Whether or not to incrementally add samples until the expected number of events (\code{sample_size * event_rate}) is met. (Applies to sampling of training data only.)
#' @param min_events a value: the minimum number of events to include in the training sample. If less than this number are included in sample of size \code{sample_size}, additional samples are added until the min_events is met. The default (\code{NA}) will use the expected value given the \code{event_rate} and the \code{sample_size}.
#' @param cl A cluster made using \code{parallel::makeCluster()}. If a cluster is provided, the simulation will be done in parallel.
#'
#' @return predictNMBscreen
#' @export
#'
#' @examples
#'
#' # perform screen with increasing values of model discimination (sim_auc)
#' if (FALSE) {
#'   get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#'   sim_screen_obj <- screen_simulation_inputs(
#'     n_sims = 50, n_valid = 10000, sim_auc = seq(0.7, 0.9, 0.1), event_rate = 0.1,
#'     fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
#'   )
#' }
screen_simulation_inputs <- function(sample_size, n_sims, n_valid, sim_auc, event_rate,
                                     cutpoint_methods = get_inbuilt_cutpoint(return_all_methods = TRUE),
                                     fx_nmb_training, fx_nmb_evaluation, meet_min_events = FALSE,
                                     min_events = NA, cl = NULL) {
  if (missing(sample_size)) {
    sample_size <- NA
  }

  if (inherits(fx_nmb_training, "function")) {
    fx_nmb_training <- list(fx_nmb_training)
  }

  if (inherits(fx_nmb_evaluation, "function")) {
    fx_nmb_evaluation <- list(fx_nmb_evaluation)
  }

  small_grid <-
    tidyr::expand_grid(
      sample_size = sample_size,
      sim_auc = sim_auc,
      event_rate = event_rate,
      min_events = min_events
    ) %>%
    add_sample_size_calcs()

  small_grid$.small_grid_id <- 1:nrow(small_grid)

  input_grid <- tidyr::expand_grid(
    .small_grid_id = 1:nrow(small_grid),
    n_sims = n_sims,
    n_valid = n_valid,
    fx_nmb_training = fx_nmb_training,
    fx_nmb_evaluation = fx_nmb_evaluation
  ) %>%
    inner_join(., small_grid, by = ".small_grid_id") %>%
    dplyr::select(-.small_grid_id)

  summary_grid <- tidyr::expand_grid(
    .small_grid_id = 1:nrow(small_grid),
    n_sims = n_sims,
    n_valid = n_valid,
    fx_nmb_training = get_fx_names(fx_nmb_training),
    fx_nmb_evaluation = get_fx_names(fx_nmb_evaluation)
  ) %>%
    inner_join(., small_grid, by = ".small_grid_id") %>%
    dplyr::select(-.small_grid_id)

  if (nrow(input_grid) == 1) {
    stop("it looks like you've only entered one possible value for each argument
          You'd be better off running the simulation directly through do_nmb_sim()")
  }

  simulations <- lapply(
    1:nrow(input_grid),
    function(i) {
      do_nmb_sim(
        sample_size = input_grid$sample_size[i],
        n_sims = input_grid$n_sims[i],
        n_valid = input_grid$n_valid[i],
        sim_auc = input_grid$sim_auc[i],
        event_rate = input_grid$event_rate[i],
        cutpoint_methods = cutpoint_methods,
        fx_nmb_training = input_grid$fx_nmb_training[[i]],
        fx_nmb_evaluation = input_grid$fx_nmb_evaluation[[i]],
        meet_min_events = meet_min_events,
        min_events = input_grid$min_events[i],
        cl = cl
      )
    }
  )

  # record which inputs are varying across screen
  screen_meta <- list()
  if (length(sample_size) != 1) {
    screen_meta <- c(screen_meta, list(sample_size = sample_size))
  }

  if (length(n_sims) != 1) {
    screen_meta <- c(screen_meta, list(n_sims = n_sims))
  }

  if (length(n_valid) != 1) {
    screen_meta <- c(screen_meta, list(n_valid = n_valid))
  }

  if (length(sim_auc) != 1) {
    screen_meta <- c(screen_meta, list(sim_auc = sim_auc))
  }

  if (length(event_rate) != 1) {
    screen_meta <- c(screen_meta, list(event_rate = event_rate))
  }

  if (length(fx_nmb_training) != 1) {
    screen_meta <- c(screen_meta, list(fx_nmb_training = fx_nmb_training))
  }

  if (length(fx_nmb_evaluation) != 1) {
    screen_meta <- c(screen_meta, list(fx_nmb_evaluation = fx_nmb_evaluation))
  }


  res <- list(
    input_grid = input_grid,
    summary_grid = summary_grid,
    screen_meta = screen_meta,
    simulations = simulations
  )

  class(res) <- "predictNMBscreen"

  res
}


get_fx_names <- function(x) {
  len <- length(x)
  fx_names <- names(x)

  if (is.null(fx_names)) {
    fx_names <- paste0("unnamed-nmb-function-", 1:len)
  }

  fx_names
}


add_sample_size_calcs <- function(x) {
  out <- lapply(
    1:nrow(x),
    function(i) {
      do_sample_size_calc(
        cstatistic = x$sim_auc[i],
        prevalence = x$event_rate[i],
        sample_size = x$sample_size[i],
        min_events = x$min_events[i]
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


#' @export
print.predictNMBscreen <- function(x, digits = 2, ...) {
  cat("predictNMBscreen object\n\n")
  cat("There were", nrow(x$grid), "combinations screened\n\n")

  if (length(x$screen_meta) == 1) {
    cat("There was only one input (", names(x$screen_meta), ") that was screened for multiple values:\n")
    print(x$screen_meta)
  } else {
    cat(
      "There were multiple inputs (", paste0(names(x$screen_meta), collapse = ", "),
      ") that was screened for multiple values:\n"
    )
    print(x$screen_meta)
  }
}
