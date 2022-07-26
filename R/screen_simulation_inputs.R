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
#'
#' @return predictNMBscreen
#' @export
#'
#' @examples
#'
#' # perform screen with increasing values of model discimination (sim_auc)
#' if (FALSE) {
#'   get_nmb <- function() c("TP" = 1, "TN" = 2, "FP" = 3, "FN" = 4)
#'   sim_screen_obj <- screen_simulation_inputs(
#'     n_sims = 50, n_valid = 10000, sim_auc = seq(0.7, 0.9, 0.1), event_rate = 0.1,
#'     fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
#'   )
#' }
screen_simulation_inputs <- function(sample_size, n_sims, n_valid, sim_auc, event_rate,
                                     cutpoint_methods = get_inbuilt_cutpoint(return_all_methods = TRUE),
                                     fx_nmb_training, fx_nmb_evaluation, meet_min_events = FALSE) {
  if (missing(sample_size)) {
    sample_size <- NA
  }

  if (inherits(fx_nmb_training, "function")) {
    fx_nmb_training <- list(fx_nmb_training)
  }

  if (inherits(fx_nmb_evaluation, "function")) {
    fx_nmb_evaluation <- list(fx_nmb_evaluation)
  }

  input_grid <- tidyr::expand_grid(
    sample_size = round_input(sample_size),
    n_sims = round_input(n_sims),
    n_valid = round_input(n_valid),
    sim_auc = round_input(sim_auc),
    event_rate = round_input(event_rate),
    fx_nmb_training = fx_nmb_training,
    fx_nmb_evaluation = fx_nmb_evaluation
  )

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
        meet_min_events = meet_min_events
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
    grid = input_grid,
    screen_meta = screen_meta,
    simulations = simulations
  )

  class(res) <- "predictNMBscreen"

  res
}

round_input <- function(x) {
  as.numeric(as.character(x))
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
