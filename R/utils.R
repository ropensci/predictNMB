utils::globalVariables(
  c(
    "name", "value", "percentile", "n_sim", "m", "outcome", "count",
    "in_interval", "ymin", "ymax", "method", "small_grid_id", "fx_nmb_both",
    "qalys", "costs"
  )
)


#' Performs sample size calculation (if required) and minimum events
#' (if desired).
#'
#' @param cstatistic C-statistic AKA Area Under the Curve.
#' @param prevalence Rate of the event ("event_rate" elsewhere).
#' @param sample_size Sample size specified by user.
#' @param min_events Minimum number of events specified by user.
#' @param meet_min_events Whether or not to meet the minimum number of events.
#'
#' @return Returns a named \code{list} with sample size and minimum events.
#' @noRd
do_sample_size_calc <- function(cstatistic,
                                prevalence,
                                sample_size,
                                min_events,
                                meet_min_events) {
  if (is.na(sample_size)) {
    pmsamp <- pmsampsize::pmsampsize(
      type = "b",
      cstatistic = cstatistic,
      parameters = 1,
      prevalence = prevalence
    )
    sample_size <- pmsamp$sample_size
  }

  if (meet_min_events) {
    if (!is.na(min_events)) {
      min_events <- min_events
    } else if (is.na(sample_size)) {
      min_events <- round(pmsamp$events)
    } else {
      min_events <- round(sample_size * prevalence)
    }
  } else {
    min_events <- 1
  }

  if (is.na(min_events)) {
    min_events <- ifelse(meet_min_events, round(sample_size * prevalence), 1)
  }

  list(
    sample_size = sample_size,
    min_events = min_events
  )
}

#' Check the user's inputs to ensure they are as expected before running
#' simulation.
#'
#' @param sample_size Argument given to \code{do_nmb_sim()}.
#' @param n_sims Argument given to \code{do_nmb_sim()}.
#' @param n_valid Argument given to \code{do_nmb_sim()}.
#' @param sim_auc Argument given to \code{do_nmb_sim()}.
#' @param event_rate Argument given to \code{do_nmb_sim()}.
#' @param cutpoint_methods Argument given to \code{do_nmb_sim()}.
#' @param fx_nmb_training Argument given to \code{do_nmb_sim()}.
#' @param fx_nmb_evaluation Argument given to \code{do_nmb_sim()}.
#' @param meet_min_events Argument given to \code{do_nmb_sim()}.
#' @param min_events Argument given to \code{do_nmb_sim()}.
#' @param cl Argument given to \code{do_nmb_sim()}.
#'
#' @srrstats {G2.0, G2.1, G2.6, G2.13, G2.14a, G2.16} All input types are
#' asserted and restricted to reasonable
#' ranges where necessary.
#' @srrstats {EA2.6} General assessment by assertthat is performed
#' (is.count, is.number) and does not assess or hold expectations on additional
#' class attributes.
#'
#' @return Returns nothing. Produces error if there's problems with inputs.
#' @noRd
validate_inputs <- function(sample_size,
                            n_sims,
                            n_valid,
                            sim_auc,
                            event_rate,
                            cutpoint_methods,
                            fx_nmb_training,
                            fx_nmb_evaluation,
                            meet_min_events,
                            min_events,
                            cl) {
  input_list <- list(
    n_sims = n_sims,
    n_valid = n_valid,
    sim_auc = sim_auc,
    event_rate = event_rate,
    fx_nmb_training = fx_nmb_training,
    fx_nmb_evaluation = fx_nmb_evaluation,
    meet_min_events = meet_min_events,
    min_events = min_events
  )

  if (!missing(sample_size)) {
    input_list <- c(list(sample_size = sample_size), input_list)
  }

  input_lengths <- vapply(input_list, length, 1)

  if (any(input_lengths > 1)) {
    stop(
      "Input(s) for ",
      "[", paste0(names(which(input_lengths > 1)), collapse = ", "), "]",
      " have length greater than 1!\n\n",
      "Did you mean to use screen_simulation_inputs() instead of do_nmb_sim()?"
    )
  }

  if (!missing(sample_size)) {
    stopifnot(assertthat::is.count(sample_size))
  }

  stopifnot(assertthat::is.count(n_sims))
  stopifnot(assertthat::is.count(n_valid))
  stopifnot(assertthat::is.number(sim_auc))
  stopifnot(sim_auc > 0 & sim_auc < 1)
  stopifnot(assertthat::is.number(event_rate))
  stopifnot(event_rate > 0 & event_rate < 1)
  stopifnot(inherits(cutpoint_methods, "character"))
  stopifnot(is.logical(meet_min_events))

  if (!missing(fx_nmb_training)) {
    stopifnot(is.function(fx_nmb_training))
  }

  stopifnot(is.function(fx_nmb_evaluation))

  if (!is.na(min_events)) {
    stopifnot(assertthat::is.count(min_events))
  }

  if (!is.na(min_events) & meet_min_events & missing(sample_size)) {
    message(
      "Power analyses is being performed to estimate sample size but",
      "'min_events' is specified so the power analyses for minimum number of",
      " events will be ignored.\n",
      "    The minimum number of events being used is ",
      min_events, "."
    )
  }

  if (!is.na(min_events) & !meet_min_events) {
    message(
      "'min_events' is specified but 'meet_min_events' is FALSE.\n",
      "'min_events' will be ignored and ",
      "no minimum number of events will be set."
    )
  }

  if (!is.null(cl)) {
    stopifnot(inherits(cl, c("SOCKcluster", "cluster")))
  }
}


#' Updates the \code{rename_vector} from user as necessary before being used
#' with \code{dplyr::rename()}.
#'
#' @param rename_vector user-specified named vector for renaming cutpoint
#' methods.
#'
#' @return Returns (updated) named vector.
#' @noRd
update_rename_vector <- function(rename_vector) {
  default_rename_vector <- get_inbuilt_cutpoint_methods()
  names(default_rename_vector) <- gsub("_", " ", default_rename_vector)
  if (missing(rename_vector)) {
    rename_vector <- default_rename_vector
  } else {
    new_rename_vector <- default_rename_vector

    # update the new rename vector for the user-defined rename vector
    # and match to method names with underscores
    if (any(rename_vector %in% new_rename_vector)) {
      matched_values <- rename_vector[rename_vector %in% new_rename_vector]
      for (i in seq_along(matched_values)) {
        names(new_rename_vector)[new_rename_vector == matched_values[i]] <-
          names(matched_values)[i]
      }
    }

    # update the new rename vector for the user-defined rename vector
    # and match to method names with underscores replaced by spaces
    matched_values <- rename_vector[rename_vector %in% names(new_rename_vector)]
    if (any(rename_vector %in% names(new_rename_vector))) {
      for (i in seq_along(matched_values)) {
        names(new_rename_vector)[
          names(new_rename_vector) == matched_values[i]
        ] <- names(matched_values)[i]
      }
    }
    rename_vector <- new_rename_vector
  }
  return(rename_vector)
}


#' Returns a logical vector for near-equal matches for a vector to a value.
#'
#' @param vec Vector of values.
#' @param val Single value for which a (near) match in the `vec` is being
#' searched.
#'
#' @srrstats {G3.0} Near equality function (below) is used for matching
#' arguments (from `autoplot()`) to arguments (`constants`).
#'
#' @return Returns a logical vector the same length as the input vector.
#' @noRd
approx_match <- function(vec, val) {
  # find indices of vec which match val
  # use this approach for matching constants to simulation inputs as simulations
  # input vectors can be passed from seq() and there are precision issues with
  # matching values to those vectors.
  # all.equal() performs a test for 'near equality' so doesn't have this issue.
  unlist(lapply(vec, function(x) isTRUE(all.equal(x, val))))
}
