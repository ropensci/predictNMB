utils::globalVariables(
  c(
    "name", "value", "percentile", "n_sim", "m", "outcome", "count",
    "in_interval", "ymin", "ymax", "method", "small_grid_id"
  )
)

do_sample_size_calc <- function(cstatistic, prevalence, sample_size, min_events) {
  if (is.na(sample_size)) {
    pmsamp <- pmsampsize::pmsampsize(
      type = "b",
      cstatistic = cstatistic,
      parameters = 1,
      prevalence = prevalence
    )
    sample_size <- pmsamp$sample_size
    min_events <- round(pmsamp$events)
  } else if (is.na(min_events)) {
    min_events <- round(sample_size * prevalence)
  }

  list(
    sample_size = sample_size,
    min_events = min_events
  )
}

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

  if(!missing(sample_size)) {
    stopifnot(assertthat::is.count(sample_size))
  }

  stopifnot(assertthat::is.count(n_sims))
  stopifnot(assertthat::is.count(n_valid))
  stopifnot(assertthat::is.number(sim_auc))
  stopifnot(sim_auc > 0 | sim_auc < 1)
  stopifnot(assertthat::is.number(event_rate))
  stopifnot(event_rate > 0 | event_rate < 1)
  stopifnot(inherits(cutpoint_methods, "character"))
  stopifnot(is.logical(meet_min_events))

  if(!missing(fx_nmb_training)) {
    stopifnot(inherits(fx_nmb_training, "function"))
  }

  stopifnot(inherits(fx_nmb_evaluation, "function"))

  if(!is.na(min_events)) {
    stopifnot(assertthat::is.count(min_events))
  }

  if(!is.null(cl)) {
    stopifnot(inherits(cl, c("SOCKcluster", "cluster")))
  }
}
