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
