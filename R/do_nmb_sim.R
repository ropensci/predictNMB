#' Do the predictNMB simulation, evaluating the net monetary benefit of the simulated model.
#'
#' @param sample_size Sample size of training set. If missing, a sample size calculation will be performed and the calculated size will be used.
#' @param n_sims Number of simulations to run.
#' @param n_valid Sample size for evaluation set.
#' @param sim_auc Simulated model discrimination (AUC).
#' @param event_rate Simulated event rate of the binary outcome being predicted.
#' @param cutpoint_methods cutpoint methods to include. Defaults to use the inbuilt methods.
#' @param fx_nmb_training Function that returns named vector of NMB assigned to classifications use for obtaining cutpoint on training set
#' @param fx_nmb_evaluation Function that returns named vector of NMB assigned to classifications use for obtaining cutpoint on evaluation set
#' @param meet_min_events Whether or not to incrementally add samples until the expected number of events (\code{sample_size * event_rate}) is met. (Applies to sampling of training data only.)
#'
#' @return predictNMBsim
#' @export
#'
#' @examples
#'
#' get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#' do_nmb_sim(
#'   n_sims = 50, n_valid = 10000, sim_auc = 0.7, event_rate = 0.1,
#'   fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
#' )
do_nmb_sim <- function(sample_size, n_sims, n_valid, sim_auc, event_rate,
                       cutpoint_methods = get_inbuilt_cutpoint(return_all_methods = TRUE),
                       fx_nmb_training, fx_nmb_evaluation, meet_min_events = TRUE) {
  if (missing(sample_size)) {
    sample_size <- NA
  }
  if (is.na(sample_size)) {
    pmsamp <- pmsampsize::pmsampsize(
      type = "b",
      cstatistic = sim_auc,
      parameters = 1,
      prevalence = event_rate
    )
    sample_size <- pmsamp$sample_size
    min_events <- pmsamp$events
  } else {
    min_events <- round(sample_size * event_rate)
  }

  for (i in 1:n_sims) {
    train_sample <- get_sample(auc = sim_auc, n_samples = sample_size, prevalence = event_rate, min_events = ifelse(meet_min_events, min_events, 0))
    valid_sample <- get_sample(auc = sim_auc, n_samples = n_valid, prevalence = event_rate, min_events = 0)

    model <- stats::glm(actual ~ x, data = train_sample, family = stats::binomial())

    train_sample$predicted <- stats::predict(model, type = "response")
    valid_sample$predicted <- stats::predict(model, type = "response", newdata = valid_sample)

    training_value_vector <- fx_nmb_training()

    thresholds <- get_thresholds(
      predicted = train_sample$predicted,
      actual = train_sample$actual,
      nmb = training_value_vector,
      cutpoint_methods = cutpoint_methods
    )

    evaluation_value_vector <- fx_nmb_evaluation()

    cost_threshold <- function(pt) {
      evaluate_cutpoint(
        predicted = valid_sample$predicted,
        actual = valid_sample$actual,
        pt = pt,
        nmb = evaluation_value_vector
      )
    }

    results_i <- t(unlist(lapply(thresholds, cost_threshold)))

    thresholds_i <- unlist(thresholds)
    if (i == 1) {
      df_result <- results_i
      df_thresholds <- thresholds_i
    } else {
      df_result <- rbind(df_result, results_i)
      df_thresholds <- rbind(df_thresholds, thresholds_i)
    }
  } # end simulation loop

  df_result <- as.data.frame.matrix(df_result)
  df_thresholds <- as.data.frame.matrix(df_thresholds)
  rownames(df_thresholds) <- NULL

  df_result <- cbind(n_sim = 1:nrow(df_result), df_result)
  df_thresholds <- cbind(n_sim = 1:nrow(df_thresholds), df_thresholds)

  res <- list(
    df_result = df_result,
    df_thresholds = df_thresholds,
    meta_data = list(
      sample_size = sample_size,
      n_sims = n_sims,
      n_valid = n_valid,
      sim_auc = sim_auc,
      event_rate = event_rate,
      fx_nmb_training = fx_nmb_training,
      fx_nmb_evaluation = fx_nmb_evaluation
    )
  )
  class(res) <- "predictNMBsim"
  res
}


#' @export
print.predictNMBsim <- function(x, digits = 2, ...) {
  cat("predictNMB object\n")

  cat(paste0("\nTraining data sample size: "), x$meta_data$sample_size)
  cat(paste0("\nEvaluation data sample size: "), x$meta_data$n_valid)
  cat(paste0("\nNumber of simulations: "), x$meta_data$n_sims)
  cat(paste0("\nSimulated AUC: "), x$meta_data$sim_auc)
  cat(paste0("\nSimulated event rate: "), x$meta_data$event_rate)
}
