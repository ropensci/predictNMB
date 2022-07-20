#' Title
#'
#' @param sample_size Sample size of training set. If missing, a sample size calculation will be performed and the calculated size will be used.
#' @param n_sims Number of simulations to run.
#' @param n_valid Sample size for evaluation set.
#' @param sim_auc Simulated model discrimination (AUC).
#' @param event_rate Simulated event rate of the binary outcome being predicted.
#' @param fx_nmb_training Function that returns named vector of NMB assigned to classifications use for obtaining cutpoint on training set
#' @param fx_nmb_evaluation Function that returns named vector of NMB assigned to classifications use for obtaining cutpoint on evaluation set
#'
#' @return predictNMBsim
#' @export
#'
#' @examples
#'
#' get_nmb <- function() c("TP" = 1, "TN" = 2, "FP" = 3, "FN" = 4)
#' do_nmb_sim(
#'   n_sims = 50, n_valid = 10000, sim_auc = 0.7, event_rate = 0.1,
#'   fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
#' )
do_nmb_sim <- function(sample_size, n_sims, n_valid, sim_auc, event_rate,
                       fx_nmb_training, fx_nmb_evaluation) {
  if (missing(sample_size)) {
    pmsamp <- pmsampsize::pmsampsize(
      type = "b",
      cstatistic = sim_auc,
      parameters = 1,
      prevalence = event_rate
    )
    sample_size <- pmsamp$sample_size
    min_events <- pmsamp$events
    check_events <- TRUE
  } else {
    check_events <- FALSE
  }

  i <- 0
  while (i < n_sims) {
    train_sample <- get_sample(auc = sim_auc, n_samples = sample_size, prevalence = event_rate)
    valid_sample <- get_sample(auc = sim_auc, n_samples = n_valid, prevalence = event_rate)
    if (length(unique(train_sample$actual)) != 2 | length(unique(valid_sample$actual)) != 2) {
      next
    }
    if (check_events) {
      if (sum(train_sample$actual) < min_events) {
        next
      }
    }
    i <- i + 1
    model <- stats::glm(actual ~ x, data = train_sample, family = stats::binomial())

    train_sample$predicted <- stats::predict(model, type = "response")
    valid_sample$predicted <- stats::predict(model, type = "response", newdata = valid_sample)

    training_value_vector <- fx_nmb_training()

    thresholds <- get_thresholds(
      predicted = train_sample$predicted,
      actual = train_sample$actual,
      nmb = training_value_vector
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

    results_i <-
      lapply(thresholds, cost_threshold) |>
      unlist() |>
      t()

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
