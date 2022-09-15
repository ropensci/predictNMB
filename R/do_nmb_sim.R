#' Do the predictNMB simulation, evaluating the net monetary benefit of the simulated model.
#'
#' @param sample_size Sample size of training set. If missing, a sample size calculation will be performed and the calculated size will be used.
#' @param n_sims Number of simulations to run.
#' @param n_valid Sample size for evaluation set.
#' @param sim_auc Simulated model discrimination (AUC).
#' @param event_rate Simulated event rate of the binary outcome being predicted. Also known as prevalence.
#' @param cutpoint_methods A value or vector of cutpoint methods to include. Defaults to use the inbuilt methods. \cr Methods:
#' \itemize{
#'  \item{"all" = treat all patients}
#'  \item{"none" = treat no patients}
#'  \item{"value_optimising" = select the cutpoint that maximises NMB}
#'  \item{"youden" = select cutpoint based on the Youden index, also known as the J-index (sensitivity + specificity - 1)}
#'  \item{"cost_minimising" = select the cutpoint that minimises expected value of costs}
#'  \item{"prod_sens_spec" = product of sensitivity and specificity (sensitivity \* specificity)}
#'  \item{"roc01" = selects the closest threshold to the (0,1) point on the ROC curve}
#' }
#' @param fx_nmb_training Function that returns named vector of NMB assigned to classifications use for obtaining cutpoint on training set. See `details` for more information.
#' @param fx_nmb_evaluation Function that returns named vector of NMB assigned to classifications use for obtaining cutpoint on evaluation set. See `details` for more information.
#' @param meet_min_events Whether or not to incrementally add samples until the expected number of events (\code{sample_size * event_rate}) is met. (Applies to sampling of training data only.)
#' @param min_events The minimum number of events to include in the training sample. If less than this number are included in sample of size \code{sample_size}, additional samples are added until the min_events is met. The default (\code{NA}) will use the expected value given the \code{event_rate} and the \code{sample_size}.
#' @param cl A cluster made using \code{parallel::makeCluster()}. If a cluster is provided, the simulation will be done in parallel.
#'
#' @details
#' The parameters fx_nmb_training and fx_nmb_evaluation require a vector of NMB values to evaluate at each simulation.
#' By assigning a value to each component of a list, the \code{do_nmb_sim()} function can pass these values to the cutpoint calculation. \cr \cr
#' For example, a confusion matrix with sample values for each cell would be: \cr
#' \code{get_nmb <- function() c("True positive" = -3, "True negative" = 0, "False positive" = -1, "False negative" = -4)}
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
                       fx_nmb_training, fx_nmb_evaluation, meet_min_events = TRUE,
                       min_events = NA, cl = NULL) {
  validate_inputs(
    sample_size,
    n_sims,
    n_valid,
    sim_auc,
    event_rate,
    cutpoint_methods,
    fx_nmb_training,
    fx_nmb_evaluation,
    meet_min_events,
    min_events,
    cl
  )

  if (missing(sample_size)) {
    sample_size <- NA
  }

  sample_size_calc <- do_sample_size_calc(
    cstatistic = sim_auc,
    prevalence = event_rate,
    sample_size = sample_size,
    min_events = min_events,
    meet_min_events = meet_min_events
  )

  sample_size <- sample_size_calc$sample_size
  min_events <- sample_size_calc$min_events

  stopifnot(min_events < sample_size)

  # do iterations
  f_iteration_wrapper <- function(...) {
    do_nmb_iteration(
      sim_auc = sim_auc,
      sample_size = sample_size,
      n_valid = n_valid,
      event_rate = event_rate,
      meet_min_events = meet_min_events,
      min_events = min_events,
      cutpoint_methods = cutpoint_methods,
      fx_nmb_training = fx_nmb_training,
      fx_nmb_evaluation = fx_nmb_evaluation
    )
  }

  if (is.null(cl)) {
    iterations <- lapply(seq_len(n_sims), f_iteration_wrapper)
  } else {
    parallel::clusterExport(cl, envir = environment(), {
      c(
        "f_iteration_wrapper",
        "do_nmb_iteration",
        "fx_nmb_training",
        "fx_nmb_evaluation",
        "get_sample",
        "get_thresholds",
        "evaluate_cutpoint"
      )
    })

    if (any(!cutpoint_methods %in% get_inbuilt_cutpoint(return_all_methods = TRUE))) {
      user_defined_methods <- cutpoint_methods[!cutpoint_methods %in% get_inbuilt_cutpoint(return_all_methods = TRUE)]
      if (any(!user_defined_methods %in% ls(envir = globalenv()))) {
        undefined_methods <- user_defined_methods[!user_defined_methods %in% ls(envir = globalenv())]
        stop("You've included functions in in 'cutpoint_methods' which are neither inbuilt cutpoint methods or defined in your global environment:\n", paste0(undefined_methods, collapse = ","))
      }

      parallel::clusterExport(cl, envir = globalenv(), {
        user_defined_methods
      })
    }

    iterations <- parallel::parLapply(cl = cl, seq_len(n_sims), f_iteration_wrapper)
  }

  df_result <- do.call("rbind", lapply(iterations, "[[", "results"))
  df_thresholds <- do.call("rbind", lapply(iterations, "[[", "thresholds"))


  df_result <- as.data.frame.matrix(df_result)
  df_thresholds <- as.data.frame.matrix(df_thresholds)
  rownames(df_thresholds) <- NULL

  df_result <- cbind(n_sim = seq_len(nrow(df_result)), df_result)
  df_thresholds <- cbind(n_sim = seq_len(nrow(df_thresholds)), df_thresholds)

  res <- list(
    df_result = df_result,
    df_thresholds = df_thresholds,
    meta_data = list(
      sample_size = sample_size,
      min_events = min_events,
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


do_nmb_iteration <- function(sim_auc,
                             sample_size,
                             n_valid,
                             event_rate,
                             meet_min_events,
                             min_events,
                             cutpoint_methods,
                             fx_nmb_training,
                             fx_nmb_evaluation) {
  train_sample <- get_sample(
    auc = sim_auc,
    n_samples = sample_size,
    prevalence = event_rate,
    min_events = ifelse(meet_min_events, min_events, 0)
  )

  valid_sample <- get_sample(
    auc = sim_auc,
    n_samples = n_valid,
    prevalence = event_rate,
    min_events = 0
  )

  model <- suppressWarnings(stats::glm(actual ~ x, data = train_sample, family = stats::binomial()))

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

  return(
    list(
      results = t(unlist(lapply(thresholds, cost_threshold))),
      thresholds = unlist(thresholds)
    )
  )
}


#' @export
print.predictNMBsim <- function(x, digits = 2, ...) {
  cat("predictNMB object\n")

  cat(paste0("\nTraining data sample size: "), x$meta_data$sample_size)
  cat(paste0("\nMinimum number of events in training sample: "), x$meta_data$min_events)
  cat(paste0("\nEvaluation data sample size: "), x$meta_data$n_valid)
  cat(paste0("\nNumber of simulations: "), x$meta_data$n_sims)
  cat(paste0("\nSimulated AUC: "), x$meta_data$sim_auc)
  cat(paste0("\nSimulated event rate: "), x$meta_data$event_rate)
}
