#' Do the predictNMB simulation, evaluating the net monetary benefit of the
#' simulated model.
#'
#' @param sample_size Sample size of training set. If missing, a sample size
#' calculation will be performed and the calculated size will be used.
#' @param n_sims Number of simulations to run.
#' @param n_valid Sample size for evaluation set.
#' @param sim_auc Simulated model discrimination (AUC).
#' @param event_rate Simulated event rate of the binary outcome being predicted.
#' Also known as prevalence.
#' @param cutpoint_methods A value or vector of cutpoint methods to include.
#' Defaults to use the inbuilt methods:
#' \itemize{
#'  \item{"all" = treat all patients (cutpoint = 0)}
#'  \item{"none" = treat no patients (cutpoint = 1)}
#'  \item{"value_optimising" = select the cutpoint that maximises NMB}
#'  \item{"youden" = select cutpoint based on the Youden index, also known as
#'  the J-index (sensitivity + specificity - 1)}
#'  \item{"cost_minimising" = select the cutpoint that minimises expected value
#'  of costs}
#'  \item{"prod_sens_spec" = product of sensitivity and specificity
#'  (sensitivity * specificity)}
#'  \item{"roc01" = selects the closest threshold to the (0,1) point on the ROC
#'  curve}
#' }
#' User-defined cutpoint methods can be used by passing the name of a function
#' that takes the following arguments:
#' \itemize{
#'  \item{\code{predicted} (predicted probabilities)}
#'  \item{\code{actual} (the actual, binary outcome)}
#'  \item{\code{nmb} (a named vector containing NMB values assigned to each
#'  predicted class (i.e. c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)))}
#' }
#' See \code{?get_thresholds} for an example of a user-defined cutpoint
#' function.
#' @param fx_nmb_training Function that returns a named vector of NMB assigned
#' to classifications used for obtaining cutpoint on training set.
#' @param fx_nmb_evaluation Function that returns a named vector of NMB
#' assigned to classifications used for obtaining cutpoint on evaluation set.
#' @param meet_min_events Whether or not to incrementally add samples until the
#' expected number of events (\code{sample_size * event_rate}) is met.
#' (Applies to sampling of training data only.)
#' @param min_events The minimum number of events to include in the training
#' sample. If less than this number are included in sample of size
#' \code{sample_size}, additional samples are added until the \code{min_events} is met.
#' The default (\code{NA}) will use the expected value given the
#' \code{event_rate} and the \code{sample_size}.
#' @param show_progress Logical. Whether to display a progress bar.
#' @param cl A cluster made using \code{parallel::makeCluster()}.
#' If a cluster is provided, the simulation will be done in parallel.
#'
#' @details
#' This function runs a simulation for a given set of inputs that represent a
#' healthcare setting using model-guided interventions. \cr\cr
#' The arguments \code{fx_nmb_training} and \code{fx_nmb_evaluation}
#' should be functions that capture the treatment being used, its costs and
#' effectiveness, and the costs of the outcome being treated/prevented. \cr\cr
#' Both of these are functions that return a named vector of NMB values when
#' called and are used for obtaining and evaluating cutpoints, respectively.
#' For example, the following function returns the appropriately named vector.
#' \cr\cr
#' \code{get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)}
#'
#' There is a helper function, \code{get_nmb_sampler()}, to help you
#' create these.
#'
#' @srrstats {G2.1a} Data types for all inputs are documented.
#'
#' @return Returns a \code{predictNMBsim} object.
#' @export
#'
#' @examples
#'
#' get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#' do_nmb_sim(
#'   sample_size = 200, n_sims = 50, n_valid = 10000, sim_auc = 0.7,
#'   event_rate = 0.1, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
#' )
do_nmb_sim <- function(sample_size,
                       n_sims,
                       n_valid,
                       sim_auc,
                       event_rate,
                       cutpoint_methods = get_inbuilt_cutpoint_methods(),
                       fx_nmb_training,
                       fx_nmb_evaluation,
                       meet_min_events = TRUE,
                       min_events = NA,
                       show_progress = FALSE,
                       cl = NULL) {
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

  df_nmb_training <- do.call(
    "rbind",
    replicate(n_sims, fx_nmb_training(), simplify = FALSE)
  )

  df_nmb_evaluation <- do.call(
    "rbind",
    replicate(n_sims, fx_nmb_evaluation(), simplify = FALSE)
  )

  # do iterations
  f_iteration_wrapper <- function(iter) {
    do_nmb_iteration(
      iter = iter,
      sim_auc = sim_auc,
      sample_size = sample_size,
      n_valid = n_valid,
      event_rate = event_rate,
      meet_min_events = meet_min_events,
      min_events = min_events,
      cutpoint_methods = cutpoint_methods,
      df_nmb_training = df_nmb_training,
      df_nmb_evaluation = df_nmb_evaluation
    )
  }

  if (is.null(cl)) {
    if (show_progress) {
      iterations <- pbapply::pblapply(seq_len(n_sims), f_iteration_wrapper)
    } else {
      iterations <- lapply(seq_len(n_sims), f_iteration_wrapper)
    }
  } else {
    parallel::clusterExport(cl, envir = environment(), {
      c(
        "f_iteration_wrapper",
        "do_nmb_iteration",
        "df_nmb_training",
        "df_nmb_evaluation",
        "get_sample",
        "get_thresholds",
        "evaluate_cutpoint",
        "get_inbuilt_cutpoint",
        "get_inbuilt_cutpoint_methods",
        "total_nmb",
        "roc_iu"
      )
    })
    inbuilt_methods <- get_inbuilt_cutpoint_methods()
    if (any(!cutpoint_methods %in% inbuilt_methods)) {
      user_defined_methods <- cutpoint_methods[
        !cutpoint_methods %in% inbuilt_methods
      ]
      if (any(!user_defined_methods %in% ls(envir = globalenv()))) {
        undefined_methods <- user_defined_methods[
          !user_defined_methods %in% ls(envir = globalenv())
        ]
        stop(
          "You've included functions in in 'cutpoint_methods' which are ",
          "neither inbuilt cutpoint methods or defined in your global ",
          "environment:\n", paste0(undefined_methods, collapse = ",")
        )
      }

      parallel::clusterExport(cl, envir = globalenv(), {
        user_defined_methods
      })
    }

    if (show_progress) {
      iterations <- pbapply::pblapply(
        cl = cl,
        X = seq_len(n_sims),
        FUN = f_iteration_wrapper
      )
    } else {
      iterations <- parallel::parLapply(
        cl = cl,
        X = seq_len(n_sims),
        fun = f_iteration_wrapper
      )
    }
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



#' Performs a single simulation: sample training and validation data, fit
#' model, evaluate model on new data according to model and cutpoints selected
#' with training data.
#'
#' @param x A grid of inputs, including details required to perform sample
#' size calculations.
#'
#' @param iter Integer. Iteration number.
#' @param sim_auc Desired AUC to be simulated.
#' @param sample_size Desired sample size for training set.
#' @param n_valid Sample size for validation set.
#' @param event_rate Prevalence of event.
#' @param meet_min_events Whether or not to add additional samples to training
#' set so that a minimum number is met.
#' @param min_events The minimum number of events to meet if
#' \code{meet_min_events=TRUE}.
#' @param cutpoint_methods The cutpoint methods apply and evaluate.
#' @param df_nmb_training A named vector of NMB values for each possible
#' classification (TN, TP, FN, FP). Used for estimating cutpoints (only for
#' methods that use this information).
#' @param df_nmb_evaluation A named vector of NMB values for each possible
#' classification (TN, TP, FN, FP). Used for evaluating selected cutpoints.
#'
#' @return \code{list} with results (NMB values) and cutpoints for each
#' cutpoint method.
#'
#' @noRd
do_nmb_iteration <- function(iter,
                             sim_auc,
                             sample_size,
                             n_valid,
                             event_rate,
                             meet_min_events,
                             min_events,
                             cutpoint_methods,
                             df_nmb_training,
                             df_nmb_evaluation) {
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

  model <- suppressWarnings(
    stats::glm(actual ~ x, data = train_sample, family = stats::binomial())
  )

  train_sample$predicted <- stats::predict(model, type = "response")
  valid_sample$predicted <- stats::predict(
    model,
    type = "response",
    newdata = valid_sample
  )

  training_value_vector <- unlist(df_nmb_training[iter, ])

  thresholds <- get_thresholds(
    predicted = train_sample$predicted,
    actual = train_sample$actual,
    nmb = training_value_vector,
    cutpoint_methods = cutpoint_methods
  )

  evaluation_value_vector <- unlist(df_nmb_evaluation[iter, ])

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


#' Print a summary of a predictNMBsim object
#'
#' @export
#' @param x A \code{predictNMBsim} object.
#' @param ... Optional, ignored arguments.
#' @return `print(x)` returns `x` invisibly.
#'
#' @examples
#' if (FALSE) {
#'   get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#'   sim_obj <- do_nmb_sim(
#'     sample_size = 200, n_sims = 50, n_valid = 10000, sim_auc = 0.7,
#'     event_rate = 0.1, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
#'   )
#'   print(sim_obj)
#' }
print.predictNMBsim <- function(x, ...) {
  cat("predictNMB object\n")

  cat("\nTraining data sample size: ", x$meta_data$sample_size)
  cat("\nMinimum number of events in training sample: ", x$meta_data$min_events)
  cat("\nEvaluation data sample size: ", x$meta_data$n_valid)
  cat("\nNumber of simulations: ", x$meta_data$n_sims)
  cat("\nSimulated AUC: ", x$meta_data$sim_auc)
  cat("\nSimulated event rate: ", x$meta_data$event_rate)
  invisible(x)
}
