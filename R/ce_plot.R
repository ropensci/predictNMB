#' Create a cost-effectiveness plot.
#'
#' @param object A \code{predictNMBsim} object.
#' @param ref_col Which cutpoint method to use as the reference strategy
#' when calculating the incremental net monetary benefit. Often sensible to use
#' a "all" or "none" approach for this.
#' @param wtp A \code{numeric}. The willingness to pay value used to create a
#' WTP threshold line on the plot (if \code{show_wtp = TRUE}). Defaults to the
#' wtp stored in the \code{predictNMBsim} object.
#' @param show_wtp A \code{logical}. Whether or not to show the willingness to
#' pay threshold.
#' @param methods_order The order (within the legend) to display the
#' cutpoint methods.
#' @param rename_vector A named vector for renaming the methods in the summary.
#' The values of the vector are the default names and the names given are the
#' desired names in the output.
#' @param shape The \code{shape} used for \code{ggplot2::geom_point()}.
#' Defaults to 21 (hollow circles). If \code{shape = "method"} or
#' \code{shape = "cost-effective"} (only applicable when \code{show_wtp = TRUE})
#' , then the shape will be mapped to that aesthetic.
#' @param wtp_linetype The \code{linetype} used for \code{ggplot2::geom_abline()}
#' when making the WTP. Defaults to \code{"dashed"}.
#' @param add_prop_ce Whether to append the proportion of simulations for that
#' method which were cost-effective (beneath the WTP threshold)
#' to their labels in the legend. Only applicable when \code{show_wtp = TRUE}.
#' @param ... Additional (unused) arguments.
#'
#' @details
#' This plot method works with \code{predictNMBsim} objects that are created
#' using \code{do_nmb_sim()}. Can be used to visualise the simulations on a
#' cost-effectiveness plot (costs vs effectiveness)
#'
#' @return Returns a \code{ggplot} object.
#' @export
#'
#' @examples
#' \donttest{
#' get_nmb_evaluation <- get_nmb_sampler(
#'   qalys_lost = function() rnorm(1, 0.33, 0.03),
#'   wtp = 28000,
#'   high_risk_group_treatment_effect = function() exp(rnorm(n = 1, mean = log(0.58), sd = 0.43)),
#'   high_risk_group_treatment_cost = function() rnorm(n = 1, mean = 161, sd = 49)
#' )
#'
#' sim_obj <- do_nmb_sim(
#'   sample_size = 200, n_sims = 50, n_valid = 10000, sim_auc = 0.7,
#'   event_rate = 0.1, fx_nmb_training = get_nmb_evaluation, fx_nmb_evaluation = get_nmb_evaluation
#' )
#'
#' ce_plot(sim_obj, ref_col = "all")
#' }
ce_plot <- function(object,
                    ref_col,
                    wtp,
                    show_wtp = TRUE,
                    methods_order = NULL,
                    rename_vector,
                    shape = 21,
                    wtp_linetype = "dashed",
                    add_prop_ce = FALSE,
                    ...) {
  UseMethod("ce_plot")
}


#' Create a cost-effectiveness plot.
#'
#' @param object A \code{predictNMBsim} object.
#' @param ref_col Which cutpoint method to use as the reference strategy
#' when calculating the incremental net monetary benefit. Often sensible to use
#' a "all" or "none" approach for this.
#' @param wtp A \code{numeric}. The willingness to pay value used to create a
#' WTP threshold line on the plot (if \code{show_wtp = TRUE}). Defaults to the
#' wtp stored in the \code{predictNMBsim} object.
#' @param show_wtp A \code{logical}. Whether or not to show the willingness to
#' pay threshold.
#' @param methods_order The order (within the legend) to display the
#' cutpoint methods.
#' @param rename_vector A named vector for renaming the methods in the summary.
#' The values of the vector are the default names and the names given are the
#' desired names in the output.
#' @param shape The \code{shape} used for \code{ggplot2::geom_point()}.
#' Defaults to 21 (hollow circles). If \code{shape = "method"} or
#' \code{shape = "cost-effective"} (only applicable when \code{show_wtp = TRUE})
#' , then the shape will be mapped to that aesthetic.
#' @param wtp_linetype The \code{linetype} used for \code{ggplot2::geom_abline()}
#' when making the WTP. Defaults to \code{"dashed"}.
#' @param add_prop_ce Whether to append the proportion of simulations for that
#' method which were cost-effective (beneath the WTP threshold)
#' to their labels in the legend. Only applicable when \code{show_wtp = TRUE}.
#' @param ... Additional (unused) arguments.
#'
#' @details
#' This plot method works with \code{predictNMBsim} objects that are created
#' using \code{do_nmb_sim()}. Can be used to visualise the simulations on a
#' cost-effectiveness plot (costs vs effectiveness)
#'
#' @return Returns a \code{ggplot} object.
#' @export
#'
#' @examples
#' \donttest{
#' get_nmb_evaluation <- get_nmb_sampler(
#'   qalys_lost = function() rnorm(1, 0.33, 0.03),
#'   wtp = 28000,
#'   high_risk_group_treatment_effect = function() exp(rnorm(n = 1, mean = log(0.58), sd = 0.43)),
#'   high_risk_group_treatment_cost = function() rnorm(n = 1, mean = 161, sd = 49)
#' )
#'
#' sim_obj <- do_nmb_sim(
#'   sample_size = 200, n_sims = 50, n_valid = 10000, sim_auc = 0.7,
#'   event_rate = 0.1, fx_nmb_training = get_nmb_evaluation, fx_nmb_evaluation = get_nmb_evaluation
#' )
#'
#' ce_plot(sim_obj, ref_col = "all")
#' }
ce_plot.predictNMBsim <- function(object,
                                  ref_col,
                                  wtp,
                                  show_wtp = TRUE,
                                  methods_order = NULL,
                                  rename_vector,
                                  shape = 21,
                                  wtp_linetype = "dashed",
                                  add_prop_ce = FALSE,
                                  ...) {
  if (missing(ref_col)) {
    stop("'ref_col' must be specified for creating a cost-effectiveness plot.")
  }

  if (!object$meta_data$track_qalys) {
    stop(
      "This predictNMBsim object did not track the QALYs and costs at each",
      " simulation so a cost-effectiveness plot cannot be made.",
      " This is likely because the functions used for ",
      "'fx_nmb_training' and 'fx_nmb_evaluation' were either not made using",
      " 'get_nmb_sampler()' or, if they were, they didn't use 'qalys_lost'",
      " and 'wtp'."
    )
  }

  p_data_costs <- get_plot_data(
    x = object,
    what = "costs",
    methods_order = methods_order,
    rename_vector = rename_vector,
    inb_ref_col = ref_col
  )

  p_data_qalys <- get_plot_data(
    x = object,
    what = "qalys",
    methods_order = methods_order,
    rename_vector = rename_vector,
    inb_ref_col = ref_col
  )

  p_data <- rbind(
    tibble::add_column(p_data_costs, type = "costs"),
    tibble::add_column(p_data_qalys, type = "qalys")
  ) %>%
    dplyr::select(-c(percentile, in_interval)) %>%
    tidyr::pivot_wider(names_from = "type", values_from = "value")

  legend_aesthetics <- list(col = "Cutpoint Methods")

  if (show_wtp) {
    if (!is.function(attr(object$meta_data$fx_nmb_evaluation, "wtp"))) {
      if (missing(wtp)) {
        wtp <- attr(object$meta_data$fx_nmb_evaluation, "wtp")
      } else {
        assertthat::is.number(wtp)
        if (!approx_match(wtp, attr(object$meta_data$fx_nmb_evaluation, "wtp"))) {
          message(paste0(
            "wtp is stored in predictNMBsim object (wtp = ",
            attr(object$meta_data$fx_nmb_evaluation, "wtp"), ")\n\n",
            "but has also been specified (wtp = ", wtp, ")\n\n",
            "Using a different WTP for evaluating the simulations (NMB) and ",
            "within the cost-effectiveness plot may lead to misinterpretation!"
          ))
        }
      }
    } else {
      message(
        "The wtp is stored in the predictNMBsim object as a function,",
        " not a fixed value.\n\n"
      )
      if (!missing(wtp)) {
        message(
          "Using the specified wtp value to draw the WTP line",
          " and ignoring the stored wtp.\n\n"
        )
        assertthat::is.number(wtp)
      } else {
        wtp <- mean(unlist(replicate(
          10000,
          attr(object$meta_data$fx_nmb_evaluation, "wtp")(),
          simplify = F
        )))
        message(paste0(
          "No wtp was specified. Using the mean of 10000 samples of the wtp",
          " function within the predictNMBsim object (wtp = ",
          round(wtp, getOption("digits")), ")"
        ))
      }
    }

    if (add_prop_ce | shape == "cost-effective") {
      p_data <- p_data %>%
        dplyr::mutate(ce = costs < wtp * qalys)

      if (add_prop_ce) {
        p_data <- p_data %>%
          dplyr::group_by(name) %>%
          dplyr::mutate(name = paste0(name, " (", scales::percent(mean(ce)), ")"))

        legend_aesthetics['col'] <- "Cutpoint Methods (cost-effective %)"
      }
    }
  }

  match_shape_and_col_legend <- FALSE
  if (shape == "method") {
    p <- p_data %>%
      ggplot2::ggplot(ggplot2::aes(qalys, costs, col = name, shape = name)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_vline(xintercept = 0)

    if (length(levels(p_data$name)) > 6) {
      p <- p +
        ggplot2::scale_shape_manual(values = 1:length(levels(p_data$name)))
    }

    match_shape_and_col_legend <- TRUE

  } else if (shape == "cost-effective") {
    stopifnot(show_wtp)
    p <- p_data %>%
      dplyr::mutate(ce = costs < wtp * qalys) %>%
      ggplot2::ggplot(ggplot2::aes(qalys, costs, col = name, shape = ce)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_vline(xintercept = 0)

    legend_aesthetics['shape'] <- "Cost-effective"

  } else {
    p <- p_data %>%
      ggplot2::ggplot(ggplot2::aes(qalys, costs, col = name)) +
      ggplot2::geom_point(shape = shape) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_vline(xintercept = 0)
  }

  if (show_wtp) {
    p <- p +
      ggplot2::geom_abline(
        intercept = 0,
        slope = wtp,
        linetype = wtp_linetype
      )
  }

  if (match_shape_and_col_legend) {
    p +
      ggplot2::labs(
        x = "Incremental Effectiveness (Quality-Adjusted Life Years)",
        y = "Incremental Costs ($)",
        col = legend_aesthetics['col'],
        shape = legend_aesthetics['col']
      )
  } else {
    p +
      ggplot2::labs(
        x = "Incremental Effectiveness (Quality-Adjusted Life Years)",
        y = "Incremental Costs ($)",
        col = legend_aesthetics['col'],
        shape = legend_aesthetics['shape']
      )
  }

}
