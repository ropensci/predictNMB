#' @srrstats {EA5.0, EA5.0a, EA5.0b, EA5.1} \code{plot.predictNMBsim()} applies
#' two colours for the fill aesthetic by default (blue and grey) and these can
#' be controlled by the user by the \code{fill_cols} argument.
#' These colours were selected with appropriate interpretation of the interval
#' and accessibility in mind. All fonts and other colour schemes
#' (including for \code{plot.predictNMBscreen()}) use the defaults by
#' \code{ggplot} by default.
#' @srrstats {EA5.4, EA5.5} Units are included on all axes and usee
#' \code{ggplot2} to produce sensibly rounded values.
#'
#' Whether or not to use linewidth vs size argument in ggplot2.
#' @return Returns logical - whether or not to use linewidth based on ggplot2
#' version.
#' @noRd
use_linewidth <- function() {
  # if the available ggplot2 version is ahead of 3.4.0, then use `linewidth`
  # argument, otherwise will use size
  utils::compareVersion(
    as.character(utils::packageVersion("ggplot2")),
    "3.4.0"
  ) %in% c(0, 1)
}


#' Gather and wrangle appropriate data for plotting from simulation(s) object.
#'
#' @param x A \code{predictNMBsim} or \code{predictNMBscreen} object.
#' @param what What to plot: one of "nmb", "inb" or "cutpoints".
#' @param methods_order The order (left to right) to display the cutpoint
#' methods.
#' @param rename_vector A named vector for renaming the methods in the summary.
#' The values of the vector are the default names and the names given are the
#' desired names in the output.
#' @param inb_ref_col Which cutpoint method to use as the reference strategy
#' when calculating the incremental net monetary benefit. See \code{do_nmb_sim}
#' for more information.
#' @param ci The confidence level.
#'
#' @return Returns a \code{data.frame}.
#' @noRd
get_plot_data <- function(x,
                          what,
                          methods_order,
                          rename_vector,
                          inb_ref_col,
                          ci) {
  # select relevant data from predictNMBsim object
  if (what == "cutpoints") {
    data <- x$df_thresholds
  } else {
    data <- x$df_result
  }

  rename_vector <- update_rename_vector(rename_vector)
  data <- dplyr::rename(data, dplyr::any_of(rename_vector))

  if (what == "inb") {
    data <-
      data %>%
      dplyr::mutate(dplyr::across(
        !n_sim,
        function(x) x - !!rlang::sym(inb_ref_col)
      )) %>%
      dplyr::select(-dplyr::all_of(inb_ref_col))
  }

  # pivot data to long format
  pivoted_data <- tidyr::pivot_longer(data, !n_sim)

  if (is.null(methods_order)) {
    methods_order <- data %>%
      dplyr::select(-n_sim) %>%
      names()
  }
  pivoted_data$name <- factor(pivoted_data$name, levels = methods_order)
  pivoted_data <- dplyr::filter(pivoted_data, !is.na(name))

  # add label (in_interval) for whether the observation is within the interval
  add_interval(pivoted_data, ci = ci)
}


add_interval <- function(data, ci) {
  probs <- c((1 - ci) / 2, 1 - (1 - ci) / 2)

  data %>%
    dplyr::group_by(name) %>%
    dplyr::arrange(value) %>%
    dplyr::mutate(percentile = dplyr::row_number() / dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(in_interval = percentile > probs[1] & percentile < probs[2])
}


#' Create plots of from predictNMB simulations.
#'
#' @param x A \code{predictNMBsim} object.
#' @param what What to summarise: one of "nmb", "inb" or "cutpoints".
#' Defaults to "nmb".
#' @param inb_ref_col Which cutpoint method to use as the reference strategy
#' when calculating the incremental net monetary benefit.
#' See \code{do_nmb_sim} for more information.
#' @param ci The confidence level.
#' Defaults to 0.95(coloured area of distribution represents 95% CIs).
#' @param methods_order The order (left to right) to display the
#' cutpoint methods.
#' @param n_bins The number of bins used when constructing histograms.
#' Defaults to 40.
#' @param label_wrap_width The number of characters in facet labels at which
#' the label is wrapped. Default is 12.
#' @param fill_cols Vector containing the colours used for fill aesthetic of
#' histograms. The first colour represents the area outside of the confidence
#' region, second colour shows the confidence region. Defaults to
#' \code{c("grey50", "#ADD8E6")}.
#' @param median_line_size Size of line used to represent the median of
#' distribution. Defaults to 2.
#' @param median_line_alpha Alpha for line used to represent the median of
#' distribution. Defaults to 0.5.
#' @param median_line_col Colour of line used to represent the median of
#' distribution. Defaults to \code{"black"}.
#' @param rename_vector A named vector for renaming the methods in the summary.
#' The values of the vector are the default names and the names given are the
#' desired names in the output.
#' @param extra_theme Additional theme applied to plot. Defaults to some
#' tasteful changes to reduce clutter in the plot.
#' @param ... Additional (unused) arguments.
#'
#' @details
#' This plot method works with \code{predictNMBsim} objects that are created
#' using \code{do_nmb_sim()}. Can be used to visualise distributions from
#' simulations for different cutpoint methods.
#'
#' @srrstats {G2.4d} `as.factor()` used when necessary.
#'
#' @return Returns a \code{ggplot} object
#' @export
#'
#' @examples
#'
#' if (FALSE) {
#'   get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#'   sim_screen_obj <- screen_simulation_inputs(
#'     n_sims = 50, n_valid = 10000, sim_auc = seq(0.7, 0.9, 0.1),
#'     event_rate = c(0.1, 0.2, 0.3),
#'     fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
#'   )
#'
#'   plot(sim_screen_obj)
#'   plot(
#'     sim_screen_obj,
#'     x_axis_var = "event_rate",
#'     constants = c(sim_auc = 0.8),
#'     dodge_width = 0.02
#'   )
#' }
plot.predictNMBsim <- function(x,
                               what = c("nmb", "inb", "cutpoints"),
                               inb_ref_col = NA,
                               ci = 0.95,
                               methods_order = NULL,
                               n_bins = 40,
                               label_wrap_width = 12,
                               fill_cols = c("grey50", "#ADD8E6"),
                               median_line_size = 2,
                               median_line_alpha = 0.5,
                               median_line_col = "black",
                               rename_vector,
                               extra_theme = ggplot2::theme(
                                 panel.spacing = ggplot2::unit(0, "lines"),
                                 axis.ticks.x = ggplot2::element_blank(),
                                 axis.text.x = ggplot2::element_blank(),
                                 strip.background = ggplot2::element_rect(fill = "#f1f1f1"),
                                 panel.grid.minor = ggplot2::element_blank()
                               ),
                               ...) {
  p_data <- get_plot_data(
    x = x,
    what = what[1],
    methods_order = methods_order,
    rename_vector = rename_vector,
    inb_ref_col = inb_ref_col,
    ci = ci
  )

  df_agg <-
    p_data %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(m = stats::median(value))

  p <-
    p_data %>%
    ggplot2::ggplot(ggplot2::aes(value, fill = in_interval)) +
    ggplot2::geom_histogram(bins = n_bins) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(
      ~name,
      labeller = ggplot2::label_wrap_gen(width = label_wrap_width),
      nrow = 1
    ) +
    ggplot2::theme_bw() +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_fill_manual(values = c(NA, fill_cols))

  my_plot_innards <- ggplot2::ggplot_build(p)

  extracted_points <- tibble::tibble(
    outcome = my_plot_innards[["data"]][[1]][["x"]],
    count = my_plot_innards[["data"]][[1]][["y"]],
    in_interval = my_plot_innards[["data"]][[1]][["group"]] %>% as.factor(),
    method = my_plot_innards[["data"]][[1]][["PANEL"]] %>% as.character()
  )

  heights <-
    df_agg %>%
    tibble::rownames_to_column(var = "method_n") %>%
    dplyr::left_join(
      extracted_points,
      by = c("method_n" = "method"),
      multiple = "all"
    ) %>%
    dplyr::mutate(diff = abs(m - outcome)) %>%
    dplyr::group_by(name) %>%
    dplyr::arrange(diff) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()


  x_axis_title <- switch(what[1],
    "nmb" = "Net Monetary Benefit ($)",
    "inb" = "Incremental Net Monetary Benefit ($)",
    "cutpoints" = "Selected Cutpoint"
  )

  if (use_linewidth()) {
    segment_layer <- ggplot2::geom_segment(
      data = heights,
      ggplot2::aes(x = m, xend = m, y = 0, yend = count),
      linewidth = median_line_size,
      alpha = median_line_alpha,
      col = median_line_col
    )
  } else {
    segment_layer <- ggplot2::geom_segment(
      data = heights,
      ggplot2::aes(x = m, xend = m, y = 0, yend = count),
      size = median_line_size,
      alpha = median_line_alpha,
      col = median_line_col
    )
  }

  p <-
    p +
    segment_layer +
    ggplot2::labs(
      x = x_axis_title,
      y = ""
    )

  if (!is.null(extra_theme)) {
    p <- p + extra_theme
  }

  p
}

#' Create plots of from screened predictNMB simulations.
#'
#' @param x A \code{predictNMBscreen} object.
#' @param x_axis_var The desired screened factor to be displayed along the
#' x axis. For example, if the simulation screen was used with many values for
#' event rate, this could be "event_rate". Defaults to the first detected,
#' varied input.
#' @param constants Named vector If multiple inputs were screened in this
#' object, this argument can be used to modify the selected values for all
#' those except the input that's varying along the x-axis. See the
#' \href{https://rwparsons.github.io/predictNMB/articles/summarising-results-with-predictNMB.html}{summarising methods vignette}.
#' @param what What to summarise: one of "nmb", "inb" or "cutpoints".
#' Defaults to "nmb".
#' @param inb_ref_col Which cutpoint method to use as the reference strategy
#' when calculating the incremental net monetary benefit.
#' See \code{do_nmb_sim} for more information.
#' @param plot_range \code{logical}. Whether or not to plot the range of the
#' distribution as a thin line. Defaults to TRUE.
#' @param plot_ci \code{logical}. Whether or not to plot the confidence region
#' of the distribution as a thicker line. Defaults to TRUE.
#' @param plot_line \code{logical}. Whether or not to connect the medians of
#' the distributions for each method along the x-axis. Defaults to TRUE.
#' @param plot_alpha Alpha value for all plot elements. Defaults to 0.5.
#' @param dodge_width The dodge width of plot elements. Can be used to avoid
#' excessive overlap between methods. Defaults to 0.
#' @param ci The confidence level.
#' Defaults to 0.95 (coloured area of distribution represents 95% CIs).
#' @param methods_order The order (left to right) to display the cutpoint
#' methods.
#' @param rename_vector A named vector for renaming the methods in the summary.
#' The values of the vector are the default names and the names given are the
#' desired names in the output.
#' @param extra_theme Additional theme applied to plot. Defaults to remove
#' minor panel grid.
#' @param ... Additional (unused) arguments.
#'
#' @details
#' This plot method works with \code{predictNMBscreen} objects that are
#' created using \code{screen_simulation_inputs()}. Can be used to visualise
#' distributions from many different simulations and assign a varying input
#' to the x-axis of the plot.
#'
#' @return Returns a \code{ggplot} object
#' @export
#'
#' @examples
#'
#' if (FALSE) {
#'   get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
#'   sim_screen_obj <- screen_simulation_inputs(
#'     n_sims = 50, n_valid = 10000, sim_auc = seq(0.7, 0.9, 0.1),
#'     event_rate = c(0.1, 0.2, 0.3),
#'     fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
#'   )
#'
#'   plot(sim_screen_obj)
#'   plot(
#'     sim_screen_obj,
#'     x_axis_var = "event_rate",
#'     constants = c(sim_auc = 0.8),
#'     dodge_width = 0.02
#'   )
#' }
plot.predictNMBscreen <- function(x,
                                  x_axis_var = NULL,
                                  constants = list(),
                                  what = c("nmb", "inb", "cutpoints"),
                                  inb_ref_col = NA,
                                  plot_range = TRUE,
                                  plot_ci = TRUE,
                                  plot_line = TRUE,
                                  plot_alpha = 0.5,
                                  dodge_width = 0,
                                  ci = 0.95,
                                  methods_order = NULL,
                                  rename_vector,
                                  extra_theme = ggplot2::theme(
                                    panel.grid.minor = ggplot2::element_blank()
                                  ),
                                  ...) {
  if (is.null(x_axis_var)) {
    message("No value for 'x_axis_var' given.")
    if (length(x$screen_meta) == 1) {
      message(paste0("Screening over ", names(x$screen_meta), " by default"))
    } else {
      message(
        paste0(
          "Screening over ",
          names(x$screen_meta)[1],
          " by default. Specify the variable in the 'x_axis_var' argument",
          " if you want to plot changes over:\n",
          paste0(names(x$screen_meta)[-1], collapse = ", ")
        )
      )
    }
    x_axis_var <- names(x$screen_meta)[1]
  }

  grid_lookup <- x$summary_grid
  sim.id_ignore <- c()

  if (length(names(x$screen_meta)) > 1) {
    message(
      paste0(
        "\n\nVarying simulation inputs, other than ",
        x_axis_var,
        ", are being held constant:\n"
      )
    )
    non_x_axis_vars <- x$screen_meta
    non_x_axis_vars[[x_axis_var]] <- NULL
    for (i in length(non_x_axis_vars)) {
      if (names(non_x_axis_vars)[i] %in% names(constants)) {
        v <- constants[[names(non_x_axis_vars)[i]]]
        possible_values <- sort(
          unique(grid_lookup[[names(non_x_axis_vars)[i]]])
        )

        if (!any(approx_match(vec = possible_values, val = v))) {
          stop(
            paste0(
              "A ",
              names(non_x_axis_vars)[i],
              " value of [",
              v,
              "] was not included in the screened inputs and cannot be used as",
              " a specified constant.",
              "\n\nScreened input values that can be used for ",
              names(non_x_axis_vars)[i],
              " are: ",
              paste0(possible_values, collapse = ", ")
            )
          )
        }
      } else {
        v <- non_x_axis_vars[[i]][[1]]
        if (inherits(v, "function")) {
          v <- names(non_x_axis_vars[[i]])[1]
          if (x$pair_nmb_train_and_evaluation_functions &
            x_axis_var %in% c("fx_nmb_training", "fx_nmb_evaluation")) {
            next
          }
        }
      }

      message(paste0(
        names(non_x_axis_vars)[i],
        ": ",
        v
      ))

      sim.id_ignore <- append(
        sim.id_ignore,
        which(
          !approx_match(
            vec = grid_lookup[[names(non_x_axis_vars)[i]]],
            val = v
          )
        )
      )
    }
    sim.id_ignore <- unique(sim.id_ignore)

    # filter the grid of simulations to only those which will be in the plot
    # (keeping the non-{x_axis_var}'s constant)
    grid_lookup <- grid_lookup[!grid_lookup$.sim_id %in% sim.id_ignore, ]
  }

  p_data_full <- NULL
  for (s in grid_lookup$.sim_id) {
    p_data <- get_plot_data(
      x = x$simulations[[s]],
      what = what[1],
      methods_order = methods_order,
      rename_vector = rename_vector,
      inb_ref_col = inb_ref_col,
      ci = ci
    )
    p_data$.sim_id <- s
    p_data_full <- rbind(p_data_full, stats::na.omit(p_data))
  }

  p_data_full <-
    dplyr::left_join(p_data_full, grid_lookup, by = ".sim_id") %>%
    dplyr::rename(x_axis_var = dplyr::all_of(x_axis_var))

  p_data_range <-
    p_data_full %>%
    dplyr::group_by(x_axis_var, name) %>%
    dplyr::summarize(
      m = stats::median(value),
      ymin = min(value),
      ymax = max(value),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  p_data_interval <-
    p_data_full %>%
    dplyr::filter(in_interval) %>%
    dplyr::group_by(x_axis_var, name) %>%
    dplyr::summarize(
      ymin = min(value),
      ymax = max(value),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  y_axis_title <- switch(what[1],
    "nmb" = "Net Monetary Benefit ($)",
    "inb" = "Incremental Net Monetary Benefit ($)",
    "cutpoints" = "Selected Cutpoint"
  )

  x_axis_title <- switch(x_axis_var,
    "sample_size" = "Training sample size (n)",
    "n_sims" = "Number of simulations (n)",
    "n_valid" = "Validation sample size (n)",
    "sim_auc" = "Model AUC",
    "event_rate" = "Event rate",
    "fx_nmb_training" = "NMB Sampling Function (training)",
    "fx_nmb_evaluation" = "NMB Sampling Function (evaluation)"
  )

  if (dodge_width == 0) {
    position <- "identity"
  } else {
    position <- ggplot2::position_dodge(width = dodge_width)
  }

  p <-
    ggplot2::ggplot() +
    ggplot2::geom_point(
      data = p_data_range,
      ggplot2::aes(x = x_axis_var, y = m, col = name, group = name),
      size = 3, alpha = plot_alpha,
      position = position
    )

  if (plot_line) {
    p <-
      p +
      ggplot2::geom_line(
        data = p_data_range,
        ggplot2::aes(x = x_axis_var, y = m, col = name, group = name),
        alpha = plot_alpha,
        position = position
      )
  }

  if (plot_ci) {
    if (use_linewidth()) {
      ci_linerange_layer <- ggplot2::geom_linerange(
        data = p_data_interval,
        ggplot2::aes(x = x_axis_var, col = name, ymin = ymin, ymax = ymax),
        linewidth = 1.2, alpha = plot_alpha,
        position = position
      )
    } else {
      ci_linerange_layer <- ggplot2::geom_linerange(
        data = p_data_interval,
        ggplot2::aes(x = x_axis_var, col = name, ymin = ymin, ymax = ymax),
        size = 1.2, alpha = plot_alpha,
        position = position
      )
    }
    p <- p + ci_linerange_layer
  }

  if (plot_range) {
    p <-
      p +
      ggplot2::geom_linerange(
        data = p_data_range,
        ggplot2::aes(x = x_axis_var, col = name, ymin = ymin, ymax = ymax),
        alpha = plot_alpha,
        position = position
      )
  }

  p <- p +
    ggplot2::labs(
      x = x_axis_title,
      y = y_axis_title,
      col = "Cutpoint Methods"
    ) +
    ggplot2::theme_bw()


  if (!inherits(p_data_range$x_axis_var, "character")) {
    p <-
      p +
      ggplot2::scale_x_continuous(
        breaks = unique(p_data_range$x_axis_var)
      )
  }

  if (!is.null(extra_theme)) {
    p <- p + extra_theme
  }
  p
}
