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

  if (!missing(rename_vector)) {
    data <- dplyr::rename(data, dplyr::any_of(rename_vector))
  }

  if (what == "inb") {
    data <-
      data %>%
      dplyr::mutate(dplyr::across(!n_sim, function(x) x - !!rlang::sym(inb_ref_col))) %>%
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


get_approx_match_indices <- function(vec, val) {
  # find indices of vec which match val
  # use this approach for matching constants to simulation inputs as simulations input vectors can be passed from seq() and there are precision issues with matching values to those vectors
  # all.equal() performs a test for 'near equality' and therefore doesn't have this issue.
  unlist(lapply(vec, function(x) isTRUE(all.equal(x, val))))
}


#' @export
plot.predictNMBsim <- function(x,
                               what = c("nmb", "inb", "cutpoints"),
                               inb_ref_col = NA,
                               ci = 0.95,
                               methods_order = NULL,
                               n_bins = 40,
                               label_wrap_width = 15,
                               fill_cols = c("grey50", "grey50", "#ADD8E6"),
                               median_line_size = 2,
                               median_line_alpha = 0.5,
                               rename_vector,
                               extra_theme = ggplot2::theme(
                                 panel.spacing = ggplot2::unit(0, "lines"),
                                 axis.ticks.x = ggplot2::element_blank(),
                                 axis.text.x = ggplot2::element_blank(),
                                 strip.background = ggplot2::element_rect(fill = "#f1f1f1")
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
    ggplot2::facet_wrap(~name, labeller = ggplot2::label_wrap_gen(width = label_wrap_width), nrow = 1) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = c(fill_cols)) +
    ggplot2::guides(fill = "none")


  my_plot_innards <- ggplot2::ggplot_build(p)

  extracted_points <- tibble::tibble(
    outcome = my_plot_innards[["data"]][[1]][["x"]],
    count = my_plot_innards[["data"]][[1]][["y"]],
    in_interval = (my_plot_innards[["data"]][[1]][["group"]]) %>% as.factor(),
    method = (my_plot_innards[["data"]][[1]][["PANEL"]] %>% as.character())
  )

  heights <-
    df_agg %>%
    tibble::rownames_to_column(var = "method_n") %>%
    dplyr::left_join(extracted_points, by = c("method_n" = "method")) %>%
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

  p <-
    p +
    ggplot2::geom_segment(
      data = heights,
      ggplot2::aes(x = m, xend = m, y = 0, yend = count),
      size = median_line_size, alpha = median_line_alpha
    ) +
    ggplot2::labs(
      x = x_axis_title,
      y = ""
    )

  if (!is.null(extra_theme)) {
    p <- p + extra_theme
  }

  p
}



#' @export
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
                                  extra_theme = NULL,
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
          " by default. Specify the variable in the 'x_axis_var' argument if you want to plot changes over:\n",
          paste0(names(x$screen_meta)[-1], collapse = ", ")
        )
      )
    }
    x_axis_var <- names(x$screen_meta)[1]
  }

  grid_lookup <- x$summary_grid
  grid_lookup$.sim_id <- seq_len(nrow(grid_lookup))
  sim.id_ignore <- c()

  if (length(names(x$screen_meta)) > 1) {
    message(paste0("\n\nVarying simulation inputs, other than ", x_axis_var, ", are being held constant:\n"))
    non_x_axis_vars <- x$screen_meta
    non_x_axis_vars[[x_axis_var]] <- NULL
    for (i in length(non_x_axis_vars)) {
      if (names(non_x_axis_vars)[i] %in% names(constants)) {
        v <- constants[[names(non_x_axis_vars)[i]]]
      } else {
        v <- non_x_axis_vars[[i]][[1]]
        if (inherits(v, "function")) {
          v <- names(non_x_axis_vars[[i]])[1]
        }
      }

      message(paste0(
        names(non_x_axis_vars)[i],
        ": ",
        v
      ))

      sim.id_ignore <- append(
        sim.id_ignore,
        which(!get_approx_match_indices(vec = grid_lookup[[names(non_x_axis_vars)[i]]], val = v))
      )
    }
    sim.id_ignore <- unique(sim.id_ignore)

    # filter the grid of simulations to only those which will be in the plot (keeping the non-{x_axis_var}'s constant)
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
    dplyr::summarize(m = stats::median(value), ymin = min(value), ymax = max(value), .groups = "drop") %>%
    dplyr::ungroup()

  p_data_interval <-
    p_data_full %>%
    dplyr::filter(in_interval) %>%
    dplyr::group_by(x_axis_var, name) %>%
    dplyr::summarize(ymin = min(value), ymax = max(value), .groups = "drop") %>%
    dplyr::ungroup()

  y_axis_title <- switch(what[1],
    "nmb" = "Net Monetary Benefit ($)",
    "inb" = "Incremental Net Monetary Benefit ($)",
    "cutpoints" = "Selected Cutpoint"
  )

  x_axis_title <- switch(x_axis_var,
    "sample_size" = "Training sample size",
    "n_sims" = "Number of simulations",
    "n_valid" = "Validation sample size",
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
    p <-
      p +
      ggplot2::geom_linerange(
        data = p_data_interval,
        ggplot2::aes(x = x_axis_var, col = name, ymin = ymin, ymax = ymax),
        size = 1.2, alpha = plot_alpha,
        position = position
      )
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
