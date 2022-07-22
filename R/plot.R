get_plot_data <- function(data, methods_order) {
  pivoted_data <- tidyr::pivot_longer(data, !n_sim)
  if(is.null(methods_order)){
    methods_order <- data %>% dplyr::select(-n_sim) %>% names()
  }
  pivoted_data$name <- factor(pivoted_data$name, levels=methods_order)

  pivoted_data
}

add_interval <- function(data, ci){
  probs <- c((1 - ci)/2, 1 - (1 - ci)/2)

  data %>%
    dplyr::group_by(name) %>%
    dplyr::arrange(value) %>%
    dplyr::mutate(percentile=dplyr::row_number()/dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(in_interval = percentile > probs[1] & percentile < probs[2])
}


#' @export
plot.predictNMBsim <- function(x,
                               what=c("nmb", "inb", "cutpoints"),
                               inb_ref_col=NA,
                               ci=0.95,
                               methods_order=NULL,
                               n_bins=40,
                               label_wrap_width=15,
                               fill_cols = c("grey50", "grey50", "#ADD8E6"),
                               median_line_size=2,
                               median_line_alpha=0.5,
                               rename_vector,
                               extra_theme=ggplot2::theme(panel.spacing = ggplot2::unit(0, "lines"),
                                                 axis.ticks.x = ggplot2::element_blank(),
                                                 axis.text.x = ggplot2::element_blank(),
                                                 strip.background = ggplot2::element_rect(fill="#f1f1f1")),
                               ...) {
  what <- what[1]

  if(what=="cutpoints") {
    data <- x$df_thresholds
  } else {
    data <- x$df_result
  }

  if(!missing(rename_vector)){
    data <- dplyr::rename(data, dplyr::any_of(rename_vector))
  }

  if(what=="inb") {
    data <-
      data %>%
      dplyr::mutate(dplyr::across(!n_sim, function(x) x - !!rlang::sym(inb_ref_col))) %>%
      dplyr::select(-dplyr::all_of(inb_ref_col))
  }

  p_data <- data %>%
    get_plot_data(methods_order=methods_order) %>%
    add_interval(ci=ci)

  df_agg <-
    p_data %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(m=stats::median(value))

  p <-
    p_data %>%
    ggplot2::ggplot(ggplot2::aes(value, fill=in_interval)) +
    ggplot2::geom_histogram(bins=n_bins) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~name, labeller=ggplot2::label_wrap_gen(width=label_wrap_width), nrow=1) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values=c(fill_cols)) +
    ggplot2::guides(fill="none")


  my_plot_innards <- ggplot2::ggplot_build(p)

  extracted_points <- tibble::tibble(
    outcome = my_plot_innards[["data"]][[1]][["x"]],
    count = my_plot_innards[["data"]][[1]][["y"]],
    in_interval = (my_plot_innards[["data"]][[1]][["group"]]) %>% as.factor(),
    method = (my_plot_innards[["data"]][[1]][["PANEL"]] %>% as.character())
  )

  heights <-
    df_agg %>%
    tibble::rownames_to_column(var="method_n") %>%
    dplyr::left_join(extracted_points, by=c("method_n"="method")) %>%
    dplyr::mutate(diff=abs(m-outcome)) %>%
    dplyr::group_by(name) %>%
    dplyr::arrange(diff) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  p <-
    p +
    ggplot2::geom_segment(
      data=heights,
      ggplot2::aes(x=m, xend=m, y=0, yend=count),
      size=median_line_size, alpha=median_line_alpha
    )

  if(!is.null(extra_theme)){
    p <- p + extra_theme
  }

  p
}
