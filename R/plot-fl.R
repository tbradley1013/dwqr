#' Plot falling limb functions
#'
#' @param data
plot_fl <- function(data, date_col, value_col, ..., rolling_window = 8){
  if (!"data.frame" %in% class(data)) stop("data must be of class data.frame or tbl")

  date_col <- rlang::enquo(date_col)
  value_col <- rlang::enquo(value_col)
  group_cols <- rlang::enquos(...)

  if (!rlang::is_empty(group_cols)){
    data <- dplyr::group_by(data, !!!group_cols)
  }

  plot_data <- data %>%
    rolling_slope(!!date_col, !!value_col, ..., rolling_window = rolling_window) %>%
    falling_limb(!!value_col, rolling_first, rolling_second, ...)

  p <- plot_data %>%
    ggplot2::ggplot(aes(!!date_col, !!value_col, color = falling_limb)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::scale_color_brewer(name = "", palette = "Dark2")

  if (!rlang::is_empty(group_cols)){
    p <- p +
      ggplot2::facet_wrap(vars(!!!group_cols))
  }

  return(p)

}
