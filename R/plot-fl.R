#' Plot falling limb functions
#'
#' @param data a data frame containing chlorine residual results
#' @param date_col unqouted column name of date time column in data
#' @param value_col unquoted column name of chlorine column in data
#' @param ... unqouted column name(s) of grouping variable(s)
#' @param rolling_window how many observations should be included in the rolling
#' average window function when calculating the first and second derivative of
#' the chlorine time series. Defaults to 8.
#' @param max_chlorine maximum chlorine residual value that can be included in
#' falling limb. For example, if you are not concerned with sites when chlorine
#' is greater than 1.5 (default) than no value greater than this threshold
#' will be classified as either "Falling Limb" or "Nitrification Ongoing"
#'
#' @export
plot_fl <- function(data, date_col, value_col, ..., rolling_window = 8,
                    max_chlorine = 1.5){
  if (!"data.frame" %in% class(data)) stop("data must be of class data.frame or tbl")

  date_col <- rlang::enquo(date_col)
  value_col <- rlang::enquo(value_col)
  group_cols <- rlang::enquos(...)

  if (!rlang::is_empty(group_cols)){
    data <- dplyr::group_by(data, !!!group_cols)
  }

  plot_data <- data %>%
    rolling_slope(!!date_col, !!value_col, ..., rolling_window = rolling_window) %>%
    falling_limb(!!value_col, rolling_first, rolling_second, ..., max_chlorine = max_chlorine)

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
