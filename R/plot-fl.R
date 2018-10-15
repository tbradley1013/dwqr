#' Plot falling limb functions
#'
#' \code{plot_fl} provides methods to plot the falling limb of a
#' chlorine residual time series plot. This function will be useful for
#' trying to visualize how selected parameters such as \code{rolling_window}
#' effect the grouping of data points
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
#' @param date_breaks a character string to be passed to either
#' \code{\link{ggplot2::scale_x_date}} or \code{\link{ggplot2::scale_x_datetime}}
#' to specify the spacing of date breaks (e.g. "1 month" or "3 weeks")
#' @param date_labels a character string specifying the desired output
#' format of dates on x axis. See \code{\link{strptime}} for options
#' @param ylab a character string specifying the y axis label for the main plot
#' @param plot_title a character string specifying the title of the plot
#' @param include_first logical; should a plot of the first derivative and
#' its moving average be included along with the main trend?
#' @param theme a \code{\link{ggplot2::theme}} object that can be passed to
#' all plots
#'
#'
#' @export
plot_fl <- function(data, date_col, value_col, ..., rolling_window = 8,
                    max_chlorine = 1.5, date_breaks = "6 months", date_labels = "%b %d, %Y",
                    ylab = "", plot_title = "", include_first = FALSE, theme = NULL,
                    nitrite_col){
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

  date_class <- plot_data %>%
    dplyr::pull(!!date_col) %>%
    head(1) %>%
    class()

  p <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(!!date_col, !!value_col, color = falling_limb)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::scale_color_brewer(name = "", palette = "Dark2") +
    ggplot2::labs(
      y = ylab,
      x = "",
      title = plot_title
    )

  if (!rlang::is_empty(group_cols)){
    p <- p +
      ggplot2::facet_wrap(dplyr::vars(!!!group_cols))
  }

  if ("Date" %in% date_class) {
    p <- p + ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels)
  } else if ("POSIXct" %in% date_class){
    p <- p + ggplot2::scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels)
  }

  if (!is.null(theme)) {
    p <- p +
      theme
  }

  if (include_first) {
    p2 <- plot_data %>%
      ggplot2::ggplot(ggplot2::aes(!!date_col)) +
      ggplot2::geom_point(ggplot2::aes(y = first_deriv, color = "black")) +
      ggplot2::geom_line(ggplot2::aes(y = rolling_first, color = "red"), size = 1) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color = "black", linetype = "dashed") +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(
        values = c("black" = "black", "red" = "red"),
        labels = c(
          "First Derivative",
          "Rolling Average of First Derivative"
        )
      ) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        x = "",
        y = "First Derivative (mg/L/s)"
      )

    if (!rlang::is_empty(group_cols)){
      p2 <- p2 +
        ggplot2::facet_wrap(dplyr::vars(!!!group_cols))
    }

    if ("Date" %in% date_class) {
      p2 <- p2 + ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels)
    } else if ("POSIXct" %in% date_class){
      p2 <- p2 + ggplot2::scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels)
    }

    if (!is.null(theme)) {
      p2 <- p2 +
        theme
    }

    # p <- patchwork::wrap_plots(
    #   p,
    #   p2,
    #   ncol = 1
    # )

  } else p2 <- NULL

  if (!missing(nitrite_col)) {
    nitrite_col <- rlang::enquo(nitrite_col)

    p3 <- plot_data %>%
      ggplot2::ggplot(ggplot2::aes(!!date_col)) +
      ggplot2::geom_point(ggplot2::aes(y = !!nitrite_col)) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        x = "",
        y = "Nitrite"
      )

    if (!rlang::is_empty(group_cols)){
      p3 <- p3 +
        ggplot2::facet_wrap(dplyr::vars(!!!group_cols))
    }

    if ("Date" %in% date_class) {
      p3 <- p3 + ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels)
    } else if ("POSIXct" %in% date_class){
      p3 <- p3 + ggplot2::scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels)
    }

    if (!is.null(theme)) {
      p3 <- p3 +
        theme
    }

    # p <- patchwork::wrap_plots(
    #   p,
    #   p3,
    #   ncol = 1
    # )

  } else p3 <- NULL

  plot_list <- list(p, p2, p3) %>%
    purrr::discard(is.null)

  output <- patchwork::wrap_plots(
    plot_list,
    ncol = 1
  )

  return(output)

}
