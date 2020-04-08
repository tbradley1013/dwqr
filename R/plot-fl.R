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
#' @param rolling_window how many weeks of data should be included in the rolling
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
#' @param ylim a vector of length two specifying the limits of the y axis for the
#' chlorine plot. If NULL (default) default ggplot2 limits will be used
#' @param plot_title a character string specifying the title of the plot
#' @param plot_subtitle a chacter string specifying the subtitle of the plot
#' @param include_first logical; should a plot of the first derivative and
#' its moving average be included along with the main trend?
#' @param first_ylim a vector of length two specifying the limits of the y axis for the
#' first derivative of chlorine plot. If NULL (default) default ggplot2 limits will be used
#' @param theme a \code{\link{ggplot2::theme}} object that can be passed to
#' all plots
#' @param nitrite_col column name (optional) of column containing nitrite
#' data that corresponds with the chlorine data. This will be plotted
#' alongside the chlorine data via the \code{\link{patchwork}} package
#' @param nitrite_ylab character string that specifies the y label for the
#' nitrite graph. Only used if nitrite_col is specified
#' @param nitrite_ylim a vector of length two specifying the limits of the y axis for the
#' nitrite plot. If NULL (default) default ggplot2 limits will be used
#' @param ncol,nrow If grouping variable is specified, these arguments set the
#' number of columns and rows, respectively of the resultant facetted plot
#'
#'
#' @export
plot_fl <- function(data, date_col, value_col, ..., rolling_window = 8,
                    max_chlorine = 1.5, smooth_deriv = FALSE, deriv_window = NULL,
                    date_breaks = "6 months", date_labels = "%b %d, %Y",
                    ylab = "", ylim = NULL, plot_title = "", plot_subtitle = "",
                    include_first = FALSE, first_ylim = NULL, theme = NULL,
                    nitrite_col, nitrite_ylab = "Nitrite",
                    nitrite_ylim = NULL,
                    ncol = NULL, nrow = NULL){
  if (!"data.frame" %in% class(data)) stop("data must be of class data.frame or tbl")

  date_col <- rlang::enquo(date_col)
  value_col <- rlang::enquo(value_col)
  group_cols <- rlang::enquos(...)

  if (!rlang::is_empty(group_cols)){
    data <- dplyr::group_by(data, !!!group_cols)
  }

  plot_data <- label_fl(data, !!date_col, !!value_col, ..., rolling_window = rolling_window,
                        smooth_deriv = smooth_deriv, deriv_window = deriv_window,
                        max_chlorine = max_chlorine)



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
      title = plot_title,
      subtitle = plot_subtitle
    )

  if (!rlang::is_empty(group_cols)){
    p <- p +
      ggplot2::facet_wrap(dplyr::vars(!!!group_cols),
                          ncol = ncol, nrow = nrow)
  }

  if ("Date" %in% date_class) {
    p <- p + ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels)
  } else if ("POSIXct" %in% date_class){
    p <- p + ggplot2::scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels)
  }

  if (!is.null(ylim)) {
    p <- p + ggplot2::scale_y_continuous(limits = ylim)
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
        ggplot2::facet_wrap(dplyr::vars(!!!group_cols),
                            ncol = ncol, nrow = nrow)
    }

    if ("Date" %in% date_class) {
      p2 <- p2 + ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels)
    } else if ("POSIXct" %in% date_class){
      p2 <- p2 + ggplot2::scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels)
    }

    if (!is.null(first_ylim)){
      p2 <- p2 + ggplot2::scale_y_continuous(limits = first_ylim)
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
        y = nitrite_ylab
      )

    if (!rlang::is_empty(group_cols)){
      p3 <- p3 +
        ggplot2::facet_wrap(dplyr::vars(!!!group_cols),
                            ncol = ncol, nrow = nrow)
    }

    if ("Date" %in% date_class) {
      p3 <- p3 + ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = date_labels)
    } else if ("POSIXct" %in% date_class){
      p3 <- p3 + ggplot2::scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels)
    }

    if (!is.null(nitrite_ylim)){
      p3 <- p3 + ggplot2::scale_y_continuous(limits = nitrite_ylim)
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
