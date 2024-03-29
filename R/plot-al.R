#' Plot the action levels of chlorine results
#'
#' Generate plots showing whether an action level is exceeded
#'
#' @param data a data frame or tibble that has chlorine residual data
#' @param date_col the unquoted column name of the date/datetime column
#' @param value_col the unqouted column name of the chlorine residual column
#' @param ... unqouted column names of all grouping columns
#' @param method either "FL" or "P" to distinguish how you want the
#' action levels to be calculated
#' @param percentiles the percentiles that will be used to calculate the
#' action levels of either the falling limb (if method = "FL") or the
#' overall distribution (if method = "P")
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
#' @param plot_title a character string specifying the title of the plot
#' @param plot_subtitle a character string specifying the subtitle of the plot
#' @param legend_title a character string specifying the legend title
#' @param action_levels a numeric vector specifying action levels. If this is
#' NULL (default) than the action levels will be specified via the method
#' provided in the method argument. If this argument is given, then the vector
#' will be sorted with the lowest number by default being the highest action level
#' and the highest number given being the lowest action level. The number of action
#' levels in the resulting data will be equal to the number of action levels given
#' @param ncol,nrow If grouping variable is specified, these arguments set the
#' number of columns and rows, respectively of the resultant faceted plot
plot_al <- function(data, date_col, value_col, ..., method = c("FL", "P"),
                    percentiles = c(.8, .5, .1), rolling_window = 8,
                    smooth_deriv = FALSE, deriv_window = NULL,
                    max_chlorine = 1.5, date_breaks = "6 months", date_labels = "%b %d, %Y",
                    ylab = "", plot_title = "", plot_subtitle = "", legend_title = "",
                    action_levels = NULL, theme = NULL, ncol = NULL, nrow = NULL){

  if (!"data.frame" %in% class(data)) stop("data must be of class data.frame or tbl")
  req_cols <- c(missing(date_col), missing(value_col))
  if (any(req_cols)) stop("both `date_col` and `value_col` must be specified", call. = FALSE)

  method <- match.arg(method, c("FL", "P"))

  value_col <- rlang::enquo(value_col)
  date_col <- rlang::enquo(date_col)
  group_cols <- rlang::enquos(...)

  date_class <- data %>%
    dplyr::pull(!!date_col) %>%
    head(1) %>%
    class()

  percentiles <- sort(percentiles, decreasing = TRUE)

  quant_names <- paste0("action_level_", seq_along(percentiles))

  # quant_names <- paste0(percentiles*100, "%")
  quants <- purrr::map(percentiles, ~{
    purrr::partial(quantile, probs = .x, na.rm = TRUE)
  }) %>%
    purrr::set_names(nm = quant_names)


  if (!is.null(action_levels)){
    if (!is.numeric(action_levels)) stop("action_levels must be a numeric vector")

    action_levels <- sort(action_levels, decreasing = TRUE)

    names(action_levels) <- paste("Action Level", 1:length(action_levels))

    action_levels <- sort(action_levels)


    p <- data %>%
      dplyr::filter(!is.na(!!value_col)) %>%
      dplyr::mutate(
        level = purrr::map_chr(!!value_col, function(y){
          output <- purrr::map(action_levels, ~{
            if (y < .x) return(.x)
          }) %>%
            purrr::flatten() %>%
            .[. == suppressWarnings(min(purrr::flatten_dbl(.)))] %>%
            names()

          if (is.null(output)){
            return("No Action Required")
          } else return(output)
        })
      ) %>%
      ggplot2::ggplot(ggplot2::aes(!!date_col, !!value_col, color = level)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::labs(
        y = ylab,
        title = plot_title,
        subtitle = plot_subtitle,
        color = legend_title
      )

    if (!rlang::is_empty(group_cols)) {
      p <- p + ggplot2::facet_wrap(dplyr::vars(!!!group_cols),
                                   ncol = ncol, nrow = nrow)
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

    return(p)

  } else {
    al <- nitrification_al(data, !!date_col, !!value_col, ..., method = method,
                           percentiles = percentiles, rolling_window = rolling_window,
                           smooth_deriv = smooth_deriv, deriv_window = deriv_window,
                           max_chlorine = max_chlorine)

    al_cols <- colnames(al)
    group_names <- purrr::map_chr(group_cols, dplyr::quo_name)
    if (rlang::is_empty(group_cols)){
      plot_data <- data
      for (i in seq_along(al_cols)){
        plot_data[[al_cols[i]]] <- al[[al_cols[i]]]
      }

    } else {
      plot_data <- data %>%
        dplyr::left_join(al, by = group_names)
    }

    plot_data$level <- "No Action Required"
    al_cols <- al_cols[!al_cols %in% group_names]

    for (i in seq_along(al_cols)){
      al_sym <- dplyr::sym(al_cols[i])

      plot_data <- plot_data %>%
        dplyr::mutate(
          level = ifelse(!!value_col < !!al_sym, al_cols[i], level)
        )
    }

    p <- plot_data %>%
      dplyr::filter(!is.na(!!value_col)) %>%
      ggplot2::ggplot(ggplot2::aes(!!date_col, !!value_col, color = level)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(
        values = c(
          "No Action Required" = "black",
          "Action Level 1" = "#ff9a00",
          "Action Level 2" = "#ff7400",
          "Action Level 3" = "#ff0000"
        ),
        name = legend_title
      ) +
      ggplot2::labs(
        y = ylab,
        title = plot_title,
        subtitle = plot_subtitle
      )

    if (!rlang::is_empty(group_cols)) {
      p <- p + ggplot2::facet_wrap(dplyr::vars(!!!group_cols),
                                   ncol = ncol, nrow = nrow)
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

    return(p)
  }



}
