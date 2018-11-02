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
#' @param plot_subtitle a character string specifying the subtitle of the plot
#' @param legend_title a character string specifying the legend title
#' @param action_levels a numeric vector specifying action levels. If this is
#' NULL (default) than the action levels will be specified via the method
#' provided in the method arguement. If this argument is given, then the vector
#' will be sorted with the lowest number by default being the highest action level
#' and the highest number given being the lowest action level. The number of action
#' levels in the resulting data will be equal to the number of action levels given
#'
#' @export
plot_al <- function(data, date_col, value_col, ..., method = c("FL", "P"),
                    percentiles = c(.8, .5, .1), rolling_window = 8,
                    max_chlorine = 1.5, date_breaks = "6 months", date_labels = "%b %d, %Y",
                    ylab = "", plot_title = "", plot_subtitle = "", legend_title = "",
                    action_levels = NULL){

  if (!"data.frame" %in% class(data)) stop("data must be of class data.frame or tbl")
  req_cols <- c(missing(date_col), missing(value_col))
  if (any(req_cols)) stop("both `date_col` and `value_col` must be specified", call. = FALSE)

  method <- match.arg(method, c("FL", "P"))

  value_col <- rlang::enquo(value_col)
  date_col <- rlang::enquo(date_col)
  group_cols <- rlang::enquos(...)

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
      p <- p + ggplot2::facet_wrap(dplyr::vars(!!!group_cols))
    }

    return(p)

  } else {

    if (method == "FL") {
      data_classed <- data %>%
        rolling_slope(!!date_col, !!value_col, ..., rolling_window = rolling_window) %>%
        falling_limb(!!value_col, rolling_first, rolling_second, ..., max_chlorine = max_chlorine)

      if (!rlang::is_empty(group_cols)) {
        data_classed <- dplyr::group_by(data_classed, !!!group_cols)
      }



      plot_data <- data_classed %>%
        dplyr::filter(falling_limb == "Falling Limb") %>%
        dplyr::mutate_at(dplyr::vars(!!value_col), dplyr::funs(!!!quants))

    } else if (method == "P") {
      if (!rlang::is_empty(group_cols)) {
        data <- dplyr::group_by(data, !!!group_cols)
      }

      plot_data <- data %>%
        dplyr::mutate_at(dplyr::vars(!!value_col), dplyr::funs(!!!quants))

    }

    p <- plot_data %>%
      {suppressMessages(dplyr::full_join(., data_classed))} %>%
      tidyr::fill(dplyr::contains("action_level")) %>%
      tidyr::fill(dplyr::contains("action_level"), .direction = "up") %>%
      dplyr::mutate(
        level = dplyr::case_when(
          !!value_col < action_level_3 ~ "Action Level 3",
          !!value_col < action_level_2 ~ "Action Level 2",
          !!value_col < action_level_1 ~ "Action Level 1",
          TRUE ~ "No Action Required"
        )
      ) %>%
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
      p <- p + ggplot2::facet_wrap(dplyr::vars(!!!group_cols))
    }

    return(p)
  }



}
