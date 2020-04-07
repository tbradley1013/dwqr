#' Calculate first and second derivative of chlorine trend
#'
#' \code{rolling_slope} calculates the first and second derivative
#' of a provided time series trend along with the moving average
#' of these values
#'
#' @param data a dataframe or tbl that contains at the minimum a date column
#' and a chlorine residual column
#' @param date_col the unqouted name of the date column
#' @param value_col the unquoted name of the chlorine residual column
#' @param ... the unqouted names of all grouping columns, if grouping is
#' needed - e.g. if the dataset contains multiple sites and/or parameters.
#' @param rolling_window how large should the rolling mean window be? Specifies
#' the number of weeks worth of data to include in rolling mean calculations
#' @importFrom stats smooth.spline predict
#'
#' @export
rolling_slope <- function(data, date_col, value_col, ..., rolling_window = 8){
  if (!"data.frame" %in% class(data)) stop("data must be a data.frame or a tibble")

  date_col <- rlang::enquo(date_col)
  date_name <- rlang::quo_name(date_col)
  value_col <- rlang::enquo(value_col)
  value_name <- rlang::quo_name(value_col)
  group_cols <- rlang::enquos(...)

  if (!rlang::is_empty(group_cols)) {
    data <- dplyr::group_by(data, !!!group_cols)
  }

  data <- data %>%
    dplyr::filter(!is.na(!!value_col),
                  !is.na(!!date_col))


  data <- data %>%
    dplyr::mutate(
      roll_mean = rolling_mean(!!value_col, !!date_col, rolling_window),
      date_numeric = as.numeric(!!date_col)
    ) %>%
    dplyr::filter(!is.na(roll_mean))

  if (length(group_cols) == 0){
    data <- tidyr::nest(data, data = dplyr::everything())
  } else {
    data <- tidyr::nest(data)
  }

  output <- data %>%
    dplyr::mutate(
      spline_ma = purrr::map(.data$data, ~smooth.spline(.x[[ !!date_name ]], .x$roll_mean)),
      first_deriv_ma = purrr::map2(.data$spline_ma, .data$data, ~{
        predict(.x, .y$date_numeric, deriv = 1) %>%
          tibble::as_tibble()
      }),
      deriv_spline_ma = purrr::map2(.data$first_deriv_ma, .data$data, ~{
        smooth.spline(.y[[ !!date_name ]], .x$y)
      }),
      second_deriv_ma = purrr::map2(.data$deriv_spline_ma, .data$data, ~{
        predict(.x, .y$date_numeric, deriv = 1) %>%
          tibble::as_tibble()
      }),
      data = purrr::pmap(
        .l = list(
          .data$data,
          .data$first_deriv_ma,
          .data$second_deriv_ma
        ),
        .f = ~{
          ..1 %>%
            dplyr::left_join(..2, by = c("date_numeric" = "x")) %>%
            dplyr::rename(first_deriv_ma = y) %>%
            dplyr::left_join(..3, by = c("date_numeric" = "x")) %>%
            dplyr::rename(second_deriv_ma = y)
        }
      )
    ) %>%
    dplyr::select(!!!group_cols, data) %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(
      rolling_first = rolling_mean(first_deriv_ma, !!date_col, rolling_window),
      rolling_second = rolling_mean(second_deriv_ma, !!date_col, rolling_window)
    )

  return(output)

}


rolling_mean <- function(value, date_col, window){
  # slider::slide_dbl(.x = value, .f = mean, .before = window, na.rm = TRUE)
  slider::slide_index_dbl(.x = value, .i = date_col, .f = mean, .before = lubridate::weeks(window), na.rm = TRUE)
}
