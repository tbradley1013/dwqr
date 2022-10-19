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

  # if (is.null(deriv_window)) deriv_window <- rolling_window

  if (!rlang::is_empty(group_cols)) {
    data <- dplyr::group_by(data, !!!group_cols)
  }


  output <- data %>%
    dplyr::filter(!is.na(!!value_col),
                  !is.na(!!date_col)) %>%
    dplyr::mutate(
      roll_mean = zoo::rollmean(!!value_col, rolling_window, fill = NA),
      date_numeric = as.numeric(!!date_col)
    ) %>%
    dplyr::filter(!is.na(roll_mean)) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      spline = purrr::map(.data$data, ~smooth.spline(.x[[ !!date_name ]], .x[[ !!value_name ]])),
      first_deriv = purrr::map2(.data$spline, .data$data, ~{
        predict(.x, as.numeric(.y[[ !!date_name ]]), deriv = 1) %>%
          tibble::as_tibble()
      }),
      spline_ma = purrr::map(.data$data, ~smooth.spline(.x[[ !!date_name ]], .x$roll_mean)),
      first_deriv_ma = purrr::map2(.data$spline_ma, .data$data, ~{
        predict(.x, .y$date_numeric, deriv = 1) %>%
          tibble::as_tibble()
      }),
      second_deriv_ma = purrr::map2(.data$spline_ma, .data$data, ~{
        predict(.x, .y$date_numeric, deriv = 2) %>%

          tibble::as_tibble()
      }),
      data = purrr::pmap(
        .l = list(
          .data$data,
          .data$first_deriv_ma,
          .data$second_deriv_ma
        ),
        .f = ~{

          out <- dplyr::bind_cols(
            ..1,
            dplyr::select(..2, first_deriv_ma = y),
            dplyr::select(..3, second_deriv_ma = y)
          )

          return(out)
        }
      )
    ) %>%
    dplyr::select(!!!group_cols, data) %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(
      rolling_first = zoo::rollmean(first_deriv_ma, rolling_window, fill = NA),
      rolling_second = zoo::rollmean(second_deriv_ma, rolling_window, fill = NA)
    )

  return(output)

}


rolling_mean <- function(value, date_col, window){
  # slider::slide_dbl(.x = value, .f = mean, .before = window, na.rm = TRUE)
  slider::slide_index_dbl(.x = value, .i = date_col, .f = mean, .before = lubridate::weeks(window), na.rm = TRUE)
}
