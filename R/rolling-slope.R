#' calculate first and second derivative of chlorine trend
#'
#' @param data a dataframe or tbl that contains at the minimum a date column
#' and a chlorine residual column
#' @param date_col the unqouted name of the date column
#' @param value_col the unquoted name of the chlorine residual column
#' @param ... the unqouted names of all grouping columns, if grouping is
#' needed - e.g. if the dataset contains multiple sites and/or parameters.
#' @param rolling_window how large should the rolling mean window be?
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

  rolling_mean <- tibbletime::rollify(mean, rolling_window)

  output <- data %>%
    dplyr::filter(!is.na(!!value_col),
                  !is.na(!!date_col)) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      spline = purrr::map(.data$data, ~smooth.spline(.x[[ !!date_name ]], .x[[ !!value_name ]])),
      first_deriv = purrr::map2(.data$spline, .data$data, ~{
        predict(.x, as.numeric(.y[[ !!date_name ]]), deriv = 1) %>%
          tibble::as_tibble()
      }),
      data_new = purrr::map2(.data$data, .data$first_deriv, ~{
        .x %>%
          dplyr::mutate(date_numeric = as.numeric(!!date_col)) %>%
          dplyr::left_join(.y, by = c("date_numeric" = "x")) %>%
          dplyr::rename(first_deriv = y) %>%
          dplyr::mutate(
            roll_mean = rolling_mean(!!value_col)
          ) %>%
          dplyr::filter(!is.na(roll_mean))
      }),
      spline_ma = purrr::map(.data$data_new, ~smooth.spline(.x[[ !!date_name ]], .x$roll_mean)),
      first_deriv_ma = purrr::map2(.data$spline_ma, .data$data_new, ~{
        predict(.x, .y$date_numeric, deriv = 1) %>%
          tibble::as_tibble()
      }),
      deriv_spline_ma = purrr::map2(.data$first_deriv_ma, .data$data_new, ~{
        smooth.spline(.y[[ !!date_name ]], .x$y)
      }),
      second_deriv_ma = purrr::map2(.data$deriv_spline_ma, .data$data_new, ~{
        predict(.x, .y$date_numeric, deriv = 1) %>%
          tibble::as_tibble()
      }),
      data_new = purrr::pmap(
        .l = list(
          .data$data_new,
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
    tidyr::unnest(data_new) %>%
    dplyr::mutate(
      rolling_first = rolling_mean(first_deriv_ma),
      rolling_second = rolling_mean(second_deriv_ma)
    )

    return(output)

}
