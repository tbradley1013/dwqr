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
#' @param group_col vector of unqouted names of all grouping columns, if grouping is
#' needed - e.g. if the dataset contains multiple sites and/or parameters.
#' @param model the model type to use to estimate total chlorine trend. One of
#' "ss" (smooth spline), "gam" (generalized additive model), or "loess"
#' (Local Polynomial Regression)
#' @importFrom stats smooth.spline predict
#'
#' @export
rolling_slope <- function(data, date_col, value_col, group_col = NULL,
                          model = c("ss", "gam", "loess"), return_models = FALSE){
  if (!"data.frame" %in% class(data)) stop("data must be a data.frame or a tibble")
  # browser()
  # date_col <- rlang::enquo(date_col)
  date_name <- rlang::as_string(rlang::ensym(date_col))
  # value_col <- rlang::enquo(value_col)
  value_name <- rlang::as_string(rlang::ensym(value_col))
  # group_cols <- rlang::enquos(group_col)

  # if (is.null(deriv_window)) deriv_window <- rolling_window
  # if (!is.null(group_col)) {
  #   data <- dplyr::group_by(data, dplyr::across({{group_col}}))
  # }


  data_nest <- data %>%
    dplyr::group_by(dplyr::across({{group_col}})) %>%
    dplyr::filter(!is.na({{value_col}}),
                  !is.na({{date_col}})) %>%
    dplyr::mutate(
      # roll_mean = zoo::rollmean(!!value_col, rolling_window, fill = NA),
      date_numeric = as.numeric({{date_col}})
    ) %>%
    # dplyr::filter(!is.na(roll_mean)) %>%
    tidyr::nest()

  if (model == "ss"){
    output <- data_nest %>%
      mutate(
        spline = purrr::map(.data$data, ~smooth.spline(.x[[ date_name ]], .x[[ value_name ]])),
        first_deriv = purrr::map2(.data$spline, .data$data, ~{
          predict(.x, as.numeric(.y[[ date_name ]]), deriv = 1) %>%
            tibble::as_tibble() %>%
            dplyr::pull(y)
        })
      )
  } else if (model == "gam") {
    output <- data_nest %>%
      dplyr::mutate(
        gam_model = purrr::map(data, ~{
          mgcv::gam(.x[[ value_name ]] ~ s(date_numeric, bs = "cr", m=2, k = 64), data = .x)
        }),
        first_deriv = purrr::map2(gam_model, data, ~{
          gratia::derivatives(.x, term = "s(date_numeric)", type = "central", n = nrow(.y), eps = 1e-5) %>%
            dplyr::pull(derivative)

        })
      )
  } else if (model == "loess") {
    output <- data_nest %>%
      dplyr::mutate(
        loess_model = purrr::map(data, ~{
          loess(.x[[ value_name ]] ~ date_numeric, data = .x, span = .15)
        }),
        first_deriv = purrr::map(loess_model, ~{
          DAMisc::loessDeriv(.x)
        }),
      )
  }

  # output <- output %>%
  #   dplyr::mutate(
  #     data = purrr::pmap(
  #       .l = list(
  #         .data$data,
  #         .data$first_deriv
  #       ),
  #       .f = ~{
  #
  #         out <- dplyr::bind_cols(
  #           ..1,
  #           dplyr::select(..2, first_deriv = y)
  #         )
  #
  #         return(out)
  #       }
  #     )
  #   )

  if (return_models) return(output)

  output <- output %>%
    dplyr::select({{group_col}}, data, first_deriv) %>%
    tidyr::unnest(c(data, first_deriv))


  return(output)

}


# rolling_mean <- function(value, date_col, window){
#   # slider::slide_dbl(.x = value, .f = mean, .before = window, na.rm = TRUE)
#   slider::slide_index_dbl(.x = value, .i = date_col, .f = mean, .before = lubridate::weeks(window), na.rm = TRUE)
# }
