#' determine action levels for nitrification
#'
#' @param data a data frame with chlorine residual results
#' @param date_col the unquoted column name of a date or datetime column in data
#' @param value_col the unquoted column name of the results in data
#' @param ... unquoted column names of grouping variables
#' @param method either "FL" (Falling Limb) or "P" (Percentiles)
#' @param percentiles a vector of the percentiles that you would like calculated
#' either on the overall data ("P" method) or on the falling limb portion of
#' the dataset ("FL" method). By default, 0.8, 0.5, and 0.1 are used for action
#' levels 1, 2, and 3, respectively
#' @param rolling_window how many observations should be included in the rolling
#' average window function when calculating the first and second derivative of
#' the chlorine time series. Defaults to 8.
#'
#'
nitrification_al <- function(data, date_col, value_col, ..., method = c("FL", "P"),
                             percentiles = c(.8, .5, .1), rolling_window = 8){
  if (!"data.frame" %in% class(data)) stop("data must be of class data.frame or tbl")
  # browser()

  method <- match.arg(method, c("FL", "P"))

  value_col <- rlang::enquo(value_col)
  date_col <- rlang::enquo(date_col)
  group_cols <- rlang::enquos(...)

  quant_names <- paste0(percentiles*100, "%")
  quants <- purrr::map(percentiles, ~{
    purrr::partial(quantile, probs = .x, na.rm = TRUE)
  }) %>%
    purrr::set_names(nm = quant_names)


  if (method == "FL") {
    data_classed <- data %>%
      rolling_slope(!!date_col, !!value_col, ..., rolling_window = rolling_window) %>%
      falling_limb(rolling_first, rolling_second, ...)

    if (!rlang::is_empty(group_cols)) {
      data_classed <- dplyr::group_by(data_classed, !!!group_cols)
    }



    output <- data_classed %>%
      dplyr::filter(falling_limb == "Falling Limb") %>%
      dplyr::summarize_at(vars(!!value_col), funs(!!!quants))
  } else if (method == "P") {
    if (!rlang::is_empty(group_cols)) {
      data <- dplyr::group_by(data, !!!group_cols)
    }

    output <- data %>%
      dplyr::summarize_at(vars(!!value_col), funs(!!!quants))

  }

  return(output)
}
