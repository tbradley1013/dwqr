#' determine action levels for nitrification
#'
#' \code{nitrification_al} calculates proposed action levels for chlorine
#' residual for nitrification control and response plans. These action
#' levels can be calculated via the falling limb method or by plain
#' percentiles of the overall data
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
#' @param max_chlorine maximum chlorine residual value that can be included in
#' falling limb. For example, if you are not concerned with sites when chlorine
#' is greater than 1.5 (default) than no value greater than this threshold
#' will be classified as either "Falling Limb" or "Nitrification Ongoing"
#' @param output_name should the output column names be given as action levels (AL),
#' i.e. Action Level 1, Action Level 2, etc., or as the percentile (P), i.e.
#' 80%, 50%, etc.
#'
#' @details
#' There are two methods provided for calculating nitrification action levels:
#'
#' FL - *Falling Limb* - This method uses the falling limb method to identify
#' the downward trend experienced by chlorine values as temperatures rise. Before
#' percentiles are caluclated, the falling limb portion of the graph is isolated
#' and only this portion is used in the percentile calculations
#'
#' P - *Percentiles* - This method simply takes the percentiles given on the
#' entire dataset to find the action levels.
#'
#' By default, there are three percentiles calculated (80%, 50%, 10%) corresponding
#' to three distinct action levels (80% - Action Level 1, 50% - Action Level 2,
#' 10% - Action Level 3). If more or less action levels are desired, then simply
#' specify more or less values in the **percentiles** argument.
#'
#' The output tibble will contain columns for each of the specified columns along
#' with any grouping variables used. By default, the column names will be given as
#' their corresponding action level. If you would like to display the percentiles as
#' column names rather than action levels, then specify **output_name = "P"**
#'
#'
#' @export
nitrification_al <- function(data, date_col, value_col, ..., method = c("FL", "P"),
                             percentiles = c(.8, .5, .1), rolling_window = 8,
                             max_chlorine = 1.5, output_name = c("AL", "P")){
  if (!"data.frame" %in% class(data)) stop("data must be of class data.frame or tbl", call. = FALSE)
  req_cols <- c(missing(date_col), missing(value_col))
  if (any(req_cols)) stop("both `date_col` and `value_col` must be specified", call. = FALSE)
  # browser()

  method <- match.arg(method, c("FL", "P"))
  output_name <- match.arg(output_name, c("AL", "P"))

  value_col <- rlang::enquo(value_col)
  date_col <- rlang::enquo(date_col)
  group_cols <- rlang::enquos(...)

  percentiles <- sort(percentiles, decreasing = TRUE)

  quant_names <- purrr::imap_chr(percentiles, ~{
    if (output_name == "AL") {
      paste("Action Level", .y)
    } else {
      paste0(.x*100, "%")
    }
  })

  # quant_names <- paste0(percentiles*100, "%")
  quants <- purrr::map(percentiles, ~{
    purrr::partial(quantile, probs = .x, na.rm = TRUE)
  }) %>%
    purrr::set_names(nm = quant_names)


  if (method == "FL") {
    data_classed <- data %>%
      rolling_slope(!!date_col, !!value_col, ..., rolling_window = rolling_window) %>%
      falling_limb(!!value_col, rolling_first, rolling_second, ..., max_chlorine = max_chlorine)

    if (!rlang::is_empty(group_cols)) {
      data_classed <- dplyr::group_by(data_classed, !!!group_cols)
    }



    output <- data_classed %>%
      dplyr::filter(falling_limb == "Falling Limb") %>%
      dplyr::summarize_at(dplyr::vars(!!value_col), dplyr::funs(!!!quants))

  } else if (method == "P") {
    if (!rlang::is_empty(group_cols)) {
      data <- dplyr::group_by(data, !!!group_cols)
    }

    output <- data %>%
      dplyr::summarize_at(dplyr::vars(!!value_col), dplyr::funs(!!!quants))

  }

  return(output)
}
