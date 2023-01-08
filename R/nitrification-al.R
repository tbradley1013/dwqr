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
#' @param group_col vector of unquoted column names of grouping variables
#' @param method one of "hmm" (hidden markov model), "cp" (changepoints), or
#' "simple". See Details
#' @param percentiles a vector of the percentiles that you would like calculated
#' either on the overall data ("P" method) or on the falling limb portion of
#' the dataset ("FL" method). By default, 0.8, 0.5, and 0.1 are used for action
#' levels 1, 2, and 3, respectively
#' @param model the model type to use to estimate total chlorine trend. One of
#' "ss" (smooth spline), "gam" (generalized additive model), or "loess"
#' (Local Polynomial Regression)
#' @param max_chlorine maximum chlorine residual value that can be included in
#' falling limb. For example, if you are not concerned with sites when chlorine
#' is greater than 1.5 (default) than no value greater than this threshold
#' will be classified as either "Falling Limb" or "Nitrification Ongoing"
#' @param output_name should the output column names be given as title
#' action levels ("AL-C"), i.e. Action Level 1, Action Level 2, etc.,
#' as action levels better suited for R code ("AL"), i.e. action_level_1, etc.,
#' or as the percentile ("P"), i.e.
#' 80%, 50%, etc.
#'
#' @details
#' The method argument must be set to one of the following:
#' - "simple" - A simple classification method that classifies any negative
#' first derivative value as a part of the falling limb. Taking the first
#' derivative of the moving average of the chlorine values is likely to reduce
#' false classification rates when this model type is selected
#' - "hmm" - This method uses the depmixS4 package to fit a hidden markov model
#' using the time trend of the first derivative of the total chlorine trend
#' - "cp" - This method uses the strucchange package to identify change points
#' in the first derivative trned and classify values based on median first derivative
#' values between change points
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
#' Both of these methods will struggle if samples are not collected regularly at
#' a given location. Regular sampling allows for the overall trends to be identified
#' and classified correctly.
#'
#'
#' @export
nitrification_al <- function(data, date_col, value_col, group_col = NULL,
                             model = c("ss", "gam", "loess"),
                             method = c("simple", "hmm", "cp"),
                             percentiles = c(.8, .5, .2),
                             max_chlorine = 1.5, output_name = c("AL-C", "AL", "P")){
  if (!"data.frame" %in% class(data)) stop("data must be of class data.frame or tbl", call. = FALSE)
  req_cols <- c(missing(date_col), missing(value_col))
  if (any(req_cols)) stop("both `date_col` and `value_col` must be specified", call. = FALSE)
  # browser()

  method <- match.arg(method, c("simple", "hmm", "cp"))
  output_name <- match.arg(output_name, c("AL-C", "AL", "P"))

  # value_col <- rlang::enquo(value_col)
  # date_col <- rlang::enquo(date_col)
  # group_cols <- rlang::enquos(...)

  percentiles <- sort(percentiles, decreasing = TRUE)

  quant_names <- purrr::imap_chr(percentiles, ~{
    if (output_name == "AL-C") {
      paste("Action Level", .y)
    } else if (output_name == "AL") {
      paste0("action_level_", .y)
    } else {
      paste0(.x*100, "%")
    }
  })

  # quant_names <- paste0(percentiles*100, "%")
  quants <- purrr::map(percentiles, ~{
    purrr::partial(quantile, probs = .x, na.rm = TRUE)
  }) %>%
    purrr::set_names(nm = quant_names)

  data_classed <- data %>%
    rolling_slope({{date_col}}, {{value_col}}, {{group_col}}, model = model) %>%
    falling_limb({{value_col}}, method = method, first_deriv, {{group_col}})



  data_classed <- dplyr::group_by(data_classed, dplyr::across({{group_col}}))

  output <- data_classed %>%
    dplyr::filter(falling_limb == "Falling Limb", {{value_col}} < max_chlorine) %>%
    dplyr::summarize_at(dplyr::vars({{value_col}}), quants)

  return(output)
}
