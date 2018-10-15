#' classify the falling limb of chlorine trend
#'
#' \code{falling_limb} use the first and second derivative of a
#' time series slope to classify portions of it as the falling limb
#' of the curve
#'
#' @param data a data frame with chlorine residual results
#' @param value_col unqouted column name of column containing the chlorine
#' results for the time series
#' @param first_deriv unquoted column name of column containing first derivative
#' of chlorine time series
#' @param second_deriv unquoted column name of column containing second
#' derivative of chlorine time series
#' @param ... unqouted column names of any grouping columns
#' @param max_chlorine maximum chlorine residual value that can be included in
#' falling limb. For example, if you are not concerned with sites when chlorine
#' is greater than 1.5 (default) than no value greater than this threshold
#' will be classified as either "Falling Limb" or "Nitrification Ongoing"
#'
#'
#' @export
falling_limb <- function(data, value_col, first_deriv, second_deriv, ..., max_chlorine = 1.5){
  if (!"data.frame" %in% class(data)) stop("data must be a data.frame or a tibble")

  value_col <- rlang::enquo(value_col)
  first_deriv <- rlang::enquo(first_deriv)
  second_deriv <- rlang::enquo(second_deriv)
  group_cols <- rlang::enquos(...)

  if (!rlang::is_empty(group_cols)) {
    data <- dplyr::group_by(data, !!!group_cols)
  }


  output <- data %>%
    dplyr::mutate(
      falling_limb = dplyr::case_when(
        !!value_col >= max_chlorine ~ "Other",
        !!first_deriv < 0 & !!second_deriv < 0 ~ "Falling Limb",
        !!first_deriv < 0 & !!second_deriv >= 0 ~ "Nitrification Ongoing",
        !!first_deriv >= 0 & !!second_deriv >= 0 ~ "Nitrification Ongoing",
        TRUE ~ "Other"
      )
    )

  if (!rlang::is_empty(group_cols)) {
    output <- dplyr::ungroup(output)
  }

  return(output)
}


