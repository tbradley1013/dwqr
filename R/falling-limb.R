#' classify the falling limb of chlorine trend
#'
#' @param data a data frame with chlorine residual results
#' @param first_deriv unquoted column name of column containing first derivative
#' of chlorine time series
#' @param second_deriv unquoted column name of column containing second
#' derivative of chlorine time series
#' @param ... unqouted column names of any grouping columns
#'
#' @export
falling_limb <- function(data, first_deriv, second_deriv, ...){
  if (!"data.frame" %in% class(data)) stop("data must be a data.frame or a tibble")

  first_deriv <- rlang::enquo(first_deriv)
  second_deriv <- rlang::enquo(second_deriv)
  group_cols <- rlang::enquos(...)

  if (!rlang::is_empty(group_cols)) {
    data <- dplyr::group_by(data, !!!group_cols)
  }


  output <- data %>%
    dplyr::mutate(
      falling_limb = dplyr::case_when(
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


