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
falling_limb <- function(data, method = c("simple", "hmm"), value_col, first_deriv, second_deriv, ...){
  if (!"data.frame" %in% class(data)) stop("data must be a data.frame or a tibble")

  value_col <- rlang::enquo(value_col)
  first_deriv <- rlang::enquo(first_deriv)
  second_deriv <- rlang::enquo(second_deriv)
  group_cols <- rlang::enquos(...)

  if (method == "simple"){
    out <- fl_class_simple(data, value_col = !!value_col, first_deriv = !!first_deriv, second_deriv = !!second_deriv, ...)
  } else if (method == "hmm"){
    out <- fl_class_hmm(data, value_col = !!value_col, first_deriv = !!first_deriv, second_deriv = !!second_deriv, ...)
  }

  return(out)
}


fl_class_hmm <- function(data, value_col, first_deriv, second_deriv, ...){
  first_deriv <- rlang::enquo(first_deriv)
  group_cols <- rlang::enquos(...)

  if (!rlang::is_empty(group_cols)) {
    data <- dplyr::group_nest(data, !!!group_cols)

    out <- data %>%
      dplyr::mutate(
        data = purrr::map(data, ~{
          .x <- .x %>%
            dplyr::mutate(x = !!first_deriv)

          mod <- depmixS4::mix(
            list(x ~ 1),
            data = .x,
            nstates = 2,
            family = list(stats::gaussian())
          )

          fm <- depmixS4::fit(mod)

          states <- depmixS4::posterior(fm)

          out <- .x %>%
            dplyr::mutate(
              est_state = as.character(states$state)
            ) %>%
            dplyr::group_by(est_state) %>%
            dplyr::mutate(
              state_avg_deriv = mean(x, na.rm = TRUE)
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              falling_limb = ifelse(state_avg_deriv == min(state_avg_deriv), "Falling Limb", "Other")
            ) %>%
            dplyr::select(-c(x, state_avg_deriv, est_state))

          return(out)
        })
      ) %>%
      tidyr::unnest(data)
  } else {
    data <- data %>%
      dplyr::mutate(x = !!first_deriv)

    mod <- depmixS4::mix(
      list(x ~ 1),
      data = data,
      nstates = 2,
      family = list(stats::gaussian())
    )

    fm <- depmixS4::fit(mod)

    states <- depmixS4::posterior(fm)

    out <- data %>%
      dplyr::mutate(
        est_state = as.character(states$state)
      ) %>%
      dplyr::group_by(est_state) %>%
      dplyr::mutate(
        state_avg_deriv = mean(x, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        falling_limb = ifelse(state_avg_deriv == min(state_avg_deriv), "Falling Limb", "Other")
      ) %>%
      dplyr::select(-c(x, state_avg_deriv, est_state))
  }



  return(out)


}


fl_class_simple <- function(data, value_col, first_deriv, second_deriv, ...){
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
        !!first_deriv < 0 & lag(!!first_deriv) < 0 ~ "Falling Limb",
        TRUE ~ "Other"
      ),
    )

  if (!rlang::is_empty(group_cols)) {
    output <- dplyr::ungroup(output)
  }

  return(output)
}
