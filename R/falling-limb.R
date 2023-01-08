#' classify the falling limb of chlorine trend
#'
#' \code{falling_limb} use the first and second derivative of a
#' time series slope to classify portions of it as the falling limb
#' of the curve
#'
#' @param data a data frame with chlorine residual results
#' @param method the method used to classify the falling limb of the chlorine
#' curve. See Details.
#' @param value_col unqouted column name of column containing the chlorine
#' results for the time series
#' @param first_deriv unquoted column name of column containing first derivative
#' of chlorine time series
#' @param group_col vector of unqouted column names of any grouping columns
#'
#' @details
#' the method argument must be set to one of the following:
#' - "simple" - A simple classification method that classifies any negative
#' first derivative value as a part of the falling limb. Taking the first
#' derivative of the moving average of the chlorine values is likely to reduce
#' false classification rates when this model type is selected
#' - "hmm" - This method uses the depmixS4 package to fit a hidden markov model
#' using the time trend of the first derivative of the total chlorine trend
#' - "cp" - This method uses the strucchange package to identify change points
#' in the first derivative trned and classify values based on median first derivative
#' values between changepoints
#'
#'
#' @export
falling_limb <- function(data, method = c("simple", "hmm", "cp"), value_col, first_deriv, group_col){
  if (!"data.frame" %in% class(data)) stop("data must be a data.frame or a tibble")

  # value_col <- rlang::enquo(value_col)
  # first_deriv <- rlang::enquo(first_deriv)
  # group_cols <- rlang::enquos(...)

  if (method == "simple"){
    out <- fl_class_simple(data, value_col = {{value_col}}, first_deriv = {{first_deriv}}, group_col = {{group_col}})
  } else if (method == "hmm"){
    out <- fl_class_hmm(data, value_col = {{value_col}}, first_deriv = {{first_deriv}}, group_col = {{group_col}})
  } else if (method == "cp"){
    out <- fl_class_cp(data, value_col = {{value_col}}, first_deriv = {{first_deriv}}, group_col = {{group_col}})
  }

  return(out)
}


fl_class_hmm <- function(data, value_col, first_deriv, group_col){
  # first_deriv <- rlang::enquo(first_deriv)
  # group_cols <- rlang::enquos(...)

  data <- dplyr::group_nest(data, dplyr::across({{group_col}}))

  out <- data %>%
    dplyr::mutate(
      data = purrr::map(data, ~{
        .x <- .x %>%
          dplyr::mutate(x = {{first_deriv}})

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


  return(out)


}


fl_class_simple <- function(data, value_col, first_deriv, group_col){
  # value_col <- rlang::enquo(value_col)
  # first_deriv <- rlang::enquo(first_deriv)
  # group_cols <- rlang::enquos(...)

  # if (!is.null(group_col)) {
  #   data <- dplyr::group_by(data, dplyr::across({{group_col}}))
  # }


  output <- data %>%
    dplyr::group_by(dplyr::across({{group_col}})) %>%
    dplyr::mutate(
      falling_limb = dplyr::case_when(
        {{first_deriv}} < 0 & lag({{first_deriv}}) < 0 ~ "Falling Limb",
        TRUE ~ "Other"
      ),
    )

  output <- dplyr::ungroup(output)

  return(output)
}


fl_class_cp <- function(data, value_col, first_deriv, group_col){
  # value_col <- rlang::enquo(value_col)
  # first_deriv <- rlang::enquo(first_deriv)
  # group_cols <- rlang::enquos(...)

  data <- dplyr::group_nest(data, dplyr::across({{group_col}}))

  out <- data %>%
    dplyr::mutate(
      data = purrr::map(data, ~{
        .x <- .x %>%
          dplyr::mutate(x = {{first_deriv}})

        bps <- strucchange::breakpoints(.x$x ~ 1, breaks = 5)

        bp_tbl <- tibble(
          bp = c(1, bps$breakpoints),
          bp_num = seq_along(c(1, bps$breakpoints))
        )

        out <- .x %>%
          dplyr::mutate(row = dplyr::row_number()) %>%
          dplyr::left_join(
            bp_tbl, by = c("row" = "bp")
          ) %>%
          tidyr::fill(bp_num, .direction = "down") %>%
          dplyr::group_by(bp_num) %>%
          dplyr::mutate(bp_group_median = median(x, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(falling_limb = ifelse(bp_group_median < 0, "Falling Limb", "Other")) %>%
          dplyr::select(-c(x, row, bp_num, bp_group_median))

        return(out)
      })
    ) %>%
    tidyr::unnest(data)


  return(out)

}

