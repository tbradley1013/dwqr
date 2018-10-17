#' Plot the action levels of chlorine results
#'
#' Generate plots showing whether an action level is exceeded
#'
#'
#' @export
plot_al <- function(data, date_col, value_col, ..., method = c("FL", "P"),
                    percentiles = c(.8, .5, .1), rolling_window = 8,
                    max_chlorine = 1.5, date_breaks = "6 months", date_labels = "%b %d, %Y",
                    ylab = "", plot_title = ""){

  if (!"data.frame" %in% class(data)) stop("data must be of class data.frame or tbl")
  req_cols <- c(missing(date_col), missing(value_col))
  if (any(req_cols)) stop("both `date_col` and `value_col` must be specified", call. = FALSE)
  # browser()

  method <- match.arg(method, c("FL", "P"))
  # output_name <- match.arg(output_name, c("AL-C", "AL", "P"))

  value_col <- rlang::enquo(value_col)
  date_col <- rlang::enquo(date_col)
  group_cols <- rlang::enquos(...)

  percentiles <- sort(percentiles, decreasing = TRUE)

  quant_names <- paste0("action_level_", seq_along(percentiles))

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



    plot_data <- data_classed %>%
      dplyr::filter(falling_limb == "Falling Limb") %>%
      dplyr::mutate_at(dplyr::vars(!!value_col), dplyr::funs(!!!quants))

  } else if (method == "P") {
    if (!rlang::is_empty(group_cols)) {
      data <- dplyr::group_by(data, !!!group_cols)
    }

    plot_data <- data %>%
      dplyr::mutate_at(dplyr::vars(!!value_col), dplyr::funs(!!!quants))

  }






}
