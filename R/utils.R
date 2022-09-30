#' @keywords internal
get_assessment_date <- function(.data) {
  dplyr::pull(
    dplyr::mutate(.data, tmp = purrr::map(.x = .data$.assessment, .f = ~ dplyr::pull(dplyr::select(.x, 1))))
  ) |>
    as.double() |>
    lubridate::as_date()
}

#' Test if an object is quadratic
#'
#' @param x An object to be tested.
#'
#' @return A flag (`TRUE` or `FALSE`)
#'
#' @keywords internal
is_quadratic <- function(x) {
  NROW(x) == NCOL(x)
}

#' @keywords internal
tbl_to_mtx <- function(x) as.matrix(dplyr::select(x, where(is.numeric)))

#' @keywords internal
get_index_col <- function(.data) {
  index_col <- purrr::map_lgl(.x = .data, .f = lubridate::is.Date)
  if (is.null(index_col)) {
    rlang::abort("No index was found in the object provided.")
  }

  .data[index_col]

}

#' @keywords internal
get_index_char <- function(.data) {

  .tbl_names <- names(get_index_col(.data))

  if (is.null(.tbl_names)) {
    warning('No name found for the index column. \n',
            'Column name being arbitrarily set to `.date`',
            immediate. = TRUE)
    .tbl_names <- '.date'
  }

  .tbl_names

}

#' @keywords internal
get_index_quo <- function(.data, .index = get_index_char(.data)) {
  rlang::quo(.index)
}


# Kurtosis ----------------------------------------------------------------


#' Kurtosis of a Time-series
#'
#' This function computes the kurtosis of a given data set. It was written for
#' internal computations.
#'
#' The method can be either \code{"moment"}, \code{"fisher"}, or \code{"excess"}.
#'
#' @param .invariant An univariate or multivariate time series.
#' @param .method A \code{character} string. One of: \code{"excess"}, \code{"moment"} or \code{"fisher"}.
#' @param .na_rm A \code{logical} value. Should missing values be removed? The default is \code{FALSE}.
#'
#' @return A tidy \code{tibble} with 2 columns.
#'
#' @keywords internal
kurtosis <- function(.invariant, .method = c("excess", "moment", "fisher"), .na_rm = FALSE) {

  method <- match.arg(.method, choices = c("excess", "moment", "fisher"))
  assertthat::assert_that(assertthat::is.flag(.na_rm))


  if (!('tbl' %in% class(.invariant))) {
    .invariant <- tibble::as_tibble(.invariant)
  }

  # if none is numeric, abort
  if (all(!purrr::map_lgl(.invariant, is.numeric))) {
    rlang::abort('At least one column must be numeric.')
  }

  # Remove NAs:
  if (.na_rm) {
    .invariant <- stats::na.omit(.invariant)
  }

  # Methods
  if (method == "excess") {

    kurtosis <- purrr::map_dbl(
      .x = .invariant,
      .f = ~ sum((.x - mean(.x)) ^ 4 / as.numeric(stats::var(.x)) ^ 2) / length(.x) - 3
    )

  } else if (method == "moment") {

    kurtosis <- purrr::map_dbl(
      .x = .invariant,
      .f = ~ sum((.x - mean(.x)) ^ 4 / as.numeric(stats::var(.x))  ^ 2) / length(.x)
    )

  } else if (method == "fisher") {
    n <- NROW(.invariant)
    kurtosis <- purrr::map_dbl(
      .x = .invariant,
      .f = ~ ((n + 1) * (n - 1) * ((sum(.x ^ 4) / n) / (sum(.x ^ 2) / n) ^ 2 - (3 * (n - 1)) / (n + 1))) / ((n - 2) * (n - 3))
    )

  }

  kurtosis

}




# Skewness ----------------------------------------------------------------

#' Skewness of a Time-series
#'
#' This function computes the kurtosis of a given data set. It was written for
#' internal computations.
#'
#' @inheritParams kurtosis
#'
#' @return A tidy \code{tibble} with 2 columns.
#'
#' @keywords internal
skewness <- function(.invariant, .method = c("moment", "fisher"), .na_rm = FALSE) {

  method <- match.arg(.method, choices = c("moment", "fisher"))
  assertthat::assert_that(assertthat::is.flag(.na_rm))

  if (!('tbl' %in% class(.invariant))) {
    .invariant <- tibble::as_tibble(.invariant)
  }

  # # if none is numeric, abort
  # if (all(!purrr::map_lgl(.invariant, is.numeric))) {
  #   stop('At least one column must be numeric', call. = FALSE)
  # }

  # Remove NAs:
  if (.na_rm) {
    .invariant <- stats::na.omit(.invariant)
  }

  # Skewness:
  n <- NROW(.invariant)

  # Selected Method:
  if (method == "moment") {

    skewness <- purrr::map_dbl(
      .x = .invariant,
      .f = ~ sum((.x - mean(.x)) ^ 3 / sqrt(as.numeric(stats::var(.x))) ^ 3) / length(.x)
    )

  } else if (method == "fisher") {

    if (n < 3) {
      skewness <- NA_real_
    } else {
      skewness <- purrr::map_dbl(
        .x = .invariant,
        .f = ~ ((sqrt(n  * (n - 1)) / (n - 2)) * (sum(.x ^ 3) / n)) / ((sum(.x ^ 2) / n) ^ (3  / 2))
      )
    }

  }

  skewness

}

#' @keywords internal
es <- function(.data, .level = 0.05) mean(.data[.data <= stats::quantile(.data, .level)])
