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
