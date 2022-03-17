#' Build a Rolling Infraestructure
#'
#' Constructs a tidy-resampling object for time-series data.
#'
#' @param .data A tabular (non-tidy) \code{tibble}.
#' @param .initial A \code{integer}. The number of samples used for analysis/modeling in the initial resample.
#' @param .assess A \code{integer}. The number of samples used for each assessment resample.
#' @param .cumulative A \code{boolean}. Should the analysis resample grow beyond the size specified by \code{.initial} at each resample?.
#' @param ... Additional arguments to be passed to \code{\link[rsample]{rolling_origin}}.
#'
#' @return A \code{tibble} of the \code{snoop_rolling} class. Contains 3 columns:
#' \code{.date}, \code{.analysis} and \code{.assessment}.
#' @export
#'
#' @examples
#' stocks <- tibble::tibble(
#'   time = as.Date('2009-01-01') + 0:99,
#'   X    = rnorm(100, 0, 1),
#'   Y    = rnorm(100, 0, 2),
#'   Z    = rnorm(100, 0, 4)
#' )
#'
#' construct_rolling_infrastructure(stocks, .initial = 50)
construct_rolling_infrastructure <- function(.data, .initial = 252, .assess = 1, .cumulative = FALSE, ...) {

  assertthat::assert_that(assertthat::is.number(.initial))
  assertthat::assert_that(assertthat::is.number(.assess))
  assertthat::assert_that(assertthat::is.flag(.cumulative))

  roll <- rsample::rolling_origin(
    data       = .data,
    initial    = .initial,
    assess     = .assess,
    cumulative = .cumulative,
    ...
  ) |>
    dplyr::mutate(.analysis   = purrr::map(.x = .data$splits, .f = rsample::analysis),
                  .assessment = purrr::map(.x = .data$splits, .f = rsample::assessment))

  roll <- roll |>
    dplyr::mutate(
      .date       = get_assessment_date(roll),
      .analysis   = purrr::map(.x = .data$.analysis,   .f = ~ dplyr::select(.x, where(is.numeric))),
      .assessment = purrr::map(.x = .data$.assessment, .f = ~ dplyr::select(.x, where(is.numeric)))
    ) |>
    dplyr::select(.data$.date, .data$.analysis, .data$.assessment)

  tibble::new_tibble(x = roll, nrow = nrow(roll), class = "snoop_rolling")

}
