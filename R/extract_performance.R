#' Portfolio Statistics
#'
#' Computes the portfolio statistics for each period being considered.
#'
#' @param .data A \code{tibble} from the \code{snoop_rolling}.
#'
#' @return A \code{tibble} with 7 new columns: \code{.weights}, \code{.return} and \code{.volatility},
#' \code{.skewness} and \code{.kurtosis}, \code{.value_at_risk} and \code{.expected_shortfall}.
#'
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
#' roll <- construct_rolling_infrastructure(stocks, .initial = 50)
#'
#' rebal <- construct_rebalance_infrastructure(roll)
#'
#' # Mean Variance Strategy
#' mu_sigma <- function(.data) {
#'   list(mu = colMeans(.data), sigma = stats::cov(.data))
#' }
#'
#' optimal <- rebalance_portfolio(rebal, mu_sigma, .strategy = "mean_variance")
#'
#' extract_statistics(optimal)
extract_statistics <- function(.data) {

  if (inherits(.data, "snoop_rolling")) {

    suppressMessages(
      suppressWarnings(
        .data |>
          dplyr::mutate(
            .weights    = purrr::map(.x = .data$.optimization, .f = "solution"),
            .var        = purrr::map(.x = .data$.analysis, .f = stats::var),
            .return     = purrr::map2_dbl(.x = .data$.assessment, .y = .data$.weights, .f = ~ as.matrix(.x) %*% as.matrix(.y)),
            .volatility = purrr::map2_dbl(.x = .data$.weights, .y = .data$.var, .f = ~ sqrt(.x %*% .y %*% .x)),
            .skewness   = purrr::map2_dbl(.x = .data$.analysis, .y = .data$.weights, .f = ~ skewness(as.matrix(.x) %*% .y)),
            .kurtosis   = purrr::map2_dbl(.x = .data$.analysis, .y = .data$.weights, .f = ~ kurtosis(as.matrix(.x) %*% .y)),
            .value_at_risk = purrr::map2_dbl(.x = .data$.analysis, .y = .data$.weights, .f = ~ stats::quantile(as.matrix(.x) %*% .y, 0.05)),
            .expected_shortfall = purrr::map2_dbl(.x = .data$.analysis, .y = .data$.weights, .f = ~ es(as.matrix(.x) %*% .y, 0.05))
          ) |>
          dplyr::select(-.data$.var)
      )
    )

  } else {

    rlang::abort("`.data` must be an object from the `snoop_rolling` class")

  }


}


