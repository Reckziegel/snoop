#' Portfolio Statistics
#'
#' Computes the portfolio statistics for each period being considered.
#'
#' @param .data A \code{tibble} from the \code{snoop_rolling}.
#' @param .cost A code{double} with the fixed cost per period of time.
#'
#' @return A \code{tibble} with 8 new columns: \code{.weights}, \code{.return} and \code{.volatility},
#' \code{.skewness} and \code{.kurtosis}, \code{.value_at_risk}, \code{.expected_shortfall} and
#' \code{.ena}.
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
#' compute_cov <- function(.data) stats::cov(as.matrix(.data))
#'
#' optimal <- rebalance_portfolio(rebal, compute_cov, .strategy = "risk_parity")
#' optimal
#'
#' extract_statistics(optimal)
extract_statistics <- function(.data, .cost = 0) {

  assertthat::assert_that(assertthat::is.number(.cost))

  if (inherits(.data, "snoop_rolling")) {

    sts <- suppressMessages(
      suppressWarnings(
        .data |>
          dplyr::mutate(
            .weights      = purrr::map(.x = .data$.optimization, .f = "solution"),
            .var          = purrr::map(.x = .data$.analysis, .f = stats::var),
            .return_gross = purrr::map2_dbl(.x = .data$.assessment, .y = .data$.weights, .f = ~ as.matrix(.x) %*% as.matrix(.y)),
            .return_net   = purrr::map2_dbl(.x = .data$.return_gross, .y = .cost, .f = ~ .x - .y),
            .volatility   = purrr::map2_dbl(.x = .data$.weights, .y = .data$.var, .f = ~ sqrt(.x %*% .y %*% .x)),
            .skewness     = purrr::map2_dbl(.x = .data$.analysis, .y = .data$.weights, .f = ~ skewness(as.matrix(.x) %*% .y)),
            .kurtosis     = purrr::map2_dbl(.x = .data$.analysis, .y = .data$.weights, .f = ~ kurtosis(as.matrix(.x) %*% .y)),
            .value_at_risk = purrr::map2_dbl(.x = .data$.analysis, .y = .data$.weights, .f = ~ stats::quantile(as.matrix(.x) %*% .y, 0.05)),
            .expected_shortfall = purrr::map2_dbl(.x = .data$.analysis, .y = .data$.weights, .f = ~ es(as.matrix(.x) %*% .y, 0.05)),
            .ena          = purrr::map_dbl(.x = .data$.weights, .f = ~ 1 / sum(.x))
          ) |>
          dplyr::select(-.data$.var)
      )
    )

  } else {

    rlang::abort("`.data` must be an object from the `snoop_rolling` class")

  }

  tibble::new_tibble(x = sts, nrow = nrow(sts), class = "snoop_statistics")

}


