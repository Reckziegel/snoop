#' A Rebalance Structure
#'
#' @param .data A tabular (non-tidy) \code{tibble}.
#' @param .fn A function to compute the optimization strategy
#' @param .strategy A \code{character} with the optimization technique to be implemented.
#' @param ... Additional arguments to be passed to \code{.fn}.
#'
#' @return A \code{tibble}.
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
#' rebalance_portfolio(rebal, mu_sigma, .strategy = "mean_variance")
#'
#' # Risk Parity Strategy
#' compute_cov <- function(.data) stats::cov(as.matrix(.data))
#'
#' rebalance_portfolio(rebal, compute_cov, .strategy = "risk_parity")
rebalance_portfolio <- function(.data, .fn, ..., .strategy = c("risk_parity", "mean_variance")) {

  .strategy <- rlang::arg_match(.strategy, c("risk_parity", "mean_variance"))
  .fn <- purrr::as_mapper(.fn, ...)

  if (inherits(.data, "snoop_rebalance")) {

    # segment code by strategy
    if (.strategy == "risk_parity") {

      tmp <- attributes(.data)$anexo |>
        dplyr::mutate(
          .cov = purrr::map_if(
            .x    = .data$.analysis,
            .p    = .data$.flag,
            .f    = .fn,
            .else = as.null),
          .weights = purrr::map_if(
            .x = .data$.cov,
            .p = .data$.flag,
            .f = risk_parity,
            .else = as.null)
        )

      tmp$.cov[[1]]     <- .fn(tmp$.analysis[[1]])
      tmp$.weights[[1]] <- risk_parity(tmp$.cov[[1]])

      for (i in 2:NROW(tmp)) {
        if (is.null(tmp$.weights[[i]])) {
          tmp$.weights[[i]] <- tmp$.weights[[i - 1]]
        }
      }

    } else if (.strategy == "mean_variance") {

      tmp <- attributes(.data)$anexo |>
        dplyr::mutate(
          .moment = purrr::map_if(
            .x    = .data$.analysis,
            .p    = .data$.flag,
            .f    = .fn,
            .else = as.null),
          .weights = purrr::map_if(
            .x = .data$.moment,
            .p = .data$.flag,
            .f = ~ mean_variance(.moments = .x, .wmin = 0, .wmax = 1),
            .else = as.null)
        )

      tmp$.moment[[1]]  <- .fn(tmp$.analysis[[1]])
      tmp$.weights[[1]] <- mean_variance(tmp$.moment[[1]], .wmin = 0, .wmax = 1)

      for (i in 2:NROW(tmp)) {
        if (is.null(tmp$.weights[[i]])) {
          tmp$.weights[[i]] <- tmp$.weights[[i - 1]]
        }
      }

    } else {
      rlang::abort("The chosen `.strategy` is currently not implemented.
                         Try `risk_parity` or `mean_variance` instead")
    }



  } else {
    rlang::abort("`.data` must be an object from the `snoop_rebalance` class.")
  }

  tmp |>
    dplyr::select(.data$.date:.data$.assessment, .data$.weights)

}
