#' Minimum Variance Objective Function
#'
#' Constructs a quadratic objective function.
#'
#' @param .data A variance-covariance matrix.
#'
#' @return A \code{list} of the \code{objective} class.
#'
#' @export
#'
#' @examples
#' x <- diff(log(EuStockMarkets))
#'
#' minimum_variance(.data = cov(x))
minimum_variance <- function(.data) {
  if (!is_quadratic(.data)) {
    rlang::abort("`.sigma` must be a square matrix.")
  }
  ROI::Q_objective(Q = 2 * .data, L = rep(0, NCOL(.data)))
}

#' Mean-Variance Objective Function
#'
#' Constructs a quadratic objective function.
#'
#' @param .data A \code{list} of length 2 with the location and dispersion parameters.
#' @param .lambda  A \code{double} with the risk-aversion parameter.
#'
#' @return A \code{list} of the \code{objective} class.
#' @export
#'
#' @examples
#' x <- diff(log(EuStockMarkets))
#'
#' mean_variance(list(mu = colMeans(x), sigma = cov(x)), 1)
mean_variance <- function(.data, .lambda) {
  assertthat::assert_that(assertthat::is.scalar(.lambda))
  if (!is.list(.data)) {
    rlang::abort("`.data` must be a list of length 2 with the location and dispersion parameters.")
  }
  if (!is_quadratic(.data[[2]])) {
    rlang::abort("`.sigma` must be a square matrix.")
  }
  ROI::Q_objective(Q = -.lambda * 2 * .data[[2]], L = .data[[1]])
}


#' Risk-Parity Objective Function
#'
#' Constructs a risk-parity objective function.
#'
#' @param .data A variance-covariance matrix.
#'
#' @return A \code{list} of the \code{objective} class.
#' @export
#'
#' @examples
#' x <- diff(log(EuStockMarkets))
#'
#' risk_parity(cov(x))
risk_parity <- function(.data) {

  if (!is_quadratic(.data)) rlang::abort("`.data` must be a square matrix.")

  n     <- ncol(.data)
  rcs   <- rep(1 / n, n)

  ROI::F_objective(
    F = function(x) drop(0.5 * crossprod(x, 2 * .data) %*% x - crossprod(rcs, log(x))),
    n = n,
    G = function(x) crossprod(2 * .data, x) - rcs / x,
    H = function(x) 2 * .data + diag(rcs / as.vector(x) ^ 2)
  )

}
