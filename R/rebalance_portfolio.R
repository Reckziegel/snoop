#' A Rebalance Structure
#'
#' @param .data A tabular (non-tidy) \code{tibble}.
#' @param .fn A function to compute the optimization strategy
#' @param .strategy A \code{character} with the optimization technique to be implemented.
#' Currently one of: \code{risk_parity} or \code{mean_variance}.
#' @param ... Additional arguments to be passed to \code{.fn}.
#' @param .constraint doc latter
#' @param .bound doc latter
#' @param .solver doc latter
#' @param .control doc latter
#' @param .lambda doc latter
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' stocks <- tibble::tibble(
#'   time = as.Date('2009-01-01') + 0:99,
#'   X    = stats::rnorm(100, 0, 1),
#'   Y    = stats::rnorm(100, 0, 2),
#'   Z    = stats::rnorm(100, 0, 4)
#' )
#'
#' roll <- construct_rolling_infrastructure(stocks, .initial = 50)
#'
#' rebal <- construct_rebalance_infrastructure(roll, .by = "week")
#'
#' # Mean Variance Strategy
#' #mu_sigma <- function(.data) {
#' #  list(mu = colMeans(.data), sigma = stats::cov(.data))
#' #}
#'
#' #rebalance_portfolio(rebal, mu_sigma, .strategy = "mean_variance")
#'
#' # Risk Parity Strategy
#' compute_cov <- function(.data) stats::cov(as.matrix(.data))
#'
#' rebalance_portfolio(rebal, compute_cov, .strategy = "risk_parity")
rebalance_portfolio <- function(.data, .fn, ..., .strategy, .constraint = NULL, .bound = NULL,
                                .solver = NULL, .control = NULL, .lambda = NULL) {

  if (!inherits(.data, "snoop_rebalance")) {
    rlang::abort("`.data` must be an object from the `snoop_rebalance` class.")
  }

  if (!is.null(.solver)) {
    assertthat::assert_that(assertthat::is.string(.solver))
  }

  if (!is.null(.control)) {
    assertthat::assert_that(is.list(.control), msg = "`.control` must be a list.")
  }
  if (!is.null(.lambda)) {
    assertthat::assert_that(assertthat::is.number(.lambda))
  }

  .fn <- purrr::as_mapper(.fn, ...)

  tmp <- attributes(.data)$anexo |>
    dplyr::mutate(
      .moment = purrr::map_if(
        .x    = .data$.analysis,
        .p    = .data$.flag,
        .f    = .fn,
        .else = as.null)
    )

  # segment code by strategy
  if (.strategy == "risk_parity") {

    n_col <- attributes(.data)$n_col

    tmp <- tmp |>
      dplyr::mutate(
        .optimization = purrr::map_if(
          .x = .data$.moment,
          .p = .data$.flag,
          .f = ~ ROI::ROI_solve(
            x       = ROI::OP(objective = risk_parity(.data = .x), maximum = FALSE),
            solver  = .solver,
            control = list(start = rep(1 / n_col, n_col))
          ),
          .else = as.null),
        .weights = purrr::map(
          .x = .data$.optimization,
          .f = "solution"
        ),
        .weights = purrr::map(.x = .data$.weights, .f = ~ .x / sum(.x))
      )

    tmp$.moment[[1]] <- .fn(tmp$.analysis[[1]])
    tmp$.optimization[[1]] <- ROI::ROI_solve(
      x       = ROI::OP(objective = risk_parity(.data = tmp$.moment[[1]]), maximum = FALSE),
      solver  = .solver,
      control = list(start = rep(1 / n_col, n_col))
    )
    tmp$.weights[[1]] <- tmp$.optimization[[1]]$solution
    tmp$.weights[[1]] <- tmp$.weights[[1]] / sum(tmp$.weights[[1]])

    .n <- NROW(tmp)
    for (i in 2:.n) {
      if (is.null(tmp$.optimization[[i]])) {
        tmp$.optimization[[i]] <- tmp$.optimization[[i - 1]]
        tmp$.weights[[i]] <- tmp$.weights[[i - 1]]
      }
    }

  } else if (.strategy == "mean_variance") {

    tmp <- tmp |>
      dplyr::mutate(
        .optimization = purrr::map_if(
          .x    = .data$.moment,
          .p    = .data$.flag,
          .f    = ~ ROI::ROI_solve(
            ROI::OP(
              objective   = mean_variance(.data = .x, .lambda = .lambda),
              constraints = .constraint,
              bounds      = .bound,
              maximum     = TRUE
            ),
            solver = "quadprog"),
          .else = as.null),
        .weights = purrr::map(
          .x = .data$.optimization,
          .f = "solution"
        )
      )

    tmp$.moment[[1]] <- .fn(tmp$.analysis[[1]])
    tmp$.optimization[[1]] <- ROI::ROI_solve(
      ROI::OP(
        objective   = mean_variance(.data = tmp$.moment[[1]], .lambda = 1),
        constraints = .constraint,
        bounds      = .bound,
        maximum     = TRUE
      ),
      solver = "quadprog")
    tmp$.weights[[1]] <- tmp$.optimization[[1]]$solution

    .n <- NROW(tmp)
    for (i in 2:.n) {
      if (is.null(tmp$.optimization[[i]])) {
        tmp$.optimization[[i]] <- tmp$.optimization[[i - 1]]
        tmp$.weights[[i]] <- tmp$.weights[[i - 1]]
      }
    }

  } else if (.strategy == "minimum_variance") {

    tmp <- tmp |>
      dplyr::mutate(
        .optimization = purrr::map_if(
          .x    = .data$.moment,
          .p    = .data$.flag,
          .f    = ~ ROI::ROI_solve(
            ROI::OP(
              objective   = ~ minimum_variance(.data = .x),
              constraints = .constraint,
              bounds      = .bound,
              maximum     = TRUE
            ),
            solver = "quadprog"),
          .else = as.null),
        .weights = purrr::map(
          .x = .data$.optimization,
          .f = "solution"
        )
      )

    tmp$.moment[[1]] <- .fn(tmp$.analysis[[1]])
    tmp$.optimization[[1]] <- ROI::ROI_solve(
      ROI::OP(
        objective   = ~ minimum_variance(.data = tmp$.moment[[1]]),
        constraints = .constraint,
        bounds      = .bound,
        maximum     = TRUE
      ),
      solver = "quadprog")
    tmp$.weights[[1]] <- tmp$.optimization[[1]]$solution

    .n <- NROW(tmp)
    for (i in 2:.n) {
      if (is.null(tmp$.optimization[[i]])) {
        tmp$.optimization[[i]] <- tmp$.optimization[[i - 1]]
        tmp$.weights[[i]] <- tmp$.weights[[i - 1]]
      }
    }

  } else {

    rlang::abort("The chosen `.strategy` is currently not implemented.
                 Try `risk_parity`, `mean_variance` or `minimum_variance` instead")

  }

  tmp |>
    dplyr::select(.data$.date:.data$.assessment, .data$.optimization, .data$.weights)

}
