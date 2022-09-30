
#' Autoplot Method
#'
#' A quick visual inspection of objects from the \code{snoop_statistics} class.
#'
#' @param .data An object of the \code{snoop_statistics} class.
#'
#' @importFrom ggplot2 autoplot
#'
#' @return A \code{ggplo2} object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' stocks <- tibble::tibble(
#'   time = as.Date('2009-01-01') + 0:99,
#'   X    = rnorm(100, 0, 1),
#'   Y    = rnorm(100, 0, 2),
#'   Z    = rnorm(100, 0, 4)
#' )
#'
#' roll <- construct_rolling_infrastructure(stocks, .initial = 50)
#' rebal <- construct_rebalance_infrastructure(roll)
#'
#' # Risk-Parity Strategy
#' compute_cov <- function(.data) stats::cov(as.matrix(.data))
#'
#' rebalance_portfolio(rebal, compute_cov, .strategy = "risk_parity") |>
#'   extract_statistics() |>
#'   autoplot()
autoplot.snoop_statistics <- function(.data) {
  .data |>
    tidyr::pivot_longer(cols = c(.data$.return_gross:.data$.ena)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$.date, y = .data$value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~.data$name, scales = "free") +
    ggplot2::labs(x = NULL, y = NULL)
}

