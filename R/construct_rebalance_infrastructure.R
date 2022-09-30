#' Build a Rebalance Infrastrucure
#'
#' Prepares a \code{snoop_rolling} object before passing it through \code{\link{rebalance_portfolio}}.
#'
#' @param .data A tabular (non-tidy) \code{tibble}.
#' @param .by A \code{string} with the periodicity to compute the rebalance. One of: \code{day}, \code{week}, \code{month} or \code{year}.
#'
#' @return A \code{tibble} of the \code{snoop_rebalance} class. Contains 3 columns:
#' \code{.date}, \code{.analysis} and \code{.assessment}.
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
#' const <- construct_rebalance_infrastructure(roll)
#' const
#'
#' # Contains information under the hood:
#' class(const)
construct_rebalance_infrastructure <- function(.data, .by = c("day", "week", "month", "year")) {

  .by <- rlang::arg_match(.by, c("day", "week", "month", "year"))

  .index_col  <- get_index_col(.data)
  .index_char <- get_index_char(.data)

  if (purrr::is_empty(.index_char)) {
    rlang::abort(message = "At least one column from `.data` must contain dates.")
  }

  att <- .data |>
    dplyr::mutate(
      .day   = lubridate::day(.data[[.index_char]]),
      .week  = lubridate::week(.data[[.index_char]]),
      .month = lubridate::month(.data[[.index_char]]),
      .year  = lubridate::year(.data[[.index_char]]),
      .flag  = dplyr::if_else(.data[[paste0(".", .by)]] == dplyr::lag(.data[[paste0(".", .by)]]), FALSE, TRUE)
      )

  att$.flag[[1]] <- FALSE

  n_col <- ncol(.data$.analysis[[1]])

  tibble::new_tibble(x = .data, nrow = nrow(.data), class = "snoop_rebalance", anexo = att, n_col = n_col)

}
