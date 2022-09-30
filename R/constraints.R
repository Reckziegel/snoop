#' Budget Constraint
#'
#' Helper that creates a budget constraint for portfolio optimization.
#'
#' @param .data A tabular (non-tidy) \code{.tibble}.
#' @param .investment A \code{number} with the percentage of wealth that should
#' be allocated to risky assets.
#'
#' @return Not defined yet.
#' @export
#'
#' @examples
#' constraint_budget(EuStockMarkets, 1)
constraint_budget <- function(.data, .investment = 1) {
  UseMethod("constraint_budget", .data)
}

#' @rdname constraint_budget
#' @export
constraint_budget.default <- function(.data, .investment = 1) {
  rlang::abort("`constraint_budget` must be a matrix, xts or tibble.")
}

#' @rdname constraint_budget
#' @export
constraint_budget.matrix <- function(.data, .investment = 1) {
  constraint_budget_(.data = .data, .investment = .investment)
}

#' @rdname constraint_budget
#' @export
constraint_budget.xts <- function(.data, .investment = 1) {
  constraint_budget_(.data = as.matrix(.data), .investment = .investment)
}

#' @rdname constraint_budget
#' @export
constraint_budget.tbl_df <- function(.data, .investment = 1) {
  constraint_budget_(.data = tbl_to_mtx(.data), .investment = .investment)
}

#' @keywords internal
constraint_budget_ <- function(.data, .investment) {
  assertthat::assert_that(assertthat::is.number(.investment))
  # if (rlang::is_empty(get_index_col(.data))) {
  #   .data <- .data |>
  #     dplyr::select(where(is.numeric))
  # }
  ROI::L_constraint(L = rep(1, ncol(.data)), dir = "==", rhs = .investment)
}


#' Budget Constraint
#'
#' Helper that creates a box constraint for portfolio optimization.
#'
#' @param .data A tabular (non-tidy) \code{.tibble}.
#' @param .min A \code{number} with the minimum allowed allocation per asset.
#' @param .max A \code{number} with the maximum allowed allocation per asset.
#'
#' @return Not defined yet.
#' @export
#'
#' @examples
#' mtx <- matrix(EuStockMarkets, ncol = 4)
#' colnames(mtx) <- colnames(EuStockMarkets)
#'
#' #constraint_box(mtx, .min = 0, .max = 1)
constraint_box <- function(.data, .min, .max) {
  UseMethod("constraint_box", .data)
}

#' @rdname constraint_box
#' @export
constraint_box.default <- function(.data, .min, .max) {
  rlang::abort("`constraint_budget` must be a matrix, xts or tibble.")
}

#' @rdname constraint_box
#' @export
constraint_box.matrix <- function(.data, .min, .max) {
  constraint_box_(.data = .data, .min = .min, .max = .max)
}

#' @rdname constraint_box
#' @export
constraint_box.xts <- function(.data, .min, .max) {
  constraint_box_(.data = as.matrix(.data), .min = .min, .max = .max)
}

#' @rdname constraint_box
#' @export
constraint_box.tbl_df <- function(.data, .min, .max) {
  constraint_box_(.data = tbl_to_mtx(.data), .min = .min, .max = .max)
}

#' @keywords internal
constraint_box_ <- function(.data, .min, .max) {
  .n <- NCOL(.data)
  rbind(
    ROI::L_constraint(
      L = diag(.n), dir = rep(">=", .n), rhs = rep(.min, .n), names = names(.data)),
    ROI::L_constraint(
      L = diag(.n), dir = rep("<=", .n), rhs = rep(.max, .n), names = names(.data))
  )
}


#' Turnover Constraint
#'
#' Helper that creates a turnover constraint for portfolio optimization.
#'
#' @param .data A tabular (non-tidy) \code{.tibble}.
#' @param .bench A \code{numeric} vector with the weights of the benchmark
#' allocation.
#' @param .turn A \code{number} with the maximum allowed turnover.
#'
#' @return Not defined yet.
#' @export
#'
#' @examples
#' constraint_turnover(EuStockMarkets, .turn = 10)
constraint_turnover <- function(.data, .bench = NULL, .turn = 100) {
  UseMethod("constraint_turnover", .data)
}

#' @rdname constraint_turnover
#' @export
constraint_turnover.default <- function(.data, .bench = NULL, .turn = 100) {
  rlang::abort("`constraint_turnover` must be a matrix, xts or tibble.")
}

#' @rdname constraint_turnover
#' @export
constraint_turnover.matrix <- function(.data, .bench = NULL, .turn = 100) {
  constraint_turnover_(.data = .data, .bench = .bench, .turn = .turn)
}

#' @rdname constraint_turnover
#' @export
constraint_turnover.matrix <- function(.data, .bench = NULL, .turn = 100) {
  constraint_turnover_(.data = as.matrix(.data), .bench = .bench, .turn = .turn)
}

#' @rdname constraint_turnover
#' @export
constraint_turnover.tbl_df <- function(.data, .bench = NULL, .turn = 100) {
  constraint_turnover_(.data = tbl_to_mtx(.data), .bench = .bench, .turn = .turn)
}

#' @keywords internal
constraint_turnover_ <- function(.data, .bench = NULL, .turn = 100) {
  assertthat::assert_that(assertthat::is.number(.turn))

  .data_nms <- names(.data)
  .n <- ncol(.data)
  .r <- nrow(.data)

  if (is.null(.bench)) {
    .bench <- rep(1 / .n, .n)
  }

  .amat <- cbind(diag(.n), -diag(.n), diag(.n))
  .nms <- c(.data_nms, paste0(".y_plus", seq_len(.n)), paste0(".y_minus", seq_len(.n)))

  rbind(
    ROI::L_constraint(
      L = .amat, dir = rep("==", .n), rhs = .bench
      ),
    ROI::L_constraint(
      L = c(rep(0, .n), rep(1, .n), rep(1, .n)), dir = "<=", rhs = .turn
      )
  )

}

#' Turnover Constraint
#'
#' Helper that creates a turnover constraint for portfolio optimization.
#'
#' @param .data A tabular (non-tidy) \code{.tibble}.
#' @param .card A \code{integer} with the minimum number of assets to be included
#' in the optimization process.
#'
#' @return Not defined yet.
#' @export
#'
#' @examples
#' constraint_cardinality(EuStockMarkets, .card = 2)
constraint_cardinality <- function(.data, .card = 1) {
  UseMethod("constraint_cardinality", .data)
}

#' @rdname constraint_cardinality
#' @export
constraint_cardinality.default <- function(.data, .card = 1) {
  rlang::abort("`constraint_cardinality` must be a matrix, xts or tibble.")
}

#' @rdname constraint_cardinality
#' @export
constraint_cardinality.matrix <- function(.data, .card = 1) {
  constraint_cardinality_(.data = .data, .card = .card)
}

#' @rdname constraint_cardinality
#' @export
constraint_cardinality.xts <- function(.data, .card = 1) {
  constraint_cardinality_(.data = as.matrix(.data), .card = .card)
}

#' @rdname constraint_cardinality
#' @export
constraint_cardinality.tbl_df <- function(.data, .card = 1) {
  constraint_cardinality_(.data = tbl_to_mtx(.data), .card = .card)
}

#' @keywords internal
constraint_cardinality_ <- function(.data, .card = 1) {

  assertthat::assert_that(assertthat::is.number(.card))

  .data_nms <- names(.data)
  .n <- ncol(.data)

  .amat <- cbind(diag(.n), -diag(.n))
  .nms <- c(.data_nms, paste0("z_card_aux", seq_len(.n)))

  rbind(
    ROI::L_constraint(L = .amat, dir = rep("<=", .n), rhs = rep(0, .n)),
    ROI::L_constraint(L = c(rep(0, .n), rep(1, .n)), dir = ">=",  rhs = .card)
    )

}

#' Bounds Constraint
#'
#' @param .data A tabular (non-tidy) \code{.tibble}.
#' @param .min A number with the minimum allowed weight.
#' @param .max A number with the maximum allowed weight.
#'
#' @return A list.
#' @export
#'
#' @examples
#' constraint_bounds(EuStockMarkets, 0, 1)
constraint_bounds <- function(.data, .min, .max) {
  UseMethod("constraint_bounds", .data)
}

#' @rdname constraint_bounds
#' @export
constraint_bounds.default <- function(.data, .min, .max) {
  rlang::abort("`constraint_cardinality` must be a matrix, xts or tibble.")
}

#' @rdname constraint_bounds
#' @export
constraint_bounds.default <- function(.data, .min, .max) {
  constraint_bounds_(.data = .data, .min =.min, .max = .max)
}

#' @rdname constraint_bounds
#' @export
constraint_bounds.matrix <- function(.data, .min, .max) {
  constraint_bounds_(.data = .data, .min = .min, .max = .max)
}

#' @rdname constraint_bounds
#' @export
constraint_bounds.xts <- function(.data, .min, .max) {
  constraint_bounds_(.data = as.matrix(.data), .min = .min, .max = .max)
}

#' @rdname constraint_bounds
#' @export
constraint_bounds.tbl_df <- function(.data, .min, .max) {
  constraint_bounds_(.data = tbl_to_mtx(.data), .min = .min, .max = .max)
}

#' @keywords internal
constraint_bounds_ <- function(.data, .min, .max) {
  assertthat::assert_that(assertthat::is.number(.min))
  assertthat::assert_that(assertthat::is.number(.max))
  .n <- ncol(.data)
  ROI::V_bound(lb = rep(.min, .n), ub = rep(.max, .n))
}

