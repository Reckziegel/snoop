#' @keywords internal
risk_parity <- function(.sigma, ...) {

  if (inherits(.sigma, "tbl")) {
    .sigma <- .sigma |>
      dplyr::select(where(is.numeric)) |>
      as.matrix()
  }
  # if (inherits(.sigma, "xts")) {
  #     .sigma <- as.matrix(.sigma)
  # }

  if (!is_quadratic(.sigma)) {
    stop("`sigma` must be a square matrix.", call. = FALSE)
  }

  has_names <- colnames(.sigma)

  n     <- ncol(.sigma)
  #sigma <- sigma(.invariant)
  rcs   <- rep(1 / n, n)

  # non-negativity constraint
  non_negativity <- cccp::nnoc(G = -diag(n), h = rep(0, n))

  # Optimization
  opt <- cccp::cccp(
    # Non negativity
    cList = list(non_negativity),
    # Equal weighted portfolio as a first guess
    x0 = rep(1 / n , n),
    # Objective function
    f0 = function(x) drop(0.5 * t(x) %*% (2 * .sigma) %*% x - t(rcs) %*% log(x)),
    # Gradient
    g0 = function(x) t(2 * .sigma) %*% x - rcs / x,
    # Hessian
    h0 = function(x) 2 * .sigma + diag(rcs / as.vector(x) ^ 2),
    # Additional controls passed through the optimization
    optctrl = cccp::ctrl(trace = FALSE, ...)
  )

  weights <- cccp::getx(opt)
  weights_normalized <- as.vector(weights / sum(weights))

  if (!is.null(has_names)) names(weights_normalized) <- has_names

  weights_normalized

}


#' @keywords internal
mean_variance <- function(.moments, .wmin = 0, .wmax = 1) {

  .mu    <- .moments[[1L]]
  .sigma <- .moments[[2L]]

  assertthat::assert_that(is.numeric(.wmin))
  assertthat::assert_that(is.numeric(.wmax))

  num_assets <- ncol(.sigma)

  Aeq  <- matrix(1, 1, num_assets)
  beq  <- 1

  A <- rbind(-diag(num_assets), diag(num_assets))
  b <- c(-if (length(.wmax) == 1L) rep(.wmax, num_assets) else .wmax, if (length(.wmin) == 1L) rep(.wmin, num_assets) else .wmin)

  Amat <- rbind(Aeq, A)
  bvec <- c(beq, b)

  weights <- quadprog::solve.QP(Dmat = 2 * .sigma, dvec = .mu, Amat = t(Amat), bvec = bvec, meq = length(beq))$solution
  weights

}
