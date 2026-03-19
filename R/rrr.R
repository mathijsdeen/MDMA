#' Reduced Rank Regression
#'
#' Fits a reduced rank regression model, estimating a coefficient matrix of
#' rank \code{rank} that best approximates the full-rank OLS solution under
#' a Frobenius norm loss. The predictors are whitened prior to SVD, ensuring
#' that the low-rank approximation is optimal with respect to the original
#' loss rather than the raw coefficient geometry.
#'
#' @param formula A two-sided \code{\link[stats]{formula}} with a multivariate
#'   response on the left-hand side and predictors on the right-hand side.
#'   The response will be coerced to a matrix via \code{\link[stats]{model.response}}.
#' @param data A data frame containing the variables referenced in
#'   \code{formula}.
#' @param rank Integer. The desired rank of the coefficient matrix. Must
#'   satisfy \code{1 <= rank <= min(ncol(Y), ncol(X))}. Not used when
#'   \code{method = "model.frame"}.
#' @param na.action A function specifying how missing values are handled.
#'   Defaults to \code{\link[stats]{na.omit}}.
#' @param model Logical. If \code{TRUE} (default), the model frame is stored
#'   in the returned object. Always included when \code{method = "model.frame"}.
#' @param x Logical. If \code{TRUE}, the design matrix \code{X} is stored in
#'   the returned object. Defaults to \code{FALSE}.
#' @param y Logical. If \code{TRUE}, the response matrix \code{Y} is stored in
#'   the returned object. Defaults to \code{FALSE}.
#' @param method Character string specifying the fitting method. Use
#'   \code{"rrr.fit"} (default) for standard reduced rank regression, or
#'   \code{"model.frame"} to return the model frame (and optionally \code{X}
#'   and \code{Y}) without fitting.
#'
#' @return If \code{method = "model.frame"}, a list containing:
#'   \describe{
#'     \item{model}{The \code{\link[stats]{model.frame}} constructed from
#'       \code{formula}, \code{data}, and \code{na.action}.}
#'     \item{x}{The design matrix. Only present if \code{x = TRUE}.}
#'     \item{y}{The response matrix. Only present if \code{y = TRUE}.}
#'   }
#'   Otherwise, an object of class \code{"rrr"}, which is a list containing:
#'   \describe{
#'     \item{BA}{A \code{p x q} numeric matrix of rank \code{rank}, where
#'       \code{p} is the number of predictors (including any intercept) and
#'       \code{q} is the number of response variables. Row names correspond
#'       to predictor names, column names to response names.}
#'     \item{terms}{The \code{\link[stats]{terms}} object from the model frame,
#'       retained for use by predict and print methods.}
#'     \item{na.action}{The \code{na.action} attribute of the model frame,
#'       indicating which observations were removed due to missing values.
#'       \code{NULL} if no observations were removed.}
#'     \item{model}{The model frame. Only present if \code{model = TRUE}.}
#'     \item{x}{The design matrix. Only present if \code{x = TRUE}.}
#'     \item{y}{The response matrix. Only present if \code{y = TRUE}.}
#'   }
#'
#' @details
#' The coefficient matrix \eqn{\mathbf{B}\mathbf{A}'} is found by:
#' \enumerate{
#'   \item Computing the symmetric square root \eqn{\mathbf{S} = (\mathbf{X}'\mathbf{X})^{1/2}}
#'     via eigendecomposition.
#'   \item Whitening the design matrix as \eqn{\mathbf{Z} = \mathbf{X}\mathbf{S}^{-1}},
#'     so that \eqn{\mathbf{Z}'\mathbf{Z} = \mathbf{I}}.
#'   \item Computing the truncated SVD of \eqn{\mathbf{Z}'\mathbf{Y}}, retaining
#'     the top \code{rank} components.
#'   \item Back-transforming via \eqn{\mathbf{B} = \mathbf{S}^{-1}\mathbf{U}\sqrt{n}}
#'     and \eqn{\mathbf{A} = \mathbf{V}\mathbf{D}/\sqrt{n}}.
#' }
#' Whitening is necessary because the Eckart-Young theorem applies to the
#' Frobenius norm of the matrix itself, whereas the regression loss is
#' weighted by \eqn{\mathbf{X}'\mathbf{X}}. Working in whitened coordinates
#' makes the two norms equivalent, so ordinary SVD yields the optimal
#' rank-constrained solution.
#'
#' @seealso
#' \code{\link[stats]{lm}} for ordinary least squares regression.
#'
#' @references
#' Izenman, A. J. (1975). Reduced-rank regression for the multivariate
#' linear model. \emph{Journal of Multivariate Analysis}, \strong{5}(2),
#' 248--264.
#'
#' @examples
#' # Simulate multivariate response data
#' set.seed(42)
#' n <- 100
#' X <- matrix(rnorm(n * 5), n, 5)
#' B_true <- matrix(c(1, 0, 0, 1, 0,
#'                    0, 1, 0, 0, 1,
#'                    0, 0, 1, 0, 0), nrow = 5)
#' Y <- X %*% B_true + matrix(rnorm(n * 3, sd = 0.5), n, 3)
#' dat <- as.data.frame(cbind(Y, X))
#' names(dat) <- c("y1", "y2", "y3", "x1", "x2", "x3", "x4", "x5")
#'
#' # Fit rank-2 reduced rank regression
#' fit <- rrr(cbind(y1, y2, y3) ~ x1 + x2 + x3 + x4 + x5,
#'            data = dat,
#'            rank = 2)
#' fit$BA
#'
#' # Inspect the model frame without fitting
#' rrr(cbind(y1, y2, y3) ~ x1 + x2 + x3 + x4 + x5,
#'     data = dat,
#'     method = "model.frame")
#' @importFrom stats model.frame model.matrix model.response
#' @author Mathijs Deen
#' @export
rrr <- function(formula, data, rank, na.action = na.omit,
                model = TRUE, x = FALSE, y = FALSE,
                method = "rrr.fit"){
  mf <- model.frame(formula = formula,
                    data = data,
                    na.action = na.action)

  mt <- attr(mf, "terms")
  Y <- as.matrix(model.response(mf))
  X <- model.matrix(mt, mf)

  if (identical(method, "model.frame")) {
    out <- list(model = mf)
    if (x) out$x <- X
    if (y) out$y <- Y
    return(out)
  }

  fit <- rrr.fit(X, Y, rank)

  fit$terms     <- mt
  fit$na.action <- attr(mf, "na.action")
  if (model) fit$model <- mf
  if (x)     fit$x     <- X
  if (y)     fit$y     <- Y

  class(fit) <- "rrr"
  return(fit)
}

#' Reduced Rank Regression — core fitter
#'
#' The underlying workhorse for \code{\link{rrr}}, operating directly on
#' numeric matrices rather than formulas. Can be called directly when the
#' design matrix and response matrix are already available, analogous to
#' \code{\link[stats]{glm.fit}}.
#'
#' @param X A numeric design matrix of dimension \code{n x p}, typically
#'   produced by \code{\link[stats]{model.matrix}}. Should include an
#'   intercept column if one is desired.
#' @param Y A numeric response matrix of dimension \code{n x q}.
#' @param m Integer. The desired rank of the coefficient matrix. Must
#'   satisfy \code{1 <= m <= min(p, q)}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{BA}{A \code{p x q} numeric matrix of rank \code{m}. Row names
#'       correspond to columns of \code{X}, column names to columns of
#'       \code{Y}.}
#'   }
#'
#' @details
#' See \code{\link{rrr}} for a full description of the algorithm and the
#' role of whitening in obtaining the optimal rank-constrained solution.
#'
#' @seealso \code{\link{rrr}} for the formula-based interface.
#'
#' @examples
#' set.seed(42)
#' n <- 100
#' X <- cbind(1, matrix(rnorm(n * 4), n, 4))
#' colnames(X) <- c("(Intercept)", "x1", "x2", "x3", "x4")
#' Y <- matrix(rnorm(n * 3), n, 3)
#' colnames(Y) <- c("y1", "y2", "y3")
#'
#' fit <- rrr.fit(X, Y, m = 2)
#' fit$BA
#'
#' @author Mathijs Deen
#' @export
rrr.fit <- function(X, Y, m){
  n <- nrow(Y)
  eig.out <- eigen(t(X) %*% X)
  S <- eig.out$vectors %*% diag(sqrt(eig.out$values)) %*% t(eig.out$vectors)
  Z <- X %*% solve(S)
  C <- t(Z) %*% Y
  C.svd <- svd(C)
  U <- C.svd$u[, 1:m, drop=FALSE]
  D <- diag(C.svd$d[1:m], nrow=m, ncol=m)
  V <- C.svd$v[, 1:m, drop = FALSE]
  B <- solve(S) %*% U * sqrt(n)
  A <- V %*% D / sqrt(n)
  BA <- B %*% t(A)
  colnames(BA) <- colnames(Y)
  rownames(BA) <- colnames(X)
  return(list(BA = BA))
}
