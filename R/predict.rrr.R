#' Predict Method for Reduced Rank Regression
#'
#' Computes predicted values for a fitted \code{"rrr"} object, optionally on
#' new data.
#'
#' @param object An object of class \code{"rrr"}, as returned by
#'   \code{\link{rrr}}.
#' @param newdata An optional data frame in which to look for variables with
#'   which to predict. If \code{NULL} (default), predictions are computed on
#'   the training data, using the stored design matrix (\code{x}) if available,
#'   or reconstructing it from the stored model frame (\code{model}).
#' @param ... Further arguments passed to or from other methods. Currently
#'   unused.
#'
#' @return A numeric matrix of predicted values with the same number of columns
#'   as the response matrix used during fitting. Row names reflect the rows of
#'   \code{newdata} (or the training data if \code{newdata = NULL}); column
#'   names correspond to the response variables.
#'
#' @details
#' When \code{newdata = NULL}, the method first checks for a stored design
#' matrix (\code{object$x}, available when \code{rrr} was called with
#' \code{x = TRUE}), then falls back to reconstructing the design matrix from
#' the stored model frame (\code{object$model}, available when \code{rrr} was
#' called with \code{model = TRUE}, which is the default). If neither is
#' present, an error is raised.
#'
#' When \code{newdata} is supplied, the design matrix is constructed via
#' \code{\link[stats]{model.frame}} and \code{\link[stats]{model.matrix}}
#' using the \code{terms} stored in \code{object}. This correctly handles
#' any transformations encoded in the original formula (e.g., \code{log(x)},
#' \code{poly(x, 2)}).
#'
#' @seealso \code{\link{rrr}} for model fitting.
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
#' fit <- rrr(cbind(y1, y2, y3) ~ x1 + x2 + x3 + x4 + x5,
#'            data = dat, rank = 2)
#'
#' # In-sample predictions
#' pred_train <- predict(fit)
#'
#' # Out-of-sample predictions on new data
#' newdat <- as.data.frame(matrix(rnorm(10 * 5), 10, 5))
#' names(newdat) <- c("x1", "x2", "x3", "x4", "x5")
#' pred_new <- predict(fit, newdata = newdat)
#' @importFrom stats model.frame model.matrix delete.response
#' @author Mathijs Deen
#' @export
predict.rrr <- function(object, newdata = NULL, ...){
  if (is.null(newdata)) {
    if (!is.null(object$x)) {
      X_new <- object$x
    } else if (!is.null(object$model)) {
      X_new <- model.matrix(object$terms, object$model)
    } else {
      stop("No data available for prediction. Refit with model = TRUE or x = TRUE.")
    }
  } else {
    tt <- delete.response(object$terms)
    X_new <- model.matrix(tt, model.frame(tt, newdata))
  }
  return(X_new %*% object$BA)
}
