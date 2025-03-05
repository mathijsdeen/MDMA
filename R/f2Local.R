#' @title Calculate local \eqn{f^2} for (generalized) linear models
#'
#' @param object a model object (currently supported: `lm`, `glm`, `vglm`).
#' @param method method for calculation of \eqn{R^2}, which is needed for the calculation of \eqn{f^2}. See Details.
#' @param ... currently not used
#'
#' @return \code{f2Local} returns a list containing \eqn{f^2} values for every parameter in a model.
#' @details
#' The following methods can be specified:
#' * \code{lm} objects: `r.squared` and `adj.r.squared` as extracted from the `lm` object.
#' * `glm` objects: `mcfadden`, `nagelkerke`, `coxsnell`, `tjur` and `efron`, as implemented
#' in the `performance` package.
#' * `vglm` objects: `mcfadden`, `nagelkerke`, `coxsnell`, `tjur` and `efron`, as implemented
#' in the `R2.vglm` function.
#'
#' Note that for multinomial models, using `method="efron"` gives questionable with `glm` objects and
#' is not possible for `vglm` objects. For `glm` objects, `method=coxsnell` cannot be used when the
#' response is not binary.
#' @importFrom stats as.formula logLik nobs terms update
#' @examples
#' model <- lm(mpg ~ cyl + wt*drat, data = mtcars)
#' @author Mathijs Deen
#' @export
f2Local <- function(object, method, ...){
  UseMethod("f2Local")
}

#' @export
f2Local.lm <- function(object, method = "r.squared", ...){
  valid_methods <- c("r.squared", "adj.r.squared")
  if(!method %in% valid_methods) stop("invalid method.", call. = FALSE)

  R2full <- summary(object)[[method]]
  preds <- attr(terms(object), which = "term.labels")
  R2preds <- rep(NA_real_, length(preds))

  for(pred in seq_along(preds)){
    R2preds[pred] <- object |>
      update(as.formula(paste(". ~ . -", preds[pred]))) |>
      summary() |>
      (\(s) s[[method]])()
  }

  f2s <- (R2full - R2preds) / (1 - R2full)
  out <- list(variable = preds,
              f2Local  = f2s)
  class(out) <- "f2Local"
  return(out)
}

#' @export
f2Local.glm <- function(object, method = "r2", ...) {
  if(method != "r2") method <- paste0("r2_", method)
  method_fun <- get(method, envir = asNamespace("performance"), inherits = FALSE)

  R2full <- as.numeric(method_fun(object))
  preds <- attr(terms(object), which = "term.labels")
  R2preds <- rep(NA_real_, length(preds))

  for(pred in seq_along(preds)){
    R2preds[pred] <- object |>
      update(as.formula(paste(". ~ . -", preds[pred]))) |>
      method_fun() |>
      as.numeric()
  }

  f2s <- (R2full - R2preds) / (1 - R2full)
  out <- list(variable = preds,
              f2Local  = f2s)
  class(out) <- "f2Local"
  return(out)
}

#' @export
f2Local.vglm <- function(object, method = "mcfadden", ...){
  valid_methods <- c("mcfadden", "nagelkerke", "efron", "coxsnell", "tjur")
  if(!method %in% valid_methods) stop("invalid method.", call.=FALSE)

  R2full <- R2.vglm(object, method = method)

  preds <- attr(terms(object), which = "term.labels")
  R2preds <- rep(NA_real_, length(preds))

  for(pred in seq_along(preds)){
    R2preds[pred] <- object |>
      update(as.formula(paste(". ~ . -", preds[pred]))) |>
      R2.vglm(method = method) |>
      as.numeric()
  }
  f2s <- (R2full - R2preds) / (1 - R2full)
  out <- list(variable = preds,
              f2Local  = f2s)
  class(out) <- "f2Local"
  return(out)
}

#################### Next: mixed models! ####################
# Use Nakagawa's R^2, more specifically its marginal R^2.   #
# Try to figure out how to generalize the implementation of #
# the full model's random effects covariance parameters     #
# into the reduced models.                                  #
#                                                           #
# Hic sunt dracones.                                        #
#############################################################
f2Local.glmmTMB <- function(object, method = "nakagawa", type="marginal", ...){
  return(NULL)
}

#' @export
print.f2Local <- function(x, ...){
  out <- data.frame(variable = x$variable,
                    f2       = x$f2Local)
  print(out)
}
