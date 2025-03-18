#' @title Local \eqn{f^2}
#' @description Calculate local \eqn{f^2} for (generalized) linear (mixed) models
#'
#' `r lifecycle::badge("experimental")`
#' @param object a model object (currently supported: `lm`, `glm`, `vglm`).
#' @param method method for calculation of \eqn{R^2}, which is needed for the calculation of \eqn{f^2}. See Details.
#' @param ... currently not used
#'
#' @return \code{f2Local} returns a list containing \eqn{f^2} values for every parameter in a model. For the `glmmTMB`
#' class, a list of all reduced models is returned as well. In a future version, this will be available for other classes as well.
#' @details
#' The following methods can be specified:
#' * `lm` objects: `r.squared` and `adj.r.squared` as extracted from the `lm` object.
#' * `glm` objects: `mcfadden`, `nagelkerke`, `coxsnell`, `tjur` and `efron`, as implemented
#' in the `performance` package.
#' * `vglm` objects: `mcfadden`, `nagelkerke`, `coxsnell`, `tjur` and `efron`, as implemented
#' in the `R2.vglm` function.
#' * `glmmTMB` objects: `nakagawa`, as implemented in the `performance` package. It can also be
#' specified whether the marginal or the conditional \eqn{R^2} should be used, however only the
#' marginal \eqn{R^2} would make sense.
#'
#' Note that for multinomial models, using `method="efron"` gives questionable with `glm` objects and
#' is not possible for `vglm` objects. For `glm` objects, `method=coxsnell` cannot be used when the
#' response is not binary.
#' @importFrom stats as.formula logLik nobs terms update
#' @importFrom performance r2_bayes r2_coxsnell r2_efron r2_ferrari r2_kullback r2_loo r2_loo_posterior r2_mcfadden r2_mckelvey r2_mlm r2_nagelkerke r2_nakagawa r2_posterior r2_somers r2_tjur r2_xu r2_zeroinflated
#' @examples
#' # linear model
#' model1 <- lm(mpg ~ cyl + wt*drat, data = mtcars)
#' f2Local(model1)
#'
#' # generalized linear mixed model
#' if(require(ClusterBootstrap) & require(glmmTMB)){
#'   model2 <- glmmTMB(pos ~ treat*time + (1 + time | id), data = medication)
#'   f2Local(model2)
#' }
#'
#' @author Mathijs Deen
#' @export
f2Local <- function(object, method, ...){
  UseMethod("f2Local")
}

#' @export
#' @describeIn f2Local Method for `lm` object
#' @method f2Local lm
f2Local.lm <- function(object, method = "r.squared", ...){
  valid_methods <- c("r.squared", "adj.r.squared")
  if(!method %in% valid_methods) stop("invalid method.", call. = FALSE)

  R2full <- summary(object)[[method]]
  preds <- attr(terms(object), which = "term.labels")
  n_preds <- length(preds)
  R2preds <- rep(NA_real_, n_preds)

  reduced_models <- vector("list", n_preds)
  names(reduced_models) <- preds

  for(pred in seq_along(preds)){
    reduced_model <- update(object,
                            formula = as.formula(paste(". ~ . -", preds[pred])))
    reduced_models[[pred]] <- reduced_model
    R2preds[pred] <- reduced_model |>
      summary() |>
      (\(s) s[[method]])()
  }

  f2s <- (R2full - R2preds) / (1 - R2full)
  out <- list(variable = preds,
              f2Local  = f2s,
              reduced_models = reduced_models)
  class(out) <- "f2Local"
  return(out)
}

#' @export
#' @describeIn f2Local Method for `glm` object
#' @method f2Local glm
f2Local.glm <- function(object, method = "r2", ...) {
  if(method != "r2") method <- paste0("r2_", method)
  method_fun <- get(method, envir = asNamespace("performance"), inherits = FALSE)

  R2full <- as.numeric(method_fun(object))
  preds <- attr(terms(object), which = "term.labels")
  n_preds <- length(preds)
  R2preds <- rep(NA_real_, n_preds)

  reduced_models <- vector("list", n_preds)
  names(reduced_models) <- preds

  for(pred in seq_along(preds)){
    reduced_model <- update(object,
                            formula = as.formula(paste(". ~ . -", preds[pred])))
    reduced_models[[pred]] <- reduced_model
    R2preds[pred] <- reduced_model |>
      method_fun() |>
      as.numeric()
  }

  f2s <- (R2full - R2preds) / (1 - R2full)
  out <- list(variable = preds,
              f2Local  = f2s,
              reduced_models = reduced_models)
  class(out) <- "f2Local"
  return(out)
}

#' @export
#' @describeIn f2Local Method for `vglm` object
#' @method f2Local vglm
f2Local.vglm <- function(object, method = "mcfadden", ...){
  valid_methods <- c("mcfadden", "nagelkerke", "efron", "coxsnell", "tjur")
  if(!method %in% valid_methods) stop("invalid method.", call.=FALSE)

  R2full <- R2.vglm(object, method = method)

  preds <- attr(terms(object), which = "term.labels")
  n_preds <- length(preds)
  R2preds <- rep(NA_real_, n_preds)

  reduced_models <- vector("list", n_preds)
  names(reduced_models) <- preds

  for(pred in seq_along(preds)){
    reduced_model <- update(object,
                            formula = as.formula(paste(". ~ . -", preds[pred])))
    reduced_models[[pred]] <- reduced_model
    R2preds[pred] <- as.numeric(R2.vglm(reduced_model, method = method))
  }
  f2s <- (R2full - R2preds) / (1 - R2full)
  out <- list(variable = preds,
              f2Local  = f2s,
              reduced_models = reduced_models)
  class(out) <- "f2Local"
  return(out)
}

#' @export
#' @describeIn f2Local Method for `glmmTMB` object
#' @param type indicate whether the marginal (fixed effects only) or the conditional (fixed + random effects)
#' \eqn{R^2} should be used. Default value is `marginal`, using `conditional` might be considered ambiguous.
#' @method f2Local glmmTMB
f2Local.glmmTMB <- function(object, method = "nakagawa", type = "marginal", ...) {
  valid_methods <- c("nakagawa")
  if (!method %in% valid_methods) stop("invalid method.", call. = FALSE)
  if (!type %in% c("marginal", "conditional")) stop("invalid type.", call. = FALSE)
  bb <- `[[`
  whichNakagawa <- paste0("R2_", type)

  ranefStructure <- .extractRandomEffects(object)
  null_model <- .generateNullModel(object)
  R2_full_result <- suppressWarnings(r2_nakagawa(object, null_model = null_model))
  if (is.null(R2_full_result) || is.na(R2_full_result[[whichNakagawa]])) {
    stop("r2_nakagawa() returned NA for the full model. Check model compatibility.")
  }
  R2full <- R2_full_result[[whichNakagawa]]

  preds <- attr(terms(object), which = "term.labels")
  n_preds <- length(preds)
  R2preds <- numeric(n_preds)

  reduced_models <- vector("list", n_preds)
  names(reduced_models) <- preds
  for (i in seq_along(preds)) {
    pred <- preds[i]
    reduced_model <- update(object,
                            formula = as.formula(paste(". ~ . -", pred)),
                            start   = ranefStructure$start,
                            map     = ranefStructure$map)
    reduced_models[[i]] <- reduced_model
    R2_reduced_result <- suppressWarnings(r2_nakagawa(reduced_model, null_model = null_model))
    if (is.null(R2_reduced_result) || is.na(R2_reduced_result[[whichNakagawa]])) {
      stop(paste("r2_nakagawa() returned NA for predictor:", pred))
    }
    R2preds[i] <- R2_reduced_result[[whichNakagawa]] |> as.numeric()
  }
  f2s <- (R2full - R2preds) / (1 - R2full)
  out <- list(variable       = preds,
              f2Local        = f2s,
              reduced_models = reduced_models)
  class(out) <- "f2Local"
  return(out)
}

#' @export
print.f2Local <- function(x, ...){
  out <- data.frame(variable = x$variable,
                    f2       = x$f2Local)
  print(out)
}

#### helpers

.extractRandomEffects <- function(object) {
  thetaValues <- object$fit$par[names(object$fit$par) == "theta"]
  thetaMap <- factor(rep(NA, length(thetaValues)))
  return(list(start = list(theta = thetaValues),
              map   = list(theta = thetaMap)))
}

#' @importFrom stats formula reformulate
#' @importFrom lme4 findbars
.generateNullModel <- function(object) {
  original_formula <- formula(object)
  response_var <- as.character(attr(terms(object), "variables")[[2]])
  random_effects <- findbars(original_formula)
  if (length(random_effects) == 0) {
    stop("No random effects found in the model!")
  }
  random_effects_text <- sapply(random_effects, function(re) paste0("(", deparse(re), ")"))
  null_formula <- reformulate(termlabels = random_effects_text, response = response_var)
  null_model <- update(object, formula = null_formula)
  return(null_model)
}


# To do:
#
# * camel casing
# * calculate f2s with a helper (facilitates testing)

