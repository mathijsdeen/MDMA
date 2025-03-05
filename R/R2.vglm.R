#' @title Calculate (pseudo) \eqn{R^2} for `vglm` objects
#'
#' @param model a `vglm` object.
#' @param method method for calculation of \eqn{R^2}.
#'
#' @return \code{R2.vglm} returns \eqn{R^2}.
#' @export
#'
#' @examples
#' if(require("VGAM")){
#'   fit <- vglm(Species ~ Sepal.Length,
#'               family = multinomial(),
#'               data = iris)
#'   R2.vglm(fit)
#' }
#' @author Mathijs Deen
R2.vglm <- function(model, method = c("mcfadden", "nagelkerke", "efron", "coxsnell", "tjur")) {
  method <- match.arg(method)
  loglik_full <- logLik(model)
  n <- nobs(model)

  null_model <- tryCatch(update(model, . ~ 1), error = function(e) NULL)
  if (is.null(null_model)) {
    warning("No fit for null model. Check model specification.", call. = FALSE)
    return(NA)
  }
  loglik_null <- logLik(null_model)

  r2 <- switch(method,
               "mcfadden" = 1 - (loglik_full / loglik_null),
               "nagelkerke" = (1 - exp((2/n) * (loglik_null - loglik_full))) / (1 - exp((2/n) * loglik_null)),
               "coxsnell" = 1 - exp((loglik_null - loglik_full) / n),
               "efron" = {
                 # Controleer of het model multinomiaal is
                 if (inherits(model@family, "multinomial")) {
                   stop("Efron's R^2 is not suitable for multinomial models. Use McFadden, Nagelkerke, or Cox & Snell instead.")
                 }

                 pred_values <- predict(model, type = "response")
                 y_numeric <- as.numeric(model@y)

                 1 - (sum((y_numeric - pred_values)^2) / sum((y_numeric - mean(y_numeric))^2))
               },
               "tjur" = {
                 if (length(unique(model@y)) != 2) {
                   warning("Tjur's R^2 is only valid for binomial models.")
                   return(NA)
                 }
                 pred_values <- predict(model, type = "response")
                 mean(pred_values[model@y == max(model@y)]) - mean(pred_values[model@y == min(model@y)])
               }
  )
  names(r2) <- method
  return(r2)
}
