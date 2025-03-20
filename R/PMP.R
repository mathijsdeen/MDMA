#' @title Posterior model probability
#' @description Calculate the posterior model probability for a set of models.
#'
#' `r lifecycle::badge("stable")`
#'
#' @usage
#' PMP(...)
#'
#' ##  aliases for backward compatibility, deprecated:
#'
#' @param ... objects of class \code{(g)lm}, given as separate arguments.
#' @return \code{PMP} returns to posterior model probabilities for the models provided.
#' @details
#' Posterior model probabilities are calculated for every model \eqn{i} as \deqn{\mathrm{PMP}_i = \frac{\mathrm{exp}\Big[-\frac{1}{2}\Delta_i\mathrm{BIC}\Big]}{\sum_{j = 1}^K\mathrm{exp}\Big[-\frac{1}{2}\Delta_j\mathrm{BIC}\Big]},}
#' where the minimal BIC value is subtracted from all BICs. In other words: the model with the lowest BIC has \eqn{\Delta\mathrm{BIC}=0}.
#' @importFrom stats BIC
#' @examples
#' lm.1 <- lm(mpg ~ hp + wt, data = mtcars)
#' lm.2 <- lm(mpg ~ hp * wt, data = mtcars)
#' lm.3 <- lm(mpg ~ hp * wt + gear, data = mtcars)
#' PMP(lm.1, lm.2, lm.3)
#' @author Mathijs Deen
#' @export
PMP <- function(...){
  models <- list(...)
  formulae <- unlist(lapply(models, FUN = function(x) .Primitive("$")(x, call)[[2]]))
  if(any(lapply(models, function(x) sum(class(x) %in% c("lm", "glm")))==0)) {
    stop("not all objects are of class 'lm' or 'glm'")
  }
  BICs <- as.matrix(lapply(models, BIC)) |> unlist()
  lowestBIC <- min(BICs)
  relBICs <- BICs - lowestBIC
  pmp <- exp(-1/2 * relBICs)/sum(exp(-1/2 * relBICs))
  res <- cbind(BIC = BICs,
               PMP = pmp)
  rownames(res) <- formulae
  return(res)
}

#' @export
#' @describeIn PMP deprecated alias
pMO <- function(...){
  .warningDeprecated("PMP", "2.0.0")
  PMP(...)
}

#' @export
#' @describeIn PMP deprecated alias
pMM <- function(...){
  .warningDeprecated("PMP", "2.0.0")
  PMP(...)
}
