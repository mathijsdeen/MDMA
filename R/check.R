
#' @title Check model for influential cases
#'
#' @param object object of class \code{lm}
#' @param ... other parameters (none are used at the moment)
#'
#' @return \code{check} returns a list containing a matrix with statistics regarding
#'     influential cases and a vector of variance inflation factors. Furthermore, it
#'     produces diagnostics plots.
#' @export
#'
#' @examples
#' lm.1 <- lm(mpg ~ disp + wt, data = mtcars)
#' check(lm.1)
#' @importFrom car vif
#' @importFrom stats hatvalues influence.measures predict rstandard
#' @author Mathijs Deen
check <- function(object, ...){
  UseMethod("check")
}

#' @export
check.lm <- function(object, ...){
  pred <- predict(object)
  res <- object$residuals
  lev <- hatvalues(object)
  std.res <- rstandard(object)
  influence <- influence.measures(object)
  influential_case <- apply(influence$is.inf, 1L, any, na.rm = TRUE)
  influence <- data.frame(predicted.value = pred,
                          residual = res,
                          std.residual = std.res,
                          influence$infmat,
                          influential = ifelse(influential_case, 1,0))

  terms <- labels(terms(object))
  n.terms <- length(terms)
  ifelse(n.terms > 1, vifs <- vif(object), vifs <- NA)
  out <- list(influence = influence, vifs = vifs)
  class(out) <- "check.lm"
  oldpar <- par(mfrow = c(3,2))
  on.exit(par(oldpar), add=TRUE)
  plot(object, which = 1)
  plot(object, which = 2)
  plot(object, which = 3)
  plot(object, which = 4)
  plot(object, which = 5)
  plot(object, which = 6)
  return(out)
}

#' @method print check.lm
print.check.lm <- function(x){
  cat(sprintf("Case fit values and influence statistics: \n"))
  print(round(x$influence, 3))
  cat(sprintf("\n"))

  if(!anyNA(x$vifs)){
    cat(sprintf("Variance inflation factors: \n"))
    print(round(x$vifs, 3))
  }else{
    cat(sprintf("No VIFs calculated since model contains fewer than 2 terms"))
  }
}

