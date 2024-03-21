#' @title Print effects of probed interaction
#' @description Print the effects from a probed interaction
#'
#' `r lifecycle::badge("stable")`
#' @param x object of class \code{probeInteraction}
#' @param ... other parameters (none are used)
#'
#' @return \code{print.probeInteraction} prints the effects table of a
#'     \code{probeInteraction} object
#' @export
#'
#' @examples
#' lm.1 <- lm(mpg ~ hp * wt, data = mtcars)
#' pI <- probeInteraction(lm.1, hp, wt, JN=TRUE, n.interval.moderator = 3,
#'                        quantile.moderator = c(0.1,0.9), values.moderator = 2)
#' print(pI)
#' @author Mathijs Deen
print.probeInteraction <- function(x, ...){
  print(x$effects)
}
