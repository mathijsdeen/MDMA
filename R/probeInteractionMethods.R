#' @title Print effects of probed interaction
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

#' @title plot probed interaction
#' @description Plot the effects of the antecedent and corresponding p value
#'     as functions of the moderator.
#'
#' `r lifecycle::badge("experimental")`
#' @param x object of class \code{probeInteraction}
#' @param ... other arguments (none are used)
#' @param lty.p linetype for p values
#' @param pch.p point character for p values
#' @param col.p color for p values
#' @param lty.effect linetype for effect
#' @param pch.effect point character for effect
#' @param col.effect color for effect
#' @param col.JN color for Johnson-Neyman cut-off line(s)
#' @param lty.JN linetype for Johnson-Neyman cut-off line(s)
#' @param pos.legend position of legend
#'
#' @return \code{plot.probeInteraction} returns a combined plot with p value on the
#'     first y axis and effect of the antecedent variable
#' @export
#'
#' @examples
#' lm.1 <- lm(mpg ~ hp * wt, data = mtcars)
#' pI <- probeInteraction(lm.1, hp, wt, JN=TRUE, n.interval.moderator = 3,
#'                        quantile.moderator = c(0.1,0.9), values.moderator = 2)
#' plot(pI)
#' @importFrom graphics axis mtext
#' @author Mathijs Deen
plot.probeInteraction <- function(x, ...,
                                  lty.p      = "solid",
                                  pch.p      = 1,
                                  col.p      = "black",
                                  lty.effect = "longdash",
                                  pch.effect = 17,
                                  col.effect = "green",
                                  col.JN     = "red",
                                  lty.JN     = "dotted",
                                  pos.legend = "topleft"){
  modName <- x$arguments$moderator
  antName <- x$arguments$antecedent
  modVals <- eval(modName, x$effects)
  pVals   <- x$effects$p
  effVals <- x$effects$Effect
  conName <- as.character(x$formula[[2]][2])
  if(is.character(pos.legend)){
    legend.x <- pos.legend
    legend.y <- NULL
  }else{
    if(length(pos.legend) == 2){
      legend.x <- pos.legend[1]
      legend.y <- pos.legend[2]
    }else{
      stop('argument pos.legend should be a numeric vector of length 2 OR a single keyword (see ?legend)', call. = FALSE)
    }
  }
  ifelse("alpha" %in% names(x$arguments), alpha <- x$arguments$alpha, alpha <- .05)
  ifelse("JN" %in% names(x$arguments), JN <- x$arguments$JN, JN <- TRUE)

  oldpar1  <- par(mar = c(5, 4, 4, 4) + 0.3)
  on.exit(par(oldpar1), add = TRUE)
  plot(x = modVals, y = pVals, ylab = "p value", pch  = pch.p, col = col.p, xlab = modName)
  lines(x = modVals, y = pVals, lty = lty.p, col = col.p)
  legend(x = legend.x, y = legend.y, legend = c("p value", "effect"), lty = c(lty.p, lty.effect),
         pch = c(pch.p, pch.effect), col = c(col.p, col.effect))

  oldpar2  <- par(new = TRUE)
  on.exit(par(oldpar2), add = TRUE)
  plot(x = modVals, y = effVals, axes = FALSE, bty = "n", xlab = "", ylab = "",
       pch = pch.effect, col = col.effect)
  lines(x = modVals, y = effVals, lty = lty.effect, col = col.effect)
  axis(side = 4, at = pretty(range(effVals)))
  mtext(paste0("effect of ", antName, " on ", conName) , side = 4, line = 3)

  if(JN) abline(v = modVals[which(abs(pVals-alpha) < 10 * .Machine$double.eps)],
                col = col.JN, lty = lty.JN)
}
