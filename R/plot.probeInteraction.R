#' @title plot probed interaction
#' @description Plot the effects of the antecedent as a function of the moderator.
#'
#' `r lifecycle::badge("experimental")`
#' @param x object of class \code{probeInteraction}.
#' @param ... other arguments (none are used).
#' @param col.JN color for Johnson-Neyman cut-off line(s).
#' @param lty.JN linetype for Johnson-Neyman cut-off line(s).
#' @param col.CI color of the shade for the confidence interval.
#' @param lty.CI linetype for confidence interval boundaries.
#' @param lty.0 linetype for the horizontal line where the effect of the focal predictor on
#'     the outcome equals 0.
#'
#' @return \code{plot.probeInteraction} returns a combined plot with p value on the
#'     first y axis and effect of the antecedent variable
#' @export
#'
#' @examples
#' lm.1 <- lm(mpg ~ hp * wt, data = mtcars)
#' pI.1 <- probeInteraction(lm.1, hp, wt, JN=TRUE, n.interval.moderator = 3,
#'                          quantile.moderator = c(0.1,0.9), values.moderator = 2)
#' plot(pI.1)
#' lm.2 <- lm(mpg ~ qsec * drat, data = mtcars)
#' pI.2 <- probeInteraction(lm.2, qsec, drat, JN=TRUE, n.interval.moderator = 30,
#'                          quantile.moderator = c(0.1,0.9), values.moderator = 2)
#' plot(pI.2)
#' @importFrom grDevices rgb
#' @author Mathijs Deen
plot.probeInteraction <- function(x,
                                  ...,
                                  col.JN = "red",
                                  lty.JN = "dotted",
                                  col.CI = rgb(red = .5, green = .5, blue = .5, alpha = .2),
                                  lty.CI = "longdash",
                                  lty.0  = "dotted"){
  modName <- x$arguments$moderator
  antName <- x$arguments$antecedent
  modVals <- eval(modName, x$effects)
  pVals   <- x$effects$p
  effVals <- x$effects$Effect
  conName <- as.character(x$formula[[2]][2])
  LLCI    <- x$effects$LLCI
  ULCI    <- x$effects$ULCI
  yRange  <- c(min(LLCI), max(ULCI))
  ifelse("alpha" %in% names(x$arguments), alpha <- x$arguments$alpha, alpha <- .05)
  ifelse("JN" %in% names(x$arguments), JN <- x$arguments$JN, JN <- TRUE)

  plot(x    = modVals,
       y    = effVals,
       ylim = yRange,
       ylab = paste0("effect of ", antName, " on ", conName),
       xlab = modName,
       ...)
  lines(x = modVals, y = effVals, ...)
  lines(x = modVals, y = LLCI, lty=lty.CI)
  lines(x = modVals, y = ULCI, lty=lty.CI)
  abline(h=0, lty=lty.0)
  polygon(c(modVals, rev(modVals)),
          c(LLCI, rev(ULCI)),
          col=col.CI,
          border = NA)
  if(JN) abline(v   = modVals[which(abs(pVals-alpha) < 10 * .Machine$double.eps)],
                col = col.JN,
                lty = lty.JN)
}
