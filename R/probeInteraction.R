#' @title Probe interaction models
#' @description Probe the effect of a moderator on an X/antecedent variable in a linear model.
#'
#' `r lifecycle::badge("experimental")`
#' @param object object of class \code{lm}
#' @param antecedent antecedent (or x) variable in \code{object}
#' @param moderator moderator variable in \code{object}
#' @param alpha desired alpha level for Johnson-Neyman procedure
#' @param JN indicate whether Johnson-Neyman procedure should be carried out
#' @param n.interval.moderator number of intervals in the moderator variable to probe
#' @param quantile.moderator quantile values in the moderator variable to probe
#' @param values.moderator raw values in the moderator variable to probe
#' @return \code{probeInteraction} returns a data frame containing values of the moderator
#'     in a linear model, the effect of the antecedent at that value of the moderator,
#'     standard errors, t values, p values and a confidence interval.
#' @details the arguments \code{n.interval.moderator}, \code{quantile.moderator} and \code{values.moderator}
#'     can be combined. All unique values from these methods combined, together with the values from the
#'     Johnson-Neyman procedure (if specified) will be part of the probing procedure.
#' @importFrom stats qt quantile na.omit qnorm coef vcov
#' @importFrom methods hasArg
#' @examples
#' lm.1 <- lm(mpg ~ hp * wt, data = mtcars)
#' probeInteraction(lm.1, hp, wt, JN=TRUE, n.interval.moderator = 3,
#'                  quantile.moderator = c(0.1,0.9), values.moderator = 2)
#' @author Mathijs Deen
#' @export
probeInteraction <- function(object, antecedent, moderator, alpha=.05, JN=TRUE,
                             n.interval.moderator, quantile.moderator, values.moderator){
  arguments <- as.list(match.call())
  Xname     <- as.character(arguments$antecedent)
  Mname     <- as.character(arguments$moderator)
  if(!inherits(object, "lm")) stop("only lm class objects are currently supported", call. = FALSE)

  # Specified for lm objects; change this section for e.g. lme or lmerMod class objects
  V         <- vcov(object)                            # Variance-covariance matrix of estimates
  B         <- coef(object)                            # Estimates
  Mobserved <- eval(arguments$moderator, object$model) # Observed moderator values in model data

  # Get the right indices for antecedent, moderator and interaction
  Xpos    <- which(names(B) %in% Xname)
  Mpos    <- which(names(B) %in% Mname)
  XMpos   <- c(Xpos, Mpos)
  XMnames <- c(Xname, Mname)[which.min(XMpos):which.max(XMpos)]
  Iname   <- paste0(XMnames[1],":",XMnames[2])
  Ipos    <- which(names(B) %in% Iname)

  # Get parameter estimates, variances, covariance, df and critical t value
  bX     <- B[Xpos]
  bI     <- B[Ipos]
  V.bX   <- V[Xpos, Xpos]
  V.bI   <- V[Ipos, Ipos]
  C.bXbI <- V[Xpos, Ipos]
  res.df <- object$df.residual
  tc     <- qt(p = 1 - alpha / 2, df = res.df)

  Mintervalpoints <- Mquantilepoints <- JNpoints <- Mrawpoints <- NA

  # Johnson-Neyman procedure
  if(JN==TRUE){
    a <- tc^2 * V.bI - bI^2
    b <- 2 * tc^2 * C.bXbI - 2 * bX * bI
    c <- tc^2 * V.bX - bX^2
    D <- b^2 - 4 * a * c
    ifelse(D < 0, JNpoints <- NA, JNpoints <- (-b + c(-1, 1) * sqrt(D)) / (2 * a))
  }

  # Determine equal steps in range of moderator for interaction probing
  if(hasArg(n.interval.moderator)){
    Mintervalpoints <- diff(range(Mobserved)) * (0:n.interval.moderator)/n.interval.moderator +
      min(Mobserved)
  }

  # Determine quantiles in moderator for interaction probing
  if(hasArg(quantile.moderator)){
    Mquantilepoints <- quantile(Mobserved, quantile.moderator)
  }

  # If there are any, use the prespecified raw moderator values as well
  if(hasArg(values.moderator)){
    Mrawpoints <- values.moderator
  }

  # Combine and sort all unique moderator values from JN, steps, quantiles, and raw input
  Mvals <- sort(na.omit(unique(c(JNpoints, Mintervalpoints, Mquantilepoints, Mrawpoints))))

  # Calculate statistics
  # maybe use predict function for this? Currently won't work with more complex models
  effect <- bX + bI*Mvals
  SE     <- sqrt(V.bX + 2 * Mvals * C.bXbI + Mvals^2 * V.bI)
  tvals  <- effect / SE
  pvals  <- 2 * (1 - pt(q = abs(tvals), df = res.df))
  #LLCI   <- effect - qnorm(1 - alpha/2) * SE
  #ULCI   <- effect + qnorm(1 - alpha/2) * SE
  LLCI   <- effect - qt(1 - alpha/2, df = res.df) * SE
  ULCI   <- effect + qt(1 - alpha/2, df = res.df) * SE

  # Create output
  effects <- data.frame(Mvals, effect, SE, tvals, pvals, LLCI, ULCI)
  names(effects) <- c(names(B)[Mpos], "Effect", "se", "t", "p", "LLCI", "ULCI")
  out <- list(effects = effects, data = object$model,
              arguments = arguments, formula = object$call)
  class(out) <- "probeInteraction"
  return(out)
}
