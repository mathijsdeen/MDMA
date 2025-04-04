#' @title Cost-effectiveness analysis
#' @description Perform a cost-effectiveness analysis. Or a cost-utility analysis.
#'
#' `r lifecycle::badge("stable")`
#' @param data a \code{data.frame} with rows representing observations in for
#'     example a cost-effectiveness trial.
#' @param group group variable in \code{data}. Should contain two levels.
#' @param cost cost variable in \code{data}.
#' @param effect effect variable in \code{data}.
#' @param B number of bootstrap samples.
#' @param currency currency unit. See ?currency2unicode for options that will return
#'     the a Unicode symbol that will be used in plot.CEA and plot.CEAC. If the parameter
#'     is not listed, the parameter itself will be used. This makes it possible to input
#'     a custom Unicode hex (e.g., \code{"\u00Ae"}). Defaults to "euro".
#' @return \code{CEA} returns a list (class \code{CEA}) with the following elements:
#' \item{stats}{a \code{data.frame} containing the bootstrap statistics: estimates for
#'     the difference in costs (\code{diffC}), the difference in effects(\code{diffE}),
#'     and the ICER as a ratio of these two.}
#' \item{diff.C.true}{Observed difference in costs.}
#' \item{diff.E.true}{Observed difference in effects.}
#' \item{ICER.true}{Observed incremental cost-effectiveness ratio.}
#' \item{gr1}{First level of group variable.}
#' \item{gr2}{Second level of group variable.}
#' \item{currencyUC}{The currency. Either in raw form (parameter \code{currency}) or as
#'     a Unicode hex.}
#' @export
#'
#' @examples
#' CEA(gnomes, insulationMethod, Costs, diffHATS, 5000, "acorns") |>
#'   plot()
#' @author Mathijs Deen
CEA <- function(data, group, cost, effect, B = 5000, currency = "euro"){
  arguments  <- as.list(match.call())
  data       <- na.omit(data)
  groups     <- eval(arguments$group, data) |> factor() |> droplevels()
  costs      <- eval(arguments$cost, data)
  effects    <- eval(arguments$effect, data)
  grLevs     <- levels(groups)
  grVar      <- deparse(substitute(group))
  currencyUC <- currency2unicode(currency)

  if(length(grLevs) != 2) stop(paste(grVar, "should have 2 levels"), call. = FALSE)

  ICERs <- diffC <- diffE <- vector(mode = "numeric", length = B)

  gr1 <- grLevs[1]
  gr2 <- grLevs[2]
  c1 <- mean(costs[groups == gr1])
  c2 <- mean(costs[groups == gr2])
  e1 <- mean(effects[groups == gr1])
  e2 <- mean(effects[groups == gr2])

  diffC.true <- c1 - c2
  diffE.true <- e1 - e2
  ICER.true <- diffC.true / diffE.true

  Bmat <- matrix(rep(1:nrow(data), B), ncol = B) |> apply(2, sample, replace = TRUE)

  d <- data.frame(groups, costs, effects)

  for(i in seq_along(1:B)){
    bSample <- d[Bmat[, i], ]
    c1 <- mean(bSample$costs[bSample$groups == gr1])
    c2 <- mean(bSample$costs[bSample$groups == gr2])
    e1 <- mean(bSample$effects[bSample$groups == gr1])
    e2 <- mean(bSample$effects[bSample$groups == gr2])
    diffC[i] <- c1 - c2
    diffE[i] <- e1 - e2
  }

  out.df <- data.frame(diffC, diffE)
  out.df <- rbind(out.df,
                  data.frame(diffC = diffC.true,
                             diffE = diffE.true))
  out.df$ICERs <- out.df$diffC / out.df$diffE

  quadrant <- vector("character", length = B)
  quadrant[out.df$diffE > 0 & out.df$diffC < 0] <- "SE"
  quadrant[out.df$diffE < 0 & out.df$diffC > 0] <- "NW"
  quadrant[out.df$diffE < 0 & out.df$diffC < 0] <- "SW"
  quadrant[out.df$diffE > 0 & out.df$diffC > 0] <- "NE"

  out.df$quadrant <- quadrant

  # TO DO: BCa and percentile intervals for ICER.

  outlist <- list(stats      = out.df,
                  diffC.true = diffC.true,
                  diffE.true = diffE.true,
                  ICER.true  = ICER.true,
                  gr1        = gr1,
                  gr2        = gr2,
                  currencyUC = currencyUC)
  class(outlist) <- "CEA"
  return(outlist)
}

#' @title Plot cost-effectiveness plane
#'
#' @param x object of class \code{CEA}, created by \code{\link{CEA}}
#' @param xlim limits of x axis (i.e., the axis of the incremental effects)
#' @param ylim limits of y axis (i.e., the axis of the incremental costs)
#' @param xlab label of x axis
#' @param ylab label of y axis
#' @param las style of the axis labels (see \code{\link[graphics]{par}})
#' @param ... other arguments to be passed to the \code{plot} and \code{abline}
#'     (for the zero lines of the axes) functions.
#'
#' @return \code{plot.CEA} returns a plot
#' @export
#'
#' @examples
#' CEA(gnomes, insulationMethod, Costs, diffHATS, 5000, "acorns") |>
#'   plot()
#' @author Mathijs Deen
plot.CEA <- function(x,
                     xlim = c(-1,1) * max(abs(x$stats$diffE)),
                     ylim = c(-1,1) * max(abs(x$stats$diffC)),
                     xlab = "Incremental effects",
                     ylab = sprintf("Incremental costs (%s)", x$currencyUC),
                     las  = 1,
                     ...){
  oldpar <- par(bty="l")
  on.exit(oldpar, add=TRUE)
  s <- x$stats

  plot(x    = s$diffE,
       y    = s$diffC,
       xlim = xlim,
       ylim = ylim,
       xlab = xlab,
       ylab = ylab,
       las  = las,
       ...)
  abline(v = 0, h = 0, ...)
}

