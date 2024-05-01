#' @title Cost-effectiveness analysis
#'
#' @param data a \code{data.frame} with rows representing observations in for
#'     example a cost-effectiveness trial.
#' @param group group variable in \code{data}. Should contain two levels.
#' @param cost cost variable in \code{data}.
#' @param effect effect variable in \code{data}.
#' @param B number of bootstrap samples.
#'
#' @return \code{CEA} returns a list (class \code{CEA}) with the following elements:
#' \item{stats}{a \code{data.frame} containing the bootstrap statistics: estimates for
#'     the difference in costs (\code{diffC}), the difference in effects(\code{diffE}),
#'     and the ICER as a ratio of these two.}
#' \item{diff.C.true}{Observed difference in costs.}
#' \item{diff.E.true}{Observed difference in effects.}
#' \item{ICER.true}{Observed incremental cost-effectiveness ratio.}
#' @export
#'
#' @examples\dontrun{
#' 1
#' }
#' @author Mathijs Deen
CEA <- function(data, group, cost, effect, B = 5000){
  arguments <- as.list(match.call())
  data      <- na.omit(data)
  groups    <- eval(arguments$group, data) |> factor() |> droplevels()
  costs     <- eval(arguments$cost, data)
  effects   <- eval(arguments$effect, data)
  grLevs    <- levels(groups)
  grVar     <- deparse(substitute(group))

  if(length(grLevs) != 2) stop(paste(grVar, "should have 2 levels"), call. = FALSE)

  ICERs <- diffC <- diffE <- vector(mode = "numeric", length = B)

  c1 <- mean(costs[groups == grLevs[1]])
  c2 <- mean(costs[groups == grLevs[2]])
  e1 <- mean(effects[groups == grLevs[1]])
  e2 <- mean(effects[groups == grLevs[2]])
  diffC.true <- c1 - c2
  diffE.true <- e1 - e2
  ICER.true <- diffC.true / diffE.true

  Bmat <- matrix(rep(1:nrow(data), B), ncol = B) |> apply(2, sample, replace = TRUE)

  d <- data.frame(groups, costs, effects)

  for(i in seq_along(1:B)){
    bSample <- d[Bmat[, i], ]
    c1 <- mean(bSample$costs[bSample$groups == grLevs[1]])
    c2 <- mean(bSample$costs[bSample$groups == grLevs[2]])
    e1 <- mean(bSample$effects[bSample$groups == grLevs[1]])
    e2 <- mean(bSample$effects[bSample$groups == grLevs[2]])
    diffC[i] <- c1 - c2
    diffE[i] <- e1 - e2
  }

  out.df <- data.frame(diffC, diffE)
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
                  ICER.true  = ICER.true)
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
#' @examples\dontrun{
#' 1
#' }
#' @author Mathijs Deen
plot.CEA <- function(x,
                     xlim = c(-1,1) * max(abs(x$stats$diffE)),
                     ylim = c(-1,1) * max(abs(x$stats$diffC)),
                     xlab = "Incremental effects",
                     ylab = "Incremental costs",
                     las  = 1,
                     ...){
  oldpar <- par(bty="l")
  on.exit(oldpar, add=TRUE)
  plot(x    = x$stats$diffE,
       y    = x$stats$diffC,
       xlim = xlim,
       ylim = ylim,
       xlab = xlab,
       ylab = ylab,
       las  = las,
       ...)
  abline(v = 0, h = 0, ...)
}

