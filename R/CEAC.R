#' Create data for cost-effectiveness acceptability curve
#'
#' @param x object of class \code{CEA}
#'
#' @return \code{CEAC} returns data that can be plotted using \code{plot.CEAC}.
#' @export
#'
#' @examples
#' CEA(gnomes, insulationMethod, Costs, diffHATS, 1000) |>
#'   CEAC() |>
#'   plot(xlim = c(0,200),
#'        xlab = "Cost-effectiveness threshold (acorns)")
#' @author Mathijs Deen

CEAC <- function(x){
  if(!inherits(x, "CEA"))
    stop(paste(deparse(substitute(x)), "is not of class 'CEA'"), call. = FALSE)
  s <- x$stats
  s$quadrant2[s$quadrant == "SE"] <- 0
  s$quadrant2[s$quadrant %in% c("NE", "SW")] <- 1
  s$quadrant2[s$quadrant == "NW"] <- 2
  s <- s[order(s$quadrant2,s$ICERs), ]
  s$qntl <- 1:nrow(s)/nrow(s)
  outlist <- list(s = s, ICER.true = x$ICER.true, diffC.true = x$diffC.true)
  class(outlist) <- "CEAC"
  return(outlist)
}

#' Plot cost-effectiveness acceptability curve
#'
#' @param x object of class \code{CEAC}
#' @param xlab label for x axis
#' @param ylab label for y axis
#' @param las style of the axis labels (see \code{\link[graphics]{par}})
#' @param xlim limits of the x axis
#' @param ... other arguments to be passed to the \code{plot} function.
#'
#' @return returns a plot
#' @importFrom graphics arrows
#' @export
#'
#' @examples
#' CEA(gnomes, insulationMethod, Costs, diffHATS, 1000) |>
#'   CEAC() |>
#'   plot(xlim = c(0,200),
#'        xlab = "Cost-effectiveness threshold (acorns)")
#' @author Mathijs Deen
plot.CEAC <- function(x,
                      xlab = "Cost-effectiveness threshold (\u20AC)",
                      ylab = "Probability that intervention is cost-effective",
                      las = 1,
                      xlim =c(0, max(x$s$ICERs)),
                      ...) {
  s <- x$s
  with(s[s$quadrant2 < 2,],
       plot(x = ICERs,
            y = qntl,
            type = 'l',
            xlim = xlim,
            ylim = c(0,1),
            xaxs = "i",
            yaxs="i",
            xlab = xlab,
            ylab = ylab,
            las = las))
  if(x$ICER.true > 0 & x$diffC.true > 0) {
    qntl.true <- s[s$ICERs == x$ICER.true & s$diffC == x$diffC.true, ]$qntl
    arrows(x0 = x$ICER.true,
           y0 = 0,
           x1 = x$ICER.true,
           y1 = qntl.true,
           length = 0,
           lty = "dashed")
    arrows(x0 = 0,
           y0 = qntl.true,
           x1 = x$ICER.true,
           y1 = qntl.true,
           length = 0,
           lty = "dashed")
    text(x = x$ICER.true,
         y = qntl.true / 2,
         pos = 4,
         offset = .5,
         labels = paste0("ICER = ", round(x$ICER.true,2), ", p = ", round(qntl.true, 2)))
  }
}
