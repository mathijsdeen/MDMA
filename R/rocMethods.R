#' @title plot roc curve
#' @param x object of class \code{roc}.
#' @param y argument for generic \code{plot} function, not used here.
#' @param ... other arguments for generic \code{plot} function, none are used here.
#' @param cutoffs.1 cutoff value(s) to be shown in the first plot
#' @param cutoffs.2 cutoff value(s) to be shown in the second plot
#' @param cutoffs.3 cutoff value(s) to be shown in the third plot
#' @param xlab.3 lable for x axis in third plot
#' @param labels.3 legend labels for third plot
#' @param xlim.3 xlim for third plot
#' @param ylim.3 ylim for third plot
#' @param pos.legend.2 legend position for second plot
#' @param pos.legend.3 legend position for third plot
#'
#' @return \code{plot.roc} provides three plots:
#' \itemize{
#'  \item The first plot contains the ROC curve.
#'  \item The second plot contains curves for the sensitivity and the specificity
#'      for all threshold values.
#'  \item The third plot contains density plots for the two classification groups.
#' }
#'
#' @examples
#' a <- roc(QIDS$QIDS, QIDS$depression, c("Yes","No"), "Yes")
#' plot(a, ylim.3 = c(0,.2), xlab.3= "QIDS value", cutoffs.1 = 14.5,
#'      cutoffs.2 = 14.5, cutoffs.3 = 14.5)
#' @author Mathijs Deen
#' @export
plot.roc <- function(x, y, ..., cutoffs.1=NULL,
                     cutoffs.2=NULL, cutoffs.3=NULL,
                     xlab.3=NULL,
                     labels.3=NULL, xlim.3=NULL,
                     ylim.3=c(0,10), pos.legend.2="right",
                     pos.legend.3="topright") {
  oldpty <- par()$pty
  oldmfrow <- par()$mfrow
  par(pty="s")
  par(mfrow=c(1,3))
  #hier nog een on.exit
  #gebruiker ook nog kunnen laten kiezen welke plot(s) er moet(en) verschijnen,
  #en hoe (verticaal, horizontaal, diagonaal)

  rr2 <- x$rdf
  rr2$falsePosRates <- 1 - x$rdf$specificities

  #plot 1
  plot(x    = rr2$falsePosRates,
       y    = rr2$sensitivities,
       ylim = c(0,1),
       xlim = c(0,1),
       xlab = "False positive rate (1 - specificity)",
       ylab = "True positive rate (sensitivity)",
       type = "l")
  abline(a=0,b=1,col="grey")

  if(!is.null(cutoffs.1)){
    trueCO.1 <- cutoffs.1 * NA
    for(i in 1:length(cutoffs.1)){
      trueCO.1[i] <- rr2$thresholds[which.min(abs(cutoffs.1[i]-rr2$thresholds))[1]]
    }
    points(x    = rr2$falsePosRates[which(rr2$thresholds %in% trueCO.1)],
           y    = rr2$sensitivities[which(rr2$thresholds %in% trueCO.1)])
    text(x      = rr2$falsePosRates[which(rr2$thresholds %in% trueCO.1)],
         y      = rr2$sensitivities[which(rr2$thresholds %in% trueCO.1)],
         labels = cutoffs.1,
         adj    = c(-0.5,1))
  }
  text(x      = .6,
       y      = .3,
       labels = sprintf("AUC = %.3f", x$auc))

  #plot 2
  plot(x    = rr2$thresholds,
       y    = rr2$sensitivities,
       type = "l",
       ylim = c(0,1),
       xlim = c(min(rr2$thresholds[is.finite(rr2$thresholds)]),
                max(rr2$thresholds[is.finite(rr2$thresholds)])),
       xlab = "Thresholds",
       ylab = "Sensitivity, Specificity",
       lty  = 1)
  lines(x   = rr2$thresholds,
        y   = rr2$specificities,
        lty = 2)
  legend(x      = pos.legend.2,
         legend = c("Sensitivity","Specificity"),
         lty    = c(1,2))
  abline(v   = cutoffs.2,
         lty = 3)
  abline(h   = -.01,
         col = "grey")
  abline(h   = 1.01,
         col = "grey")

  #plot 3
  ifelse(is.null(labels.3),
         legendnames <- levels(x$group),
         legendnames <- labels.3)
  ifelse(is.null(xlim.3),
         xlm3 <- c(min(x$response),max(x$response)),
         xlm3 <- xlim.3)
  plot(x    = density(x$response[x$group==levels(x$group)[1]]),
       xlim = xlm3,
       main = "",
       xlab = xlab.3,
       lty  = 1,
       ylim = ylim.3)
  lines(x   = density(x$response[x$group==levels(x$group)[2]]),
        lty = 2)
  legend(x      = pos.legend.3,
         legend = legendnames,
         lty    = c(1,2))
  abline(v   = cutoffs.3,
         lty = 3)
  abline(h   = 0,
         col ="grey")
  par(pty = oldpty)
  par(mfrow = oldmfrow)
}

#' @title Area under the curve
#' @param x object of class \code{roc}.
#'
#' @return returns the area under the curve for a \code{roc} class object.
#' @examples
#' a <- roc(QIDS$QIDS, QIDS$depression, c("Yes","No"), "Yes")
#' auc(a)
#' @author Mathijs Deen
#' @export
auc <- function(x){
  UseMethod("auc")
}

#' @export
#' @rdname auc
auc.roc <- function(x){
  return(x$auc)
}
