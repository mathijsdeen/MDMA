#' @title Generate plots for classification
#' @description Generate plots for classification of two groups given one continuous variable
#' @param response continuous variable that is used as a classifier
#' @param group grouping variable
#' @param levels levels of the grouping variable that are to be classified (should be of length 2)
#' @param cutoffs.1 cutoff values to be shown in the ROC curve
#' @param cutoffs.2 cutoff values to be shown in the sensitivity/specificity plot
#' @param cutoffs.3 cutoff values to be shown in the conditional distribution plot
#' @param xlab.3 label for x axis in conditional distribution plot
#' @param labels.3 legend labels for both groups in \code{levels} in conditional distribution plot
#' @param xlim.3 \code{xlim} for conditional distribution plot
#' @param ylim.3 \code{ylim} for conditional distribution plot
#' @param pos.legend.2 location for legend in sensitivity/specificity plot
#' @param pos.legend.3 location for legend in conditional distribution plot
#' @return \code{classificationplots} returns three plots, and invisibly a dataframe containing sensitivities and specificities at all available threshold values.
#' @examples
#' library(pROC)
#' classificationPlots(aSAH$s100b,aSAH$outcome,levels=c("Good","Poor"))
#' @author Mathijs Deen
#' @import pROC
#' @importFrom graphics abline legend lines par points text
#' @importFrom stats density
#' @export
classificationPlots <- function(response, group, levels, cutoffs.1=NULL,
                                cutoffs.2=NULL, cutoffs.3=NULL,
                                xlab.3="Predicted probability",
                                labels.3=NULL, xlim.3=NULL,
                                ylim.3=c(0,10), pos.legend.2="right",
                                pos.legend.3="topright"){
  oldpty <- par()$pty
  oldmfrow <- par()$mfrow
  par(pty="s")
  par(mfrow=c(1,3))
  rr <- roc(group,response,levels=levels)
  rr2 <- data.frame("sensitivities"=rr$sensitivities,
                    "specificities"=rr$specificities,
                    "specificities_inv"=1-rr$specificities,
                    "thresholds"=rr$thresholds)

  #plot 1
  plot(rr2$specificities_inv,rr2$sensitivities, ylim=c(0,1),xlim=c(0,1),
       xlab="False positive rate (1 - specificity)",
       ylab="True positive rate (sensitivity)", type="l")
  abline(a=0,b=1,col="grey")
  if(!is.null(cutoffs.1)){
    trueCO.1 <- cutoffs.1 * NA
    for(i in 1:length(cutoffs.1)){
      trueCO.1[i] <- rr2$thresholds[which.min(abs(cutoffs.1[i]-rr2$thresholds))[1]]
    }
    points(rr2$specificities_inv[which(rr2$thresholds %in% trueCO.1)],
           rr2$sensitivities[which(rr2$thresholds %in% trueCO.1)])
    text(x=rr2$specificities_inv[which(rr2$thresholds %in% trueCO.1)],
         y=rr2$sensitivities[which(rr2$thresholds %in% trueCO.1)],
         cutoffs.1, adj=c(-0.5,1))
  }
  text(x=.6,y=.3, sprintf("AUC = %.3f",as.numeric(auc(rr))))

  #plot 2
  plot(rr2$thresholds,rr2$sensitivities, type="l", ylim=c(0,1),
       xlim=c(min(rr2$thresholds[is.finite(rr2$thresholds)]),
              max(rr2$thresholds[is.finite(rr2$thresholds)])),
       xlab="Thresholds", ylab="Sensitivity / Specificity",lty=1)
  lines(rr2$thresholds,rr2$specificities, lty=2)
  legend(pos.legend.2, legend=c("Sensitivity","Specificity"),lty=c(1,2))
  abline(v=cutoffs.2, lty=3)
  abline(h=-.01,col="grey")
  abline(h=1.01,col="grey")

  #plot 3
  ifelse(is.null(labels.3),
         legendnames <- levels(group),
         legendnames <- labels.3)
  ifelse(is.null(xlim.3),
         xlm3 <- c(min(response),max(response)),
         xlm3 <- xlim.3)
  plot(density(response[group==levels(group)[1]]), xlim=xlm3,
       main="",xlab=xlab.3,lty=1, ylim=ylim.3)
  lines(density(response[group==levels(group)[2]]), lty=2)
  legend(pos.legend.3,legend=legendnames, lty=c(1,2))
  abline(v=cutoffs.3, lty=3)
  abline(h=0,col="grey")
  par(pty=oldpty)
  par(mfrow=oldmfrow)
  invisible(rr2[,-3])
}
