#' @title Summarize outcome of a t-test
#'
#' @param object object of class \code{htest} (i.e., the result of \code{mdma::tTest} or \code{stats::t.test})
#' @param rnd number of decimal places. Should have length 1 or 3. One value specifies the rounding
#'     value for the degrees of freedom, t statistic and p value all at once, while specifying three values
#'     gives the rounding values for the three statistics respectively.
#' @param ... other arguments of the summary generic (none are used)
#' @return \code{summary.htest} returns a typical APA-like output (without italics) for a t-test.
#' @export
#' @examples
#' x1 <- QIDS$QIDS[QIDS$depression == "Yes"]
#' x2 <- QIDS$QIDS[QIDS$depression == "No"]
#' tt <- tTest(x1, x2)
#' summary(tt, rnd = c(1,2,3))
#' @author Mathijs Deen
summary.tTest <- function(object, ..., rnd = 3L){
  if(length(rnd) == 1){
    dfRnd <- tRnd <- pRnd <- rnd
  }else if(length(rnd) == 3){
    dfRnd <- rnd[1]
    tRnd  <- rnd[2]
    pRnd  <- rnd[3]
  }else{
    stop("argument rnd should have lenght of 1 or 3", call. = FALSE)
  }
  if(any(rnd > 9)) stop("Maximum number of decimals is 9", call. = FALSE)
  x <- object
  pOrig <- x$p.value
  p <- format(round(pOrig, pRnd), nsmall = pRnd)
  pUnit <- strsplit(p, "\\.")[[1]][1]
  if(pUnit == "1") {
    pFinal <- substr("p > .999999999", 1, pRnd + 5)
  }else{
    pDecs <- strsplit(p, "\\.")[[1]][2]
    if(as.numeric(pDecs) == 0){
      pFinal <- paste0("p < .", paste(rep("0", pRnd-1), collapse=""), "1")
    }else{
      pFinal <- paste0("p = .", pDecs)
    }
  }
  outf <- paste0("t(%.", dfRnd, "f) = %.", tRnd, "f, %s")
  cat(sprintf(outf, x$parameter, x$statistic, pFinal))
}

#' Print t-test
#'
#' @param x an object used to select a method
#' @param ... further arguments passed to or from other methods
#' @return prints the \code{tTest} object as a \code{htest} object.
#' @export
#' @examples
#' x1 <- QIDS$QIDS[QIDS$depression == "Yes"]
#' x2 <- QIDS$QIDS[QIDS$depression == "No"]
#' tt <- tTest(x1, x2)
#' print(tt)
#' @author Mathijs Deen
print.tTest <-function(x, ...){
  NextMethod()
}
