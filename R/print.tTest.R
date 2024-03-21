#' @title Print t test
#' @description Print the output of a t test
#'
#' `r lifecycle::badge("stable")`
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
