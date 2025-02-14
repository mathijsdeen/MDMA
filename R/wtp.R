#' Probe willingness to pay
#' @description Get the probability of being cost-effective given a certain
#' cost-effectiveness threshold, and vice versa.
#'
#' @param x object of class \code{CEAC}
#' @param threshold cost-effectiveness threshold
#' @param probability probability of being cost-effective
#'
#' @return \code{wtp} either the probability or the threshold. If there is no exact match
#'    to the given parameter in the bootstrap samples, the result is interpolated.
#'
#' @details One of the two parameters \code{threshold} and \code{probability}
#'    should be specified.
#' @export
#' @importFrom stats approx
#' @examples
#' CEA(gnomes, insulationMethod, Costs, diffHATS, 1000, "acorns") |>
#'   CEAC() |>
#'   wtp(probability = 0.80)
#' CEA(gnomes, insulationMethod, Costs, diffHATS, 1000, "acorns") |>
#'   CEAC() |>
#'   wtp(threshold = 8)
#' @author Mathijs Deen
wtp <- function(x, threshold = NULL, probability = NULL){
  if(!inherits(x, "CEAC")) stop("x should be of class 'CEAC'")
  if(sum(sapply(list(threshold, probability), is.null)) != 1) stop("Exactly one of 't' and 'p' should be provided.")

  d <- x$s[,c("ICERs", "qntl")]

  if(is.null(threshold)){
    if(probability < 0 | probability > 1) stop("p should be within 0 and 1.")
    have <- "probability that the intervention is cost-effective"
    haveVal <- probability
    want <- "willingness to pay threshold"
    if(probability %in% d$qntl) {
      val <- d$ICERs[d$qntl == probability]
    } else{
      val <- approx(d$qntl, d$ICERs, xout = probability, rule = 2)$y
    }
  }

  if(is.null(probability)){
    have <- "willingness to pay threshold"
    haveVal <- threshold
    want <- "probability of cost-effectiveness"
    if(threshold %in% d$ICERs) {
      val <- d$qntl[df$ICERs == threshold]
    } else{
      val <- approx(d$ICERs, d$qntl, xout = threshold, rule = 2)$y
    }
  }

  out <- list(val = val, have = have, haveVal = haveVal, want = want)
  class(out) <- "wtp"
  return(out)
}

#' Print willingness to pay probe
#' @description Print the outcome of a willingness to pay threshold probe.
#'
#' @param x object of class \code{wtp}.
#' @param ... other arguments (none are used).
#'
#' @return \code{print.wtp} prints the outcome of \code{wtp}
#'
#' @export
#'
#' @examples
#' CEA(gnomes, insulationMethod, Costs, diffHATS, 1000, "acorns") |>
#'   CEAC() |>
#'   wtp(probability = 0.80)
#' CEA(gnomes, insulationMethod, Costs, diffHATS, 1000, "acorns") |>
#'   CEAC() |>
#'   wtp(threshold = 8)
#' @author Mathijs Deen
print.wtp <- function(x, ...){
  cat(sprintf("For the %s of %.3f, the %s is %.3f.", x$have, x$haveVal, x$want, x$val))
}
