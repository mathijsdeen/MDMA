#' @title Receiver operator characteristic
#' @description Calculate ROC
#'
#' `r lifecycle::badge("stable")`
#' @param response response variable
#' @param group group variable
#' @param levels level variable
#' @param state state level
#'
#' @return blaat
#' @export
#'
#' @examples \dontrun{
#' x <- 1}
#' @author Mathijs Deen
roc <- function(response, group, levels, state) {
  obs <- group[group %in% levels]
  data <- na.omit(data.frame(response, obs))
  x <- data$response
  xsort <- sort(unique(x))
  thresholds <- (c(xsort, Inf) + c(-Inf, xsort)) / 2
  obs <- data$obs
  obsBool <- obs == state
  xlist <- rep(list(x), length(thresholds))
  xWhich <- mapply(.Primitive(">="), xlist, thresholds)

  sensitivities <- colMeans(xWhich[obsBool == TRUE, ])
  specificities <- 1 - colMeans(xWhich[obsBool == FALSE, ])
  falsePosRates <- colMeans(xWhich[obsBool == FALSE, ])

  auc <- 1 - wilcox.test(x ~ obs)$statistic / prod(table(obs))

  rdf <- data.frame("thresholds"    = thresholds,
                    "sensitivities" = sensitivities,
                    "specificities" = specificities)
  outlist <- list(data = data, rdf = rdf, auc = as.numeric(auc),
                  response = response, group = group, levels = levels,
                  state = state)
  class(outlist) <- "roc"
  return(outlist)
}
