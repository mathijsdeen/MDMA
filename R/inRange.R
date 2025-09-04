#' @title inRange
#' @description Return which values are in a certain range
#' \code{%inRange%} indicates which values are in a certain range, including the
#'     boundaries of the range (i.e., a closed interval).
#'
#' `r lifecycle::badge("stable")`
#' @param lhs numeric vector.
#' @param rhs numeric vector of length 2 with the bounds of the range.
#'
#' @return \code{%inRange%} returns a logical vector of \code{length(lhs)}, indicating which
#'     values of \code{lhs} are and are not in range \code{rhs}. Boundaries of \code{rhs} are included.
#' @export
#'
#' @examples
#' a <- seq(0, 100, 5)
#' r <- c(40, 70)
#' cbind(a,
#'       'a %inRange% r' = a %inRange% r,
#'       'a %withinRange% r' = a %withinRange% r)
#' @author Mathijs Deen
#' @seealso \code{\link{\%withinRange\%}}
`%inRange%` <- function(lhs, rhs){
  lhs >= min(rhs) & lhs <= max(rhs)
}

#' @title withinRange
#' @description Return which values are within a certain range
#'
#' \code{%withinRange%} indicates which values are in a certain range, excluding
#'     the boundaries of the range (i.e., an open interval).
#'
#' `r lifecycle::badge("stable")`
#' @param lhs numeric vector.
#' @param rhs numeric vector of length 2 with the bounds of the range.
#'
#' @return \code{%withinRange%} returns a logical vector of \code{length(lhs)}, indicating which
#'     values of \code{lhs} are and are not in range \code{rhs}. Boundaries of \code{rhs} are excluded.
#' @export
#'
#' @examples
#' a <- seq(0, 100, 5)
#' r <- c(40, 70)
#' cbind(a,
#'       'a %inRange% r' = a %inRange% r,
#'       'a %withinRange% r' = a %withinRange% r)
#' @author Mathijs Deen
#' @seealso \code{\link{\%inRange\%}}
`%withinRange%` <- function(lhs, rhs){
  lhs > min(rhs) & lhs < max(rhs)
}
