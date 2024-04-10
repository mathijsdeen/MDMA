#' @title Return which values are in a certain range
#' @description
#' \code{%in.range%} indicates which values are in a certain range, including the
#'     boundaries of the range.
#'
#'
#' `r lifecycle::badge("experimental")`
#' @param lhs numeric vector
#' @param rhs numeric vector of length 2 with the bounds of the range
#'
#' @return \code{%in.range%} returns a logical vector of \code{length(lhs)}, indicating which
#'     values of \code{lhs} are and are not in range \code{rhs}. Boundaries of \code{rhs} are included.
#' @export
#'
#' @examples
#' a <- seq(0, 1, 0.05)
#' r <- c(0.4,0.7)
#' cbind(a,
#'       'a %in.range% r' = a %in.range% r,
#'       'a %within.range% r' = a %within.range% r)
#' @author Mathijs Deen
#' @seealso [`%within.range%`]
`%in.range%` <- function(lhs, rhs){
  lhs >= min(rhs) & lhs <= max(rhs)+.Machine$double.eps
}

#' @title Return which values are within a certain range
#' @description
#'
#' \code{%within.range%} indicates which values are in a certain range, excluding
#'     the boundaries of the range.
#'
#' `r lifecycle::badge("experimental")`
#' @param lhs numeric vector
#' @param rhs numeric vector of length 2 with the bounds of the range
#'
#' @return \code{%within.range%} returns a logical vector of \code{length(lhs)}, indicating which
#'     values of \code{lhs} are and are not in range \code{rhs}. Boundaries of \code{rhs} are excluded.
#' @export
#'
#' @examples
#' a <- seq(0, 1, 0.05)
#' r <- c(0.4,0.7)
#' cbind(a,
#'       'a %in.range% r' = a %in.range% r,
#'       'a %within.range% r' = a %within.range% r)
#' @author Mathijs Deen
#' @seealso [`%in.range%`]
`%within.range%` <- function(lhs, rhs){
  lhs > min(rhs) & lhs < max(rhs)
}
