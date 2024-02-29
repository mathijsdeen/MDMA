#' @title \%nin\% function
#' @description Evaluates whether the left hand side argument is not in the right hand side argument.
#' @param lhs left hand side
#' @param rhs right hand side
#' @return The function returns a vector of the same length as \code{lhs}.
#' @details The \code{\%nin\%} function negates the \code{\%in\%} operator.
#' @examples
#' c(1,2,3) %nin% c(1,2)
#' @author Mathijs Deen
#' @export
`%nin%` <- function(lhs, rhs){
  `%notin%` <- Negate(`%in%`)
  return(lhs %notin% rhs)
}
