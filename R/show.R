#' @title Simultaneously print and return an object
#' @description \code{show} prints and returns an object. This is useful
#'     if one wants to show intermediate results in a pipe sequence.
#' @param object the object that should be printed and returned.
#'
#' @return \code{show} returns the value of \code{object}, while also printing it.
#' @export
#'
#' @examples
#' mtcars |>
#' lm(mpg ~  disp + hp, data = _) |>
#' keep(lm.mpg_disp_hp) |>
#' summary() |>
#' show() |>
#' .Primitive("$")("df")
#' @author Mathijs Deen
show <- function(object){
  print(object)
  return(object)
}
