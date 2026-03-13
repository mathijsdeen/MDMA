#' Convert a matrix to a named vector
#'
#' Converts a matrix to a named vector by stacking columns, with element
#' names formed by combining column and row names separated by \code{sep}.
#' Useful for extracting and labelling matrix entries, for example when
#' collecting bootstrap estimates across response-predictor pairs into a
#' flat vector.
#'
#' @param x An object to convert to a named vector.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A named vector in column-major order.
#'
#' @export
namedVector <- function(x, ...) UseMethod("namedVector")

#' @rdname namedVector
#'
#' @param x A matrix with non-\code{NULL} row names and column names.
#' @param sep A single character string used to separate the column name
#'   from the row name in each element's name. Defaults to \code{"|"}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A named vector of length \code{nrow(x) * ncol(x)}, in
#'   column-major order. Names follow the pattern
#'   \code{"<colname><sep><rowname>"}.
#'
#' @details
#' Stacking is column-major, consistent with R's \code{\link[base]{as.vector}}
#' and \code{\link[base]{matrix}} conventions. The name of the element in
#' column \code{j} and row \code{i} is \code{paste(colname_j, rowname_i, sep = sep)}.
#'
#' If \code{x} has \code{NULL} row names or column names the corresponding
#' part of each element name will be \code{""}, producing names like
#' \code{"|rowname"} or \code{"colname|"}.
#'
#' @examples
#' M <- matrix(1:6, nrow = 2,
#'             dimnames = list(c("a", "b"), c("x", "y", "z")))
#' namedVector(M)
#' # x|a  x|b  y|a  y|b  z|a  z|b
#' #   1    2    3    4    5    6
#'
#' namedVector(M, sep = ".")
#' # x.a  x.b  y.a  y.b  z.a  z.b
#' #   1    2    3    4    5    6
#'
#' @seealso \code{\link[base]{as.vector}}, \code{\link{setNames}}
#' @export
namedVector.matrix <- function(x, sep = "|", ...){
  v        <- as.vector(x)
  rn       <- rownames(x)
  cn       <- colnames(x)
  names(v) <- paste(rep(cn,  each = length(rn)),
                    rep(rn, times = length(cn)),
                    sep = sep)
  return(v)
}
