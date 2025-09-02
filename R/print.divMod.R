#' @export
print.divMod <- function(x, ...) {
  print.data.frame(x, row.names = FALSE, ...)
  invisible(x)
}
