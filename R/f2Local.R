f2Local <- function(object, ...){
  UseMethod("f2Local")
}

f2Local.lm <- function(object, method = r.squared){
  method <- deparse(substitute(method))
  valid_methods <- c("r.squared", "adj.r.squared")
  if(!method %in% valid_methods) stop("invalid method", call. = FALSE)

  R2full <- summary(object)[[method]]
  preds <- attr(terms(object), which = "term.labels")
  R2preds <- rep(NA_real_, length(preds))

  for(pred in seq_along(preds)){
    R2preds[pred] <- m.full |>
      update(as.formula(paste(". ~ . -", preds[pred]))) |>
      summary() |>
      (\(s) s$r.squared)()
  }

  f2s <- (R2full - R2preds) / (1 - R2full)
  out <- tibble(variable = preds,
                    f2Local  = f2s)
  class(out) <- "f2Local"
  return(out)
}

print.f2Local <- function(x, ...){
  f2s <- x$f2Local
  out <- tibble(variable = x$variable, f2s)
  print(out)
}

library(tibble)
f2Local.lm(m.full)

View(f2Local.lm(m.full))
