f2Local <- function(object, ...){
  UseMethod("f2Local")
}

f2Local.lm <- function(object, method = "r.squared"){
  valid_methods <- c("r.squared", "adj.r.squared")
  if(!method %in% valid_methods) stop("invalid method.", call. = FALSE)

  R2full <- summary(object)[[method]]
  preds <- attr(terms(object), which = "term.labels")
  R2preds <- rep(NA_real_, length(preds))

  for(pred in seq_along(preds)){
    R2preds[pred] <- object |>
      update(as.formula(paste(". ~ . -", preds[pred]))) |>
      summary() |>
      (\(s) s[[method]])()
  }

  f2s <- (R2full - R2preds) / (1 - R2full)
  out <- list(variable = preds,
              f2Local  = f2s)
  class(out) <- "f2Local"
  return(out)
}

f2Local.glm <- function(object, method = "r2") {
  if (is.character(method)) {
    method_fun <- get(method, envir = asNamespace("performance"), inherits = FALSE)
  } else if (is.function(method)) {
    method_fun <- method
  } else {
    stop("argument 'method' be either a function or a string.", call. = FALSE)
  }

  R2full <- as.numeric(method_fun(object))
  #return(R2full)
  preds <- attr(terms(object), which = "term.labels")
  R2preds <- rep(NA_real_, length(preds))

  for(pred in seq_along(preds)){
    R2preds[pred] <- object |>
      update(as.formula(paste(". ~ . -", preds[pred]))) |>
      method_fun() |>
      as.numeric() |> round(5)
    print(R2preds[pred])
  }

  f2s <- (R2full - R2preds) / (1 - R2full)
  out <- list(variable = preds,
              f2Local  = f2s)
  class(out) <- "f2Local"
  return(out)
}

f2Local.vglm <- function(object, method = "mcfadden"){
  valid_methods <- c("mcfadden", "nagelkerke", "efron")
  if(!method %in% valid_methods) stop("invalid method.", call.=FALSE)

  if(method == "mcfadden"){
    method_fun <- function(){
      cat("mcfadden")
    }
  }else if(method == "nagelkerke"){
    method_fun <- function(){
      cat("nagelkerke")
    }
  }else if(method == "efron"){
    method_fun <- function(){
      cat("efron")
    }
  }
  method_fun()
}

print.f2Local <- function(x, ...){
  out <- data.frame(variable = x$variable,
                f2       = x$f2Local)
  print(out)
}

