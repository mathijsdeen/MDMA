R2.vglm <- function(model, method = c("mcfadden", "nagelkerke", "efron", "coxsnell", "tjur")) {
  method <- match.arg(method)
  loglik_full <- logLik(model)
  n <- nobs(model)

  null_model <- tryCatch(update(model, . ~ 1), error = function(e) NULL)
  if (is.null(null_model)) {
    warning("No fit for null model. Check model specification.")
    return(NA)
  }
  loglik_null <- logLik(null_model)

  r2 <- switch(
    method,
    "mcfadden"   = 1 - (loglik_full / loglik_null),
    "nagelkerke" = (1 - exp((2/n) * (loglik_null - loglik_full))) / (1 - exp((2/n) * loglik_null)),
    "coxsnell"   = 1 - exp((loglik_null - loglik_full) / n),
    "efron"      = {
      pred_values <- predict(model, type = "response")
      if (is.matrix(model@y)) {
        y_numeric <- max.col(model@y, ties.method = "first")
      } else {
        y_numeric <- as.numeric(model@y)
      }
      if (is.matrix(pred_values)) {
        observed_probs <- pred_values[cbind(1:n, y_numeric)]
        1 - (sum((1 - observed_probs)^2) / sum((1 - mean(observed_probs))^2))
      } else {
        1 - (sum((y_numeric - pred_values)^2) / sum((y_numeric - mean(y_numeric))^2))
      }
    },
    "tjur"       = {
      if (length(unique(model@y)) != 2) {
        warning("Tjur's R^2 is only valid for binomial models.")
        return(NA)
      }
      pred_values <- predict(model, type = "response")
      mean_pred_1 <- mean(pred_values[model@y == max(model@y)])
      mean_pred_0 <- mean(pred_values[model@y == min(model@y)])
      mean_pred_1 - mean_pred_0
    }
  )

  return(r2)
}

R2.vglm <- function(model, method = c("mcfadden", "nagelkerke", "efron", "coxsnell", "tjur")) {
  method <- match.arg(method)
  loglik_full <- logLik(model)
  n <- nobs(model)

  null_model <- tryCatch(update(model, . ~ 1), error = function(e) NULL)
  if (is.null(null_model)) {
    warning("No fit for null model. Check model specification.")
    return(NA)
  }
  loglik_null <- logLik(null_model)

  r2 <- switch(method,
               "mcfadden" = 1 - (loglik_full / loglik_null),
               "nagelkerke" = (1 - exp((2/n) * (loglik_null - loglik_full))) / (1 - exp((2/n) * loglik_null)),
               "coxsnell" = 1 - exp((loglik_null - loglik_full) / n),
               "efron" = {
                 pred_values <- predict(model, type = "response")

                 if (is.matrix(model@y)) {
                   y_numeric <- max.col(model@y, ties.method = "first")
                 } else {
                   y_numeric <- as.numeric(model@y)
                 }

                 if (is.matrix(pred_values)) {
                   # Multinomiaal: waarschijnlijkheid van de geobserveerde klasse pakken
                   observed_probs <- pred_values[cbind(1:n, y_numeric)]

                   # Correctie: Vermijd 0'en en 1'en die de log-likelihood kunnen verpesten
                   observed_probs <- pmax(pmin(observed_probs, 1 - 1e-10), 1e-10)

                   # Gemiddelde waarschijnlijkheid van het model
                   mean_prob <- mean(observed_probs)

                   # Verbeterde schaal: gebruik log-odds als betrouwbaarder alternatief
                   efron_score <- mean(log(observed_probs / (1 - observed_probs))) - mean(log(mean_prob / (1 - mean_prob)))

                   # Terugschalen naar 0-1 range
                   r2_value <- 1 / (1 + exp(-efron_score))

                   r2_value
                 } else {
                   # Standaard Efron voor binomiale/continue modellen
                   1 - (sum((y_numeric - pred_values)^2) / sum((y_numeric - mean(y_numeric))^2))
                 }
               },
               "tjur" = {
                 if (length(unique(model@y)) != 2) {
                   warning("Tjur's R^2 is only valid for binomial models.")
                   return(NA)
                 }
                 pred_values <- predict(model, type = "response")
                 mean(pred_values[model@y == max(model@y)]) - mean(pred_values[model@y == min(model@y)])
               }
  )

  return(r2)
}


