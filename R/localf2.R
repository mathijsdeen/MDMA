library(nlme)
library(MuMIn)

data(Orthodont, package = "nlme")

fm1 <- lme(distance ~ Sex * age, ~ 1 | Subject, data = Orthodont)

fmnull <- lme(distance ~ 1, ~ 1 | Subject, data = Orthodont)

r.squaredGLMM(fmnull)
r.squaredGLMM(fm1)
r.squaredGLMM(fm1, fmnull)
r.squaredGLMM(update(fm1, . ~ Sex), fmnull)


r.squaredLR(fm1)
r.squaredLR(fm1, null.RE = TRUE)

#####

library(performance)
print(model_performance(fm1))

#####

library(effectsize)
f2(fm1)

####

library(nlme)
library(performance)
library(MuMIn)
data("Dialyzer")

predictors <- c("index", "QB", "pressure")
f2_results <- setNames(vector("list", length(predictors)), predictors)
full_model2 <- lme(fixed  = paste("rate", "~", paste(predictors, collapse = "+")) |>
                     as.formula(),
                   random = ~1 | Subject,
                   data   = Dialyzer)
summary(full_model2)
r2_full <- r.squaredGLMM(full_model2)[,"R2m"]

for(var in predictors) {
  reduced_set <- paste(setdiff(predictors, var), collapse = "+")
  reduced_model <- lme(fixed  = paste("rate", "~", reduced_set) |>
                         as.formula(),
                       random = ~ 1 | Subject,
                       data   = Dialyzer)
  assign(paste0("m.", var), value = reduced_model)
  r2_reduced <- r.squaredGLMM(reduced_model)[,"R2m"]
  f2_results[[var]] <- (r2_full - r2_reduced) / (1 - r2_full)
}

random_var_full <- as.numeric(VarCorr(full_model2))
lme(fixed  = rate ~  index + pressure,
    random = pdDiag(~1 | Subject, fixed = random_var),
    data   = Dialyzer)


f2_results
r2_full
r.squaredGLMM(m.index)[,"R2m"]
r.squaredGLMM(m.QB)[,"R2m"]
r.squaredGLMM(m.pressure)[,"R2m"]

summary(full_model)
summary(m.index)
summary(m.QB)
summary(m.pressure)

getVarCov(full_model)

#####
library(nlme)
library(brms)

data("Dialyzer")

full_model <- brm(rate ~ QB + index + pressure + (1 | Subject), data = Dialyzer)
ranef_variance <- VarCorr(full_model)$Subject$sd[1]
fixed_prior <- set_prior(paste0("normal(", ranef_variance, ", 0.001)"),
                         class = "sd",
                         group = "Subject")
reduced_model <- brm(rate ~ index + pressure + (1 | Subject),
                     data = Dialyzer,
                     prior = fixed_prior)
reduced_model_free <- brm(rate ~ index + pressure + (1 | Subject), data = Dialyzer)
VarCorr(full_model)
VarCorr(reduced_model) # approximately the same
(bayes_R2(full_model)[1] - bayes_R2(reduced_model)[1]) / (1 - bayes_R2(full_model)[1])

bayes_R2(full_model)
r.squaredGLMM(full_model2)


#######

library(glmmTMB)
library(performance)
library(nlme)

# Fit het volledige model
full_model <- glmmTMB(rate ~ QB + pressure + (1 + QB | Subject), data = Dialyzer)
summary(full_model)

# Haal de random-effect standaarddeviatie op
random_sd <- attr(VarCorr(full_model)$cond$Subject, "stddev")  # Standaarddeviatie
print(random_sd)

# Gereduceerd model met gefixeerde random-effectvariantie
reduced_model <- glmmTMB(
  rate ~ index + pressure + (1 + QB | Subject),
  data = Dialyzer,
  map = list(theta = factor(c(NA))),  # Fixeer de parameter
  start = list(theta = log(random_sd))  # Log-schaal voor random-effect SD
)

# Bekijk het gereduceerde model
summary(reduced_model)

r2_full <- r2_nakagawa(full_model)
r2_reduced <- r2_nakagawa(reduced_model)

# Extract marginale R^2
R2_marginal_full <- r2_full$R2_marginal
R2_marginal_reduced <- r2_reduced$R2_marginal

# Bereken Cohen's f²
f2 <- (R2_marginal_full - R2_marginal_reduced) / (1 - R2_marginal_full)
cat("Cohen's f²:", f2, "\n")

effectsize::effectsize(full_model2)


Dit lijkt goed te werken. Idee: schrijf een functie waar je een lme/lmer-object in kunt stoppen, en waarin glmmTMB wordt gebruikt om de f2-waarden te berekenen. Optimisatie van glmmTMB misschien helpen door als start values de parameterschattingen uit het object mee te geven? Dit voorkomt evt. ook discrepanties in de schattingen van glmmTMB t.o.v. het object.

#########

library(ClusterBootstrap)


