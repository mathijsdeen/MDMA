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


#Dit lijkt goed te werken. Idee: schrijf een functie waar je een lme/lmer-object in kunt stoppen, en waarin glmmTMB wordt gebruikt om de f2-waarden te berekenen. Optimisatie van glmmTMB misschien helpen door als start values de parameterschattingen uit het object mee te geven? Dit voorkomt evt. ook discrepanties in de schattingen van glmmTMB t.o.v. het object.

#########

library(ClusterBootstrap)

############

m.full <- lm(mpg ~ cyl + wt + drat, data = mtcars)

R2full <- summary(m.full)$r.squared
preds <- attr(terms(m.full), which = "term.labels")
preds
R2preds <- rep(NA_real_, length(preds))

for(pred in seq_along(preds)){
  R2preds[pred] <- summary(update(m.full, as.formula(paste(". ~ . -", preds[pred]))))$r.squared
}

for(pred in seq_along(preds)){
  R2preds[pred] <- m.full |>
    update(as.formula(paste(". ~ . -", preds[pred]))) |>
    summary() |>
    (\(s) s$r.squared)()
}

options(scipen=1)
(R2full - R2preds) / (1 - R2full) |> round(3)

R2_cyl <- summary(lm(mpg~wt + drat, data = mtcars))$r.squared
R2_wt <- summary(lm(mpg~ cyl + drat, data = mtcars))$r.squared
R2_drat <- summary(lm(mpg~ cyl + wt, data = mtcars))$r.squared

(R2full - R2_cyl) / (1 - R2full)
(R2full - R2_wt) / (1 - R2full)
(R2full - R2_drat) / (1 - R2full)

# test voor lm
m.full <- lm(mpg ~ cyl * wt, data = mtcars)
R2full <- summary(m.full)$r.squared
f2Local.lm(m.full, method="r.squared")
m.cyl <- lm(mpg ~ cyl:wt + wt + drat, data = mtcars)
R2cyl <- summary(m.cyl)$r.squared
(R2full - R2cyl) / (1 - R2full)
m.wt <- lm(mpg ~ cyl + cyl:wt + drat, data = mtcars)
R2wt <- summary(m.wt)$r.squared
(R2full - R2wt) / (1 - R2full)
m.cyl_wt <- lm(mpg ~ cyl + wt + drat, data = mtcars)
R2cyl_wt <- summary(m.cyl_wt)$r.squared
(R2full - R2cyl_wt) / (1 - R2full)
m.drat <- lm(mpg ~ cyl*wt, data = mtcars)
R2drat <- summary(m.drat)$r.squared
(R2full - R2drat) / (1 - R2full)

View(f2Local.lm(m.full))

attr(terms(m.full), which = "term.labels")

# test voor glm
## binary logistic
library(performance)
m <- glm(vs ~  cyl*wt + mpg, data = mtcars, family = "binomial")
r2_coxsnell(m)
f2Local(m, method = "r2_coxsnell") |> as.numeric()
f2Local(m, method = "r2_efron")
f2Local(m, method = "r2_nakagawa")

## multinomial logistic
library(mlogit)
data("Fishing", package = "mlogit")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
View(Fish)
model <- mlogit(mode ~ price + catch, data = Fish)
f2Local(model)
summary(model)
model.price <- mlogit(mode ~ catch, data = Fish)
model.catch <- mlogit(mode ~ price, data = Fish)
summary(model.price)
summary(model.catch)
summary(model)
r2_mcfadden(model)
r2_mcfadden(model.price)
r2_mcfadden(model.catch)
(r2_mcfadden(model) - r2_mcfadden(model.price)) / (1 - r2_mcfadden(model))

Fish2 <- mlogit.data(Fishing, choice = "mode", shape = "wide")
View(Fish2)

chatUpLines <- read.delim("https://studysites.sagepub.com/dsur/study/DSUR%20Data%20Files/Chapter%208/Chat-Up%20Lines.dat",
                          header = TRUE, sep = "\t")
chatUpLines$Gender <- relevel(as.factor(chatUpLines$Gender), ref = 2)
chatUpLines_ML <- mlogit.data(chatUpLines, choice = "Success", shape = "wide")
#View(chatUpLines_ML)
m.chatUpLines <- mlogit(Success ~ 1 | Good_Mate + Funny*Gender + Gender*Sex,
                        data = chatUpLines_ML, method = "nr",
                        reflevel = "No response/Walk Off", print.level = 0)
summary(m.chatUpLines)

library(VGAM)
m.cUL <- vglm(Success ~ Good_Mate + Funny + Gender + Sex + Gender:Sex + Funny:Gender,
              family = multinomial, data = chatUpLines)
summary(m.cUL)
r2_efron(m.cUL)

class(m.cUL)
performance::r2(m.cUL)

m.chatUpLines_GoodMate <- mlogit(Success ~ 1 | Funny + Gender + Sex + Gender:Sex + Funny:Gender,
                                 data = chatUpLines_ML, method = "nr",
                                 reflevel = "No response/Walk Off", print.level = 0)
summary(m.chatUpLines_GoodMate)
library(performance)
R2full <- r2_mcfadden(m.chatUpLines)
R2Good_mate <- r2_mcfadden(m.chatUpLines_GoodMate)
R2Good_mate

(R2full - R2Good_mate) / (1 - R2full)

f2Local.vglm(3,method="efron")
m.cUL@family |> View()

R2.vglm(model, method = "efron")
##########
model
update(model, formula = model@y ~ 1)
vglm(Success ~ 1, data = chatUpLines, family = multinomial)
model <- vglm(Success ~ Good_Mate + Funny + Gender*Sex, data = chatUpLines,
              family = multinomial)
logLik_full <- logLik(model)

pred_values <- predict(model, type="response")

pred_values

max.col(model@y, ties.method="first")
View(model@y)
