library(brms)
library(loo)
library(ggplot2)
pest_data <- read.csv("data/pest_data.csv")
pest_data <- pest_data[-c(455:463),] #remove dummy years
pest_formula_1 <- brm(L1_Yellow_rust_Crop_Incidence ~ 1 + ar(time = Year, gr = Region, p = 1, cov = F),
                    data = pest_data)
pest_formula_2 <- brm(L1_Yellow_rust_Crop_Incidence ~ 1 + ar(time = Year, gr = Region, p = 2, cov = F),
                      data = pest_data)
pest_formula_5 <- brm(L1_Yellow_rust_Crop_Incidence ~ 1 + ar(time = Year, gr = Region, p = 5, cov = F),
                      data = pest_data)
pest_formula_1 <- add_criterion(pest_formula_1, c("loo","bayes_R2"))
pest_formula_2 <- add_criterion(pest_formula_2, c("loo","bayes_R2"))
pest_formula_5 <- add_criterion(pest_formula_5, c("loo","bayes_R2"))
#Theory:
#Model with two components:
#A equilibrium component of endemic infection
#A spiking component of severe infection
#No covariates, let's do this from first principles

pest_predicts <- posterior_predict(pest_formula)
preds <- cbind(
  Estimate = colMeans(pest_predicts), 
  Q5 = apply(pest_predicts, 2, quantile, probs = 0.05),
  Q95 = apply(pest_predicts, 2, quantile, probs = 0.95)
)

ggplot(cbind(pest_data, preds), aes(x = Year, y = Estimate)) +
  geom_smooth(aes(ymin = Q5, ymax = Q95), stat = "identity", linewidth = 0.5) +
  geom_point(aes(y = L1_Yellow_rust_Crop_Incidence)) + theme_classic()
test <- cbind(pest_data,preds)

table(test$L1_Yellow_rust_Crop_Incidence >= test$Q5 & test$L1_Yellow_rust_Crop_Incidence <= test$Q95)
