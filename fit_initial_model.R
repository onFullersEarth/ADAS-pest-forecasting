library(brms)
library(loo)
library(ggplot2)
pest_data <- read.csv("data/pest_data.csv")

pest_formula <- brm(L1_Yellow_rust_Crop_Incidence ~ ar(time = Year, gr = Region, p = 5) + (1 | Region), data = pest_data)

pest_predicts <- posterior_predict(pest_formula)
preds <- cbind(
  Estimate = colMeans(pest_predicts), 
  Q5 = apply(pest_predicts, 2, quantile, probs = 0.05),
  Q95 = apply(pest_predicts, 2, quantile, probs = 0.95)
)

ggplot(cbind(pest_data, preds), aes(x = Year, y = Estimate)) +
  geom_smooth(aes(ymin = Q5, ymax = Q95), stat = "identity", linewidth = 0.5) +
  geom_point(aes(y = L1_Yellow_rust_Crop_Incidence))
