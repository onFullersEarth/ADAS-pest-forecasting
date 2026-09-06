library(mvgam)
library(loo)
library(tidyverse)
pest_data <- read.csv("data/pest_data.csv")
new_years <- pest_data[c(455:463),]
pest_data <- pest_data[-c(455:463),] #remove dummy years

names(new_years) <- c("year","region",
                      "L1_ZT_DS","L1_YR_DS",
                      "L1_ZT_CI","L1_YR_CI",
                      "L2_ZT_DS","L2_YR_DS",
                      "L2_ZT_CI","L2_YR_CI")

new_years <- pivot_longer(new_years, cols = 3:10, names_to = "measure", values_to = "values")
new_years <- complete(new_years, year, region, measure)
new_years$type <- substr(new_years$measure, nchar(new_years$measure)-1, nchar(new_years$measure))
new_years$type <- as.factor(new_years$type)
new_years <- unite(new_years, col = "new_measure", c(2,3), remove = FALSE)
new_years$pathogen <- substr(new_years$measure, 4, 5)
new_years$pathogen <- as.factor(new_years$pathogen)
new_years$region <- as.factor(new_years$region)
new_years$measure <- as.factor(new_years$measure)
new_years$series <- factor(new_years$new_measure)
new_years$time <- new_years$year


new_years$values <- NA

names(pest_data) <- c("year","region",
                      "L1_ZT_DS","L1_YR_DS",
                      "L1_ZT_CI","L1_YR_CI",
                      "L2_ZT_DS","L2_YR_DS",
                      "L2_ZT_CI","L2_YR_CI")

#DS_mean <- mean(c(pest_data$L1_ZT_DS, pest_data$L1_YR_DS, pest_data$L2_ZT_DS, pest_data$L2_ZT_DS))
#DS_sd <- sd(c(pest_data$L1_ZT_DS, pest_data$L1_YR_DS, pest_data$L2_ZT_DS, pest_data$L2_ZT_DS))
#CI_mean <- mean(c(pest_data$L1_ZT_CI, pest_data$L1_YR_CI, pest_data$L2_ZT_CI, pest_data$L2_YR_CI))
#CI_sd <- sd(c(pest_data$L1_ZT_CI, pest_data$L1_YR_CI, pest_data$L2_ZT_CI, pest_data$L2_YR_CI))

#pest_data[,c(3,4,7,8)] <- (pest_data[,c(3,4,7,8)] - DS_mean)/DS_sd
#pest_data[,c(5,6,9,10)] <- (pest_data[,c(5,6,7,8)] - CI_mean)/CI_sd

pest_data <- pivot_longer(pest_data, cols = 3:10, names_to = "measure", values_to = "values")
pest_data <- complete(pest_data, year = 1971:2025, region, measure)
pest_data$type <- substr(pest_data$measure, nchar(pest_data$measure)-1, nchar(pest_data$measure))
pest_data$type <- as.factor(pest_data$type)
pest_data <- unite(pest_data, col = "new_measure", c(2,3), remove = FALSE)
pest_data$pathogen <- substr(pest_data$measure, 4, 5)
pest_data$pathogen <- as.factor(pest_data$pathogen)
pest_data$region <- as.factor(pest_data$region)
pest_data$measure <- as.factor(pest_data$measure)

#pest_data <- pest_data[pest_data$type == "DS",]
pest_data$series <- factor(pest_data$new_measure)
pest_data$time <- pest_data$year

plot_mvgam_series(data = pest_data,
                  y = "values",
                  series = "all")



full_data <- list()
full_data$data_train <- pest_data
full_data$data_test <- new_years
trend_maps <- data.frame(series = as.factor(unique(pest_data$new_measure)))
trend_maps$trend <- as.integer(unique(pest_data$type))
varmod_simple <- mvgam(
  formula = values ~ 1,
  data = full_data$data_train,
  trend_model = AR(p = 1),
  newdata = full_data$data_test,
  family = gaussian,
  burnin = 1000,
  samples = 1000,
  trend_map = trend_maps,
  share_obs_params = FALSE,
  silent = 0,
  control = list(refresh = 1, adapt.delta = 0.999),
)

plot(varmod_simple)
hindcasts <- hindcast(varmod_simple)
forecast_tests <- forecast(varmod_simple)

rmse_by_series <- summary(hindcasts) %>%
  filter(!is.na(truth)) %>%
  group_by(series) %>%
  summarise(rmse = sqrt(mean((truth - predQ50)^2)))

rmse_by_series <- separate_wider_delim(rmse_by_series, 1, "_", names = c("region","leaf","pathogen","measure"))
rmse_by_series$pathogen <- replace(rmse_by_series$pathogen, rmse_by_series$pathogen=="YR", "Yellow_rust")
rmse_by_series$pathogen <- replace(rmse_by_series$pathogen, rmse_by_series$pathogen=="ZT", "Zymoseptoria_tritici")
rmse_by_series$measure <- replace(rmse_by_series$measure, rmse_by_series$measure=="CI", "Crop_Incidence")
rmse_by_series$measure <- replace(rmse_by_series$measure, rmse_by_series$measure=="DS", "Disease_Severity")

rmse_by_series <- unite(rmse_by_series, col = "target", c(2,3,4))

forecast_2026 <- summary(forecast_tests) %>%
  filter(is.na(truth) & time == 2026) %>%
  summarise(forecast_value = predQ50)

forecast_2026$region <- rmse_by_series$region
forecast_2026$target <- rmse_by_series$target
forecast_2026$year <- 2026
forecast_2026 <- forecast_2026[,c(2,3,4,1)]

write.csv(forecast_2026, "./submission/pest_forecasts_2026.csv")
write.csv(rmse_by_series, "./submission/pest_model_performance.csv")
