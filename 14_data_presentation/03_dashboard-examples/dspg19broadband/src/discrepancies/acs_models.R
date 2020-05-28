# note A: include RUCA codes rather than RUCC codes
# note B: get land area from shapefile rather than density/population
# note C: use other ACS variables that Teja included;
# working from home, vacant properties, long commute, social assistance, in labor force, renters
# (see: discr_getacs_county.R)
# note D: the full hyperparameter grid search will take ~6 hours for Census tract random forets (run overnight)

# Land area is in square meters (https://www.census.gov/quickfacts/fact/note/US/LND110210), convert to square miles (divide by 2,589,988)
# discr$landarea <- (discr$ALAND / 2589988)

# try to predict discrepency in FCC data (FCC-ACS, FCC-Microsoft)
# Random Forest approach; report AUC, accuracy/precision
# Census Tract level (72,438 data points), ACS (availability only)

library(here)
library(data.table)
library(maditr)
library(readr)
library(dplyr)
library(sf)
library(randomForest)
library(ranger)
library(rsample)
library(caret)
library(naniar)
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(viridis)
library(naniar)

options(scipen = 999)

#
# Read in data & prepare -------------------------------------------------------------------------------
#

# FCC 2017 tract
fcc_file <- here("data", "working", "fcc_processed_tract_25.csv")
fcc <- fread(fcc_file, colClasses=c(state_fips="character",county_short="character",county = "character",tract_short="character", tract="character"))

# ACS 2013-17 tract
acs_file <- here("data", "working", "summary_acs_census_tract.csv")
acs <- fread(acs_file, colClasses=c(state="character",county="character",census_tract="character"))

# load remaining ACS variables by tract
acs_file2 <- here("data", "working", "brookingsOLS", "broadband_acs_by_census_tract_2015.csv")
acs2 <- fread(acs_file2)

#
# Merge ----------------------------------------------------------------------------------------------
#

fcc_acs = merge(fcc, acs, by.x = c('state_fips', 'county_short', 'tract_short'), by.y = c('state', 'county', 'census_tract')) %>%
  dt_filter(state_fips==state_fips)  

# Notes on merge:
# FCC has more rows because it includes American Samoa, Guam, Northern Mariana Islands, Puerto Rico, Virgin Islands
# 02270 is an old FIPS code
# 46113 is an old FIPS code
# 51515 city is no longer a city, is now part of a county

# join to brookingsOLS/broadband_acs_by_census_tract_2015.csv
fcc_acs$tract <- as.numeric(fcc_acs$tract)
fcc_acs_final <- merge(fcc_acs, acs2, by.x = "tract", by.y = "GEOID") 

# create the difference metric
fcc_acs_final$fcc_acs_difference <- fcc_acs_final$availability_adv*100 - fcc_acs_final$B28002_007_per

# subset to create the final dataset
rf_data <- fcc_acs_final %>% dplyr::transmute(
  difference_availability = fcc_acs_difference,
  #difference_subscription = fcc_acs_difference_subscription,
  #availability = availability_adv,
  #subscription = subscription_continuous,
  #acsbroadband = B28002_007_per,
  state = STATEFP,
  population,
  hs_or_less,
  poverty,
  age_65_older,
  hispanic,
  black,
  area=population/density,
  family,
  foreign,
  #rural,
  RUCC_2013
)
discr <- rf_data[rowSums(is.na(rf_data)) == 0,] # 293 (0.4%) tracts with missing data

# new definition of rural: RUCC_2013 > 3
discr$ru_binary <- ifelse(discr$RUCC_2013 > 3, "nonmetro", "metro")

#
# Prepare ---------------------------------------------------------------------------
#

data <- discr
# Split
data_rural <- data %>%
  filter(ru_binary == "nonmetro") %>% select(-ru_binary)
data_urban <- data %>%
  filter(ru_binary == "metro") %>% select(-ru_binary)
data <- data %>% select(-ru_binary)

# Set seed
set.seed(2410)

# Split
split <- initial_split(data, prop = 0.8)
data_train <- training(split)
data_test <- testing(split)

split_urban <- initial_split(data_urban, prop = 0.8)
data_train_urban <- training(split_urban)
data_test_urban <- testing(split_urban)

split_rural <- initial_split(data_rural, prop = 0.8)
data_train_rural <- training(split_rural)
data_test_rural <- testing(split_rural)

#
# Random forest regression: ALL ---------------------------------------------------------------------------
#

# Sample model
n_features <- length(setdiff(names(data_train), "difference_availability"))

discr_m1 <- ranger(
  difference_availability ~ ., 
  data = data_train,
  num.trees = n_features * 10, # Number of trees needs to be sufficiently large to stabilize error rate. Rule of thumb = start with 10 x the number of features, but adjust to other hyperparameters (mtry, node size). More trees = more robust/stable error estimates and variable importance measures + more computational time. 
  mtry = floor(n_features / 3), # Controls the split-variable randomization feature. Helps balance low tree correlation with reasonable predictive strength. With regression problems the default value is often mtry=(p/3) and for classification mtry=sqrt(p) (p = number of features). When there are fewer relevant predictors, a higher mtry value performs better because it makes it more likely to select those features with the strongest signal. When there are many relevant predictors, a lower mtry might perfrom better.
  min.node.size = 5, # When adjusting node size start with three values between 1–10 and adjust depending on impact to accuracy and run time. Regression default is 5. 
  replace = TRUE, # If you have inbalanced categorical features try sampling without replacement.
  sample.fraction = 1, # Decreasing the sample size leads to more diverse trees and thereby lower between-tree correlation, which can have a positive effect on the prediction accuracy. If there are a few dominating features in your data set, reducing the sample size can also help to minimize between-tree correlation. Assess 3–4 values of sample sizes ranging from 25%–100%.
  seed = 2410
)

discr_m1
default_rmse <- sqrt(discr_m1$prediction.error) # OOB RMSE

# Variable importance from default model
discr_m1perm <- ranger(
  difference_availability ~ ., 
  data = data_train,
  num.trees = n_features * 10, 
  mtry = floor(n_features / 3), 
  min.node.size = 5, 
  replace = TRUE,
  sample.fraction = 1, 
  importance = "permutation",
  seed = 2410
)

discr_m1imp <- ranger(
  difference_availability ~ ., 
  data = data_train,
  num.trees = n_features * 10, 
  mtry = floor(n_features / 3), 
  min.node.size = 5, 
  replace = TRUE,
  sample.fraction = 1, 
  importance = "impurity",
  seed = 2410
)

p1 <- vip::vip(discr_m1perm, bar = FALSE) + ggtitle("RFR predicting FCC-ACS broadband discrepancy (all tracts):\nPermutation-based variable importance") + 
  #  scale_x_discrete("Variable", labels = c("Renters", "Social Assistance", "Vacant properties", "In labor force",
  #                                          "High school or less", "Land area", "RUCC", "No internet", "Population", 
  #                                          "Microsoft usage")) +
  labs(caption = "Note: RFR = Random forest regression. FCC = Federal Communications Commission.")
p2 <- vip::vip(discr_m1imp, bar = FALSE) + ggtitle("\nImpurity-based variable importance")  + 
  #  scale_x_discrete(labels = c("Renters", "Long commute", "In labor force", "High school or less",
  #                              "RUCC", "Land area", "Vacant properties", "No internet", "Population", 
  #                              "Microsoft usage")) +
  labs(caption = "Note: RFR = Random forest regression. FCC = Federal Communications Commission.")

tracts_all_imp <- gridExtra::grid.arrange(p1, p2, nrow = 1)
ggsave("./doc/discrepancies/tracts_all_imp_perm.png", plot = p1, device = "png", width=10, height=6)
ggsave("./doc/discrepancies/tracts_all_imp_impur.png", plot = p2, device = "png", width=10, height=6)
ggsave("./doc/discrepancies/tracts_all_imp.png", plot = tracts_all_imp, device = "png", width=10, height=6)
#importance_pvalues(discr_m1perm, method = "altmann", formula = difference_availability ~ ., data = data_train)
#importance_pvalues(discr_m1imp, method = "altmann", formula = difference_availability ~ ., data = data_train)

# Predict
preds <- predict(discr_m1perm, data_test)
preds <- data.frame(preds$predictions)

comparison <- cbind(difference_availability = data_test$difference_availability, preds = preds$preds.predictions)
comparison <- as.data.frame(comparison)
head(comparison)

# Plot actual versus predicted FCC broadband availability (in %)
ggplot(data = comparison, aes(x = difference_availability, y = preds)) + 
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  geom_point(size = 0.5) + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  labs(title = "Actual versus predicted FCC-ACS broadband coverage report discrepancy (%) [all tracts]", x = "Actual FCC-ACS discrepancy (%)", 
       y = "Predicted FCC-ACS discrepancy (%)", caption = "Notes: FCC = Federal Communications Commission.\nRed line indicates perfect prediction.",
       subtitle = "RMSE = 16.07%") +
  theme_hc()
ggsave("./doc/discrepancies/tracts_all_preds.png", plot = last_plot(), device = "png", width=10, height=6)

#
# Random forest regression: RURAL ---------------------------------------------------------------------------
#

# Sample model
n_features <- length(setdiff(names(data_train), "difference_availability"))

discr_m1 <- ranger(
  difference_availability ~ ., 
  data = data_train_rural,
  num.trees = n_features * 10, # Number of trees needs to be sufficiently large to stabilize error rate. Rule of thumb = start with 10 x the number of features, but adjust to other hyperparameters (mtry, node size). More trees = more robust/stable error estimates and variable importance measures + more computational time. 
  mtry = floor(n_features / 3), # Controls the split-variable randomization feature. Helps balance low tree correlation with reasonable predictive strength. With regression problems the default value is often mtry=(p/3) and for classification mtry=sqrt(p) (p = number of features). When there are fewer relevant predictors, a higher mtry value performs better because it makes it more likely to select those features with the strongest signal. When there are many relevant predictors, a lower mtry might perfrom better.
  min.node.size = 5, # When adjusting node size start with three values between 1–10 and adjust depending on impact to accuracy and run time. Regression default is 5. 
  replace = TRUE, # If you have inbalanced categorical features try sampling without replacement.
  sample.fraction = 1, # Decreasing the sample size leads to more diverse trees and thereby lower between-tree correlation, which can have a positive effect on the prediction accuracy. If there are a few dominating features in your data set, reducing the sample size can also help to minimize between-tree correlation. Assess 3–4 values of sample sizes ranging from 25%–100%.
  seed = 2410
)

discr_m1
default_rmse <- sqrt(discr_m1$prediction.error) # OOB RMSE

# Variable importance from default model
discr_m1perm <- ranger(
  difference_availability ~ ., 
  data = data_train_rural,
  num.trees = n_features * 10, 
  mtry = floor(n_features / 3), 
  min.node.size = 5, 
  replace = TRUE,
  sample.fraction = 1, 
  importance = "permutation",
  seed = 2410
)

discr_m1imp <- ranger(
  difference_availability ~ ., 
  data = data_train_rural,
  num.trees = n_features * 10, 
  mtry = floor(n_features / 3), 
  min.node.size = 5, 
  replace = TRUE,
  sample.fraction = 1, 
  importance = "impurity",
  seed = 2410
)

p1 <- vip::vip(discr_m1perm, bar = FALSE) + ggtitle("RFR predicting FCC-ACS broadband discrepancy (rural tracts):\nPermutation-based variable importance") + 
  #  scale_x_discrete("Variable", labels = c("Renters", "Social Assistance", "Vacant properties", "In labor force",
  #                                          "High school or less", "Land area", "RUCC", "No internet", "Population", 
  #                                          "Microsoft usage")) +
  labs(caption = "Note: RFR = Random forest regression. FCC = Federal Communications Commission.")
p2 <- vip::vip(discr_m1imp, bar = FALSE) + ggtitle("\nImpurity-based variable importance")  + 
  #  scale_x_discrete(labels = c("Renters", "Long commute", "In labor force", "High school or less",
  #                              "RUCC", "Land area", "Vacant properties", "No internet", "Population", 
  #                              "Microsoft usage")) +
  labs(caption = "Note: RFR = Random forest regression. FCC = Federal Communications Commission.")

tracts_all_imp <- gridExtra::grid.arrange(p1, p2, nrow = 1)
ggsave("./doc/discrepancies/tracts_rural_imp_perm.png", plot = p1, device = "png", width=10, height=6)
ggsave("./doc/discrepancies/tracts_rural_imp_impur.png", plot = p2, device = "png", width=10, height=6)
ggsave("./doc/discrepancies/tracts_rural_imp.png", plot = tracts_all_imp, device = "png", width=10, height=6)
#importance_pvalues(discr_m1perm, method = "altmann", formula = difference_availability ~ ., data = data_train)
#importance_pvalues(discr_m1imp, method = "altmann", formula = difference_availability ~ ., data = data_train)

# Predict
preds <- predict(discr_m1perm, data_test_rural)
preds <- data.frame(preds$predictions)

comparison <- cbind(difference_availability = data_test_rural$difference_availability, preds = preds$preds.predictions)
comparison <- as.data.frame(comparison)
head(comparison)

# Plot actual versus predicted FCC broadband availability (in %)
ggplot(data = comparison, aes(x = difference_availability, y = preds)) + 
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  geom_point(size = 0.5) + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  labs(title = "Actual versus predicted FCC-ACS broadband coverage report discrepancy (%) [rural tracts]", x = "Actual FCC-ACS discrepancy (%)", 
       y = "Predicted FCC-ACS discrepancy (%)", caption = "Notes: FCC = Federal Communications Commission.\nRed line indicates perfect prediction.",
       subtitle = "RMSE = 26.87%") +
  theme_hc()
ggsave("./doc/discrepancies/tracts_rural_preds.png", plot = last_plot(), device = "png", width=10, height=6)


#
# Random forest regression: URBAN ---------------------------------------------------------------------------
#

# Sample model
n_features <- length(setdiff(names(data_train), "difference_availability"))

discr_m1 <- ranger(
  difference_availability ~ ., 
  data = data_train_urban,
  num.trees = n_features * 10, # Number of trees needs to be sufficiently large to stabilize error rate. Rule of thumb = start with 10 x the number of features, but adjust to other hyperparameters (mtry, node size). More trees = more robust/stable error estimates and variable importance measures + more computational time. 
  mtry = floor(n_features / 3), # Controls the split-variable randomization feature. Helps balance low tree correlation with reasonable predictive strength. With regression problems the default value is often mtry=(p/3) and for classification mtry=sqrt(p) (p = number of features). When there are fewer relevant predictors, a higher mtry value performs better because it makes it more likely to select those features with the strongest signal. When there are many relevant predictors, a lower mtry might perfrom better.
  min.node.size = 5, # When adjusting node size start with three values between 1–10 and adjust depending on impact to accuracy and run time. Regression default is 5. 
  replace = TRUE, # If you have inbalanced categorical features try sampling without replacement.
  sample.fraction = 1, # Decreasing the sample size leads to more diverse trees and thereby lower between-tree correlation, which can have a positive effect on the prediction accuracy. If there are a few dominating features in your data set, reducing the sample size can also help to minimize between-tree correlation. Assess 3–4 values of sample sizes ranging from 25%–100%.
  seed = 2410
)

discr_m1
default_rmse <- sqrt(discr_m1$prediction.error) # OOB RMSE

# Variable importance from default model
discr_m1perm <- ranger(
  difference_availability ~ ., 
  data = data_train_urban,
  num.trees = n_features * 10, 
  mtry = floor(n_features / 3), 
  min.node.size = 5, 
  replace = TRUE,
  sample.fraction = 1, 
  importance = "permutation",
  seed = 2410
)

discr_m1imp <- ranger(
  difference_availability ~ ., 
  data = data_train_urban,
  num.trees = n_features * 10, 
  mtry = floor(n_features / 3), 
  min.node.size = 5, 
  replace = TRUE,
  sample.fraction = 1, 
  importance = "impurity",
  seed = 2410
)

p1 <- vip::vip(discr_m1perm, bar = FALSE) + ggtitle("RFR predicting FCC-ACS broadband discrepancy (urban tracts):\nPermutation-based variable importance") + 
  #  scale_x_discrete("Variable", labels = c("Renters", "Social Assistance", "Vacant properties", "In labor force",
  #                                          "High school or less", "Land area", "RUCC", "No internet", "Population", 
  #                                          "Microsoft usage")) +
  labs(caption = "Note: RFR = Random forest regression. FCC = Federal Communications Commission.")
p2 <- vip::vip(discr_m1imp, bar = FALSE) + ggtitle("\nImpurity-based variable importance")  + 
  #  scale_x_discrete(labels = c("Renters", "Long commute", "In labor force", "High school or less",
  #                              "RUCC", "Land area", "Vacant properties", "No internet", "Population", 
  #                              "Microsoft usage")) +
  labs(caption = "Note: RFR = Random forest regression. FCC = Federal Communications Commission.")

tracts_all_imp <- gridExtra::grid.arrange(p1, p2, nrow = 1)
ggsave("./doc/discrepancies/tracts_urban_imp_perm.png", plot = p1, device = "png", width=10, height=6)
ggsave("./doc/discrepancies/tracts_urban_imp_impur.png", plot = p2, device = "png", width=10, height=6)
ggsave("./doc/discrepancies/tracts_urban_imp.png", plot = tracts_all_imp, device = "png", width=10, height=6)
#importance_pvalues(discr_m1perm, method = "altmann", formula = difference_availability ~ ., data = data_train)
#importance_pvalues(discr_m1imp, method = "altmann", formula = difference_availability ~ ., data = data_train)

# Predict
preds <- predict(discr_m1perm, data_test_urban)
preds <- data.frame(preds$predictions)

comparison <- cbind(difference_availability = data_test_urban$difference_availability, preds = preds$preds.predictions)
comparison <- as.data.frame(comparison)
head(comparison)

# Plot actual versus predicted FCC broadband availability (in %)
ggplot(data = comparison, aes(x = difference_availability, y = preds)) + 
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  geom_point(size = 0.5) + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  labs(title = "Actual versus predicted FCC-ACS broadband coverage report discrepancy (%) [urban tracts]", x = "Actual FCC-ACS discrepancy (%)", 
       y = "Predicted FCC-ACS discrepancy (%)", caption = "Notes: FCC = Federal Communications Commission.\nRed line indicates perfect prediction.",
       subtitle = "RMSE = 12.99%") +
  theme_hc()
ggsave("./doc/discrepancies/tracts_urban_preds.png", plot = last_plot(), device = "png", width=10, height=6)
