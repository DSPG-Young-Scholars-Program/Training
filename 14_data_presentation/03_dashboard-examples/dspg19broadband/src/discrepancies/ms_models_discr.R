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


#
# Read in data & prepare -------------------------------------------------------------------------------
#

# FCC 2017 county
fcc_orig <- read_csv("./data/working/fcc_processed_county_25_2017.csv")
fcc <- fcc_orig %>% select(-X1, -Description)
fcc$availability_adv <- fcc$availability_adv*100
fcc$county <- ifelse(nchar(fcc$county) == 4, paste0("0", fcc$county), fcc$county)
colnames(fcc)[colnames(fcc) == "county"] <- "GEOID"
remove(fcc_orig)

# ACS 2013-17 county (see discr_getacs_county for coding)
acs_orig <- st_read("./data/working/acs_2013-17/calc/acs_2013-17_calc_count.shp")
acs <- acs_orig %>% select(-AFFGEOI, -AWATER)
acs$GEOID <- as.character(acs$GEOID)
acs$GEOID <- ifelse(nchar(acs$GEOID) == 4, paste0("0", acs$GEOID), acs$GEOID)
remove(acs_orig)

# Microsoft 2018 county
ms_orig <- read_csv("./data/original/microsoft/microsoft.csv")
ms <- ms_orig
ms$`COUNTY ID` <- as.character(ms$`COUNTY ID`)
ms$`COUNTY ID` <- ifelse(nchar(ms$`COUNTY ID`) == 4, paste0("0", ms$`COUNTY ID`), ms$`COUNTY ID`)
colnames(ms)[colnames(ms) == "COUNTY ID"] <- "GEOID"
colnames(ms)[colnames(ms) == "COUNTY NAME"] <- "countyname"
colnames(ms)[colnames(ms) == "BROADBAND USAGE"] <- "usage"
any(ms$usage == "-")
sum(ms$usage == "-")
ms$usage <- ifelse(ms$usage == "-", NA, ms$usage)
ms$usage <- as.numeric(ms$usage)
ms$usage <- ms$usage*100
ms <- ms %>% select(-"BROADBAND AVAILABILITY PER FCC")
remove(ms_orig)


#
# Merge ----------------------------------------------------------------------------------------------
#

# Notes on merge: see county_discrepancy_aland.R
# FCC has more rows because it includes American Samoa, Guam, Northern Mariana Islands, Puerto Rico, Virgin Islands
# 02270 is an old FIPS code
# 46113 is an old FIPS code
# 51515 city is no longer a city, is now part of a county

sum(fcc$GEOID %in% acs$GEOID)
discr <- left_join(fcc, acs, by = "GEOID")

sum(discr$GEOID %in% ms$GEOID)
discr <- left_join(ms, discr, by = "GEOID")


#
# Create measures, address missingness --------------------------------------------------------------------------------------
#

# Land area is in square meters (https://www.census.gov/quickfacts/fact/note/US/LND110210), convert to square miles (divide by 2,589,988)
discr$landarea <- (discr$ALAND / 2589988)

# RUCC dichotomization
discr$ru_binary <- ifelse(discr$RUCC_2013 > 3, "nonmetro", "metro")

# FCC-MS discrepancy (numeric)
discr$dis_rel_fcc_ms <- discr$availability_adv - discr$usage
table(discr$dis_rel_fcc_ms == 0)

# FCC-MS discrepancy (categorical): under, match, over
discr <- discr %>% mutate(dis_cat_fcc_ms = case_when(dis_rel_fcc_ms > 0 ~ "Over",
                                                     dis_rel_fcc_ms == 0 ~ "Match",
                                                     dis_rel_fcc_ms < 0 ~ "Under"))
discr$dis_cat_fcc_ms <- ordered(discr$dis_cat_fcc_ms, levels = c("Under", "Match", "Over"))

# Missingness
miss <- discr %>% select(availability_adv, usage, dis_rel_fcc_ms,
                         RUCC_2013, ru_binary, landarea, popultn,
                         hs_r_ls, poverty, ag_65_l, hispanc, black, family, foreign,
                         wrkfrmh, lngcmmt, assstnc, labrfrc, vacant, renters, yearblt, rntbrdn, nontrnt)
table(is.na(miss$availability_adv))
table(is.na(miss$usage))

miss_var_summary(miss)

# Filter to observations with recorded outcome
discr <- discr %>% filter(!is.na(dis_rel_fcc_ms))
miss_var_summary(discr)

# Plot outcome
hist(discr$dis_rel_fcc_ms)


#
# Prepare ---------------------------------------------------------------------------
#

# Select data
data <- discr %>% select(dis_rel_fcc_ms, RUCC_2013, ru_binary, state, landarea, popultn, hs_r_ls, poverty, ag_65_l, hispanc, black, family, foreign,
                         wrkfrmh, lngcmmt, assstnc, labrfrc, vacant, renters, yearblt, rntbrdn, nontrnt)

# Filter out if needed
miss_var_summary(data)
data <- data %>% filter(!is.na(yearblt), !is.na(rntbrdn))

# Split
data_rural <- data %>%
  filter(ru_binary == "nonmetro") %>%
  select(dis_rel_fcc_ms, RUCC_2013, state, landarea, popultn, hs_r_ls, poverty, ag_65_l, hispanc, black, family, foreign,
         wrkfrmh, lngcmmt, assstnc, labrfrc, vacant, renters, yearblt, rntbrdn, nontrnt)
data_urban <- data %>%
  filter(ru_binary == "metro") %>%
  select(dis_rel_fcc_ms, RUCC_2013, state, landarea, popultn, hs_r_ls, poverty, ag_65_l, hispanc, black, family, foreign,
         wrkfrmh, lngcmmt, assstnc, labrfrc, vacant, renters, yearblt, rntbrdn, nontrnt)
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
n_features <- length(setdiff(names(data_train), "dis_rel_fcc_ms"))

discr_m1 <- ranger(
  dis_rel_fcc_ms ~ ., 
  data = data_train,
  num.trees = n_features * 10, # Number of trees needs to be sufficiently large to stabilize error rate. Rule of thumb = start with 10 x the number of features, but adjust to other hyperparameters (mtry, node size). More trees = more robust/stable error estimates and variable importance measures + more computational time. 
  mtry = floor(n_features / 3), # Controls the split-variable randomization feature. Helps balance low tree correlation with reasonable predictive strength. With regression problems the default value is often mtry=(p/3) and for classification mtry=sqrt(p) (p = number of features). When there are fewer relevant predictors, a higher mtry value performs better because it makes it more likely to select those features with the strongest signal. When there are many relevant predictors, a lower mtry might perfrom better.
  min.node.size = 5, # When adjusting node size start with three values between 1–10 and adjust depending on impact to accuracy and run time. Regression default is 5. 
  replace = TRUE, # If you have inbalanced categorical features try sampling without replacement.
  sample.fraction = 1, # Decreasing the sample size leads to more diverse trees and thereby lower between-tree correlation, which can have a positive effect on the prediction accuracy. If there are a few dominating features in your data set, reducing the sample size can also help to minimize between-tree correlation. Assess 3–4 values of sample sizes ranging from 25%–100%.
  oob.error = TRUE,
  seed = 2410
)

discr_m1
default_rmse <- sqrt(discr_m1$prediction.error) # OOB RMSE

# Grid search setup
hyper_grid <- expand.grid(
  num.trees = n_features * c(seq(1, 15, 3)),           # number of trees
  mtry = floor(n_features * c(seq(0.01, 0.50, 0.10))),  # split rule
  min.node.size = c(seq(1, 10, 1.2)),                      # tree complexity
  replace = c(TRUE, FALSE),                               # sampling scheme
  sample.fraction = c(seq(0.05, 1, 0.4)),                    # sampling scheme
  rmse = NA                                               # results placeholder
)

# Full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = dis_rel_fcc_ms ~ ., 
    data            = data_train, 
    num.trees       = hyper_grid$num.trees[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 2410,
  )
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

# View top 10 performing models and consider gain from default model
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)

# Variable importance from best model
discr_m1perm <- ranger(
  dis_rel_fcc_ms ~ ., 
  data = data_train,
  num.trees = 200, 
  mtry = 8, 
  min.node.size = 5.8, 
  replace = FALSE, 
  sample.fraction = 0.45, 
  importance = "permutation",
  seed = 2410
)

discr_m1imp <- ranger(
  dis_rel_fcc_ms ~ ., 
  data = data_train,
  num.trees = 200, 
  mtry = 8, 
  min.node.size = 5.8, 
  replace = FALSE, 
  sample.fraction = 0.45, 
  importance = "impurity",
  seed = 2410
)

p1 <- vip::vip(discr_m1perm, bar = FALSE) + ggtitle("RFR predicting broadband coverage discrepancies (all counties):\nPermutation-based variable importance") + 
  scale_x_discrete("Variable", labels = c("Workers from home", "In labor force", "In poverty", "Land area",
                                          "Foreign-born", "Social assistance", "Vacant properties", "High school or less", "Population", 
                                          "No internet")) +
  labs(caption = "Note: RFR = Random forest regression.")
p2 <- vip::vip(discr_m1imp, bar = FALSE) + ggtitle("\nImpurity-based variable importance")  + 
  scale_x_discrete(labels = c("Family households", "In labor force", "Social assistance", "Workers from home",
                              "Foreign-born", "High school or less", "Vacant properties", "Land area", "Population", 
                              "No internet")) +
  labs(caption = "Note: RFR = Random forest regression.")
counties_all_imp <- gridExtra::grid.arrange(p1, p2, nrow = 1)
ggsave("./doc/discrepancies/counties_all_discr_imp_perm.png", plot = p1, device = "png")
ggsave("./doc/discrepancies/counties_all_discr_imp_impur.png", plot = p2, device = "png")
ggsave("./doc/discrepancies/counties_all_discr_imp.png", plot = counties_all_imp, device = "png")

importance_pvalues(discr_m1perm, method = "altmann", formula = dis_rel_fcc_ms ~ ., data = data_train)
importance_pvalues(discr_m1imp, method = "altmann", formula = dis_rel_fcc_ms ~ ., data = data_train)

# Predict
preds <- predict(discr_m1perm, data_test)
preds <- data.frame(preds$predictions)

comparison <- cbind(dis_rel_fcc_ms = data_test$dis_rel_fcc_ms, preds = preds$preds.predictions)
comparison <- as.data.frame(comparison)
head(comparison)

# Plot actual versus predicted FCC-MS discrepancy (in %)
ggplot(data = comparison, aes(x = dis_rel_fcc_ms, y = preds)) + 
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_point(size = 1) + 
  labs(title = "Actual versus predicted FCC-MS broadband coverage report discrepancy (%)", x = "Actual FCC-MS report discrepancy (%)", 
       y = "Predicted FCC-MS report discrepancy (%)", caption = "Notes: FCC = Federal Communications Commission. MS = Microsoft.\nRed line indicates perfect prediction.",
       subtitle = "RMSE = 19.10%") +
  theme_hc()
ggsave("./doc/discrepancies/counties_all_discr_preds.png", plot = last_plot(), device = "png")


#
# Random forest regression: URBAN ---------------------------------------------------------------------------
#

# Sample model
n_features <- length(setdiff(names(data_train_urban), "dis_rel_fcc_ms"))

discr_m1 <- ranger(
  dis_rel_fcc_ms ~ ., 
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

# Grid search setup
hyper_grid <- expand.grid(
  num.trees = n_features * c(seq(1, 15, 3)),           # number of trees
  mtry = floor(n_features * c(seq(0.01, 0.50, 0.10))),  # split rule
  min.node.size = c(seq(1, 10, 1.2)),                      # tree complexity
  replace = c(TRUE, FALSE),                               # sampling scheme
  sample.fraction = c(seq(0.05, 1, 0.4)),                    # sampling scheme
  rmse = NA                                               # results placeholder
)

# Full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = dis_rel_fcc_ms ~ ., 
    data            = data_train_urban, 
    num.trees       = hyper_grid$num.trees[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 2410,
  )
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

# View top 10 performing models and consider gain from default model
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)

# Variable importance from best model
discr_m1perm <- ranger(
  dis_rel_fcc_ms ~ ., 
  data = data_train_urban,
  num.trees = 260, 
  mtry = 4, 
  min.node.size = 1, 
  replace = FALSE, 
  sample.fraction = 0.45, 
  importance = "permutation",
  seed = 2410
)

discr_m1imp <- ranger(
  dis_rel_fcc_ms ~ ., 
  data = data_train_urban,
  num.trees = 260, 
  mtry = 4, 
  min.node.size = 1, 
  replace = FALSE, 
  sample.fraction = 0.45, 
  importance = "impurity",
  seed = 2410
)

p1 <- vip::vip(discr_m1perm, bar = FALSE) + ggtitle("RFR predicting broadband coverage discrepancies (urban counties):\nPermutation-based variable importance") + 
  scale_x_discrete("Variable", labels = c("Renters", "In labor force", "Workers from home", "In poverty",
                                          "Social assistance", "Hispanic", "Population", "High school or less", "No internet", 
                                          "Foreign-born")) +
  labs(caption = "Note: RFR = Random forest regression. FCC = Federal Communications Commission.")
p2 <- vip::vip(discr_m1imp, bar = FALSE) + ggtitle("\nImpurity-based variable importance")  + 
  scale_x_discrete(labels = c("Vacant properties", "Land area", "In poverty", "Social assistance",
                              "Workers from home", "Hispanic", "Population", "High school or less", "Foreign-born", 
                              "No internet"))
  labs(caption = "Note: RFR = Random forest regression. FCC = Federal Communications Commission.")
counties_urban_imp <- gridExtra::grid.arrange(p1, p2, nrow = 1)
ggsave("./doc/discrepancies/counties_urban_discr_imp_perm.png", plot = p1, device = "png")
ggsave("./doc/discrepancies/counties_urban_discr_imp_impur.png", plot = p2, device = "png")
ggsave("./doc/discrepancies/counties_urban_discr_imp.png", plot = counties_urban_imp, device = "png")

importance_pvalues(discr_m1perm, method = "altmann", formula = dis_rel_fcc_ms ~ ., data = data_train_urban)
importance_pvalues(discr_m1imp, method = "altmann", formula = dis_rel_fcc_ms ~ ., data = data_train_urban)

# Predict
preds <- predict(discr_m1perm, data_test_urban)
preds <- data.frame(preds$predictions)

comparison <- cbind(dis_rel_fcc_ms = data_test_urban$dis_rel_fcc_ms, preds = preds$preds.predictions)
comparison <- as.data.frame(comparison)
head(comparison)

# Plot actual versus predicted FCC-MS discrepancy (in %)
ggplot(data = comparison, aes(x = dis_rel_fcc_ms, y = preds)) + 
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_point(size = 1) + 
  labs(title = "Actual versus predicted FCC-MS broadband coverage report discrepancy (%) [urban counties]", x = "Actual FCC-MS discrepancy (%)", 
       y = "Predicted FCC-MS discrepancy (%)", caption = "Notes: FCC = Federal Communications Commission. MS = Microsoft.\nRed line indicates perfect prediction.",
       subtitle = "RMSE = 14.45%") +
  theme_hc()
ggsave("./doc/discrepancies/counties_urban_discr_preds.png", plot = last_plot(), device = "png")


#
# Random forest regression: RURAL---------------------------------------------------------------------------
#

# Sample model
n_features <- length(setdiff(names(data_train_rural), "dis_rel_fcc_ms"))

discr_m1 <- ranger(
  dis_rel_fcc_ms ~ ., 
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

# Grid search setup
hyper_grid <- expand.grid(
  num.trees = n_features * c(seq(1, 15, 3)),           # number of trees
  mtry = floor(n_features * c(seq(0.01, 0.50, 0.10))),  # split rule
  min.node.size = c(seq(1, 10, 1.2)),                      # tree complexity
  replace = c(TRUE, FALSE),                               # sampling scheme
  sample.fraction = c(seq(0.05, 1, 0.4)),                    # sampling scheme
  rmse = NA                                               # results placeholder
)

# Full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = dis_rel_fcc_ms ~ ., 
    data            = data_train_rural, 
    num.trees       = hyper_grid$num.trees[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 2410,
  )
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

# View top 10 performing models and consider gain from default model
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)

# Variable importance from best model
discr_m1perm <- ranger(
  dis_rel_fcc_ms ~ ., 
  data = data_train_rural,
  num.trees = 260, 
  mtry = 6, 
  min.node.size = 1, 
  replace = TRUE, 
  sample.fraction = 0.45, 
  importance = "permutation",
  seed = 2410
)

discr_m1imp <- ranger(
  dis_rel_fcc_ms ~ ., 
  data = data_train_rural,
  num.trees = 260, 
  mtry = 6, 
  min.node.size = 1, 
  replace = TRUE, 
  sample.fraction = 0.45, 
  importance = "impurity",
  seed = 2410
)

p1 <- vip::vip(discr_m1perm, bar = FALSE) + ggtitle("RFR predicting broadband coverage discrepancies(rural counties):\nPermutation-based variable importance") + 
  scale_x_discrete("Variable", labels = c("Age 65+", "High school or less", "In labor force", "In poverty",
                                          "No internet", "Black", "Social assistance", "Population", "Vacant properties", 
                                          "Land area")) +
  labs(caption = "Note: RFR = Random forest regression. FCC = Federal Communications Commission.")
p2 <- vip::vip(discr_m1imp, bar = FALSE) + ggtitle("\nImpurity-based variable importance")  + 
  scale_x_discrete(labels = c("In labor force", "Social asssistance", "No internet", "Workers from home", 
                              "Age 65+", "High school or less", "Long commute", "Population", "Vacant properties", 
                              "Land area")) +
  labs(caption = "Note: RFR = Random forest regression. FCC = Federal Communications Commission.")
counties_rural_imp <- gridExtra::grid.arrange(p1, p2, nrow = 1)
ggsave("./doc/discrepancies/counties_rural_discr_imp_perm.png", plot = p1, device = "png")
ggsave("./doc/discrepancies/counties_rural_discr_imp_impur.png", plot = p2, device = "png")
ggsave("./doc/discrepancies/counties_rural_discr_imp.png", plot = counties_rural_imp, device = "png")

importance_pvalues(discr_m1perm, method = "altmann", formula = dis_rel_fcc_ms ~ ., data = data_train_rural)
importance_pvalues(discr_m1imp, method = "altmann", formula = dis_rel_fcc_ms ~ ., data = data_train_rural)

# Predict
preds <- predict(discr_m1perm, data_test_rural)
preds <- data.frame(preds$predictions)

comparison <- cbind(dis_rel_fcc_ms = data_test_rural$dis_rel_fcc_ms, preds = preds$preds.predictions)
comparison <- as.data.frame(comparison)
head(comparison)

# Plot actual versus predicted FCC-MS report discrepancy (in %)
ggplot(data = comparison, aes(x = dis_rel_fcc_ms, y = preds)) + 
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_point(size = 1) + 
  labs(title = "Actual versus predicted FCC-MS broadband coverage report discrepancy (%) [rural counties]", x = "Actual FCC-MS report discrepancy (%)", 
       y = "Predicted FCC-MS report discrepancy (%)", caption = "Notes: FCC = Federal Communications Commission.\nRed line indicates perfect prediction.",
       subtitle = "RMSE = 21.04%") +
  theme_hc()
ggsave("./doc/discrepancies/counties_rural_discr_preds.png", plot = last_plot(), device = "png")


#
# Linear model: all counties ------------------------------------------------------------------------------------------------
#

discr_m2 <- lm(dis_rel_fcc_ms ~ ., data = data_train)

# Predict
preds <- predict(discr_m2, data_test)
preds <- data.frame(preds)

comparison <- cbind(dis_rel_fcc_ms = data_test$dis_rel_fcc_ms, preds = preds)
comparison <- as.data.frame(comparison)
head(comparison)

# Plot actual versus predicted FCC broadband availability (in %)
ggplot(data = comparison, aes(x = dis_rel_fcc_ms, y = preds)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "Actual versus predicted FCC broadband availability (%)", x = "Actual FCC broadband availability (%)", 
       y = "Predicted FCC broadband availability (%)")


#
# Linear model: urban counties ------------------------------------------------------------------------------------------------
#

discr_m2 <- lm(dis_rel_fcc_ms ~ ., data = data_train_urban)

# Predict
preds <- predict(discr_m2, data_test_urban)
preds <- data.frame(preds)

comparison <- cbind(dis_rel_fcc_ms = data_test_urban$dis_rel_fcc_ms, preds = preds)
comparison <- as.data.frame(comparison)
head(comparison)

# Plot actual versus predicted FCC broadband availability (in %)
ggplot(data = comparison, aes(x = dis_rel_fcc_ms, y = preds)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "Actual versus predicted FCC broadband availability (%) [urban counties]", x = "Actual FCC broadband availability (%)", 
       y = "Predicted FCC broadband availability (%)")


#
# Linear model: rural counties ------------------------------------------------------------------------------------------------
#

discr_m2 <- lm(dis_rel_fcc_ms ~ ., data = data_train_rural)

# Predict
preds <- predict(discr_m2, data_test_rural)
preds <- data.frame(preds)

comparison <- cbind(dis_rel_fcc_ms = data_test_rural$dis_rel_fcc_ms, preds = preds)
comparison <- as.data.frame(comparison)
head(comparison)

# Plot actual versus predicted FCC broadband availability (in %)
ggplot(data = comparison, aes(x = dis_rel_fcc_ms, y = preds)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(title = "Actual versus predicted FCC broadband availability (%) [rural counties]", x = "Actual FCC broadband availability (%)", 
       y = "Predicted FCC broadband availability (%)")


