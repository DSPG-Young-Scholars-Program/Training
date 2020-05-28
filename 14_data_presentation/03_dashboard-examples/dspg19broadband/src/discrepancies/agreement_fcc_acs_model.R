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
# Load ---------------------------------------------------------------------------
#

acsdata <- read_csv("./data/working/acs_2013-17/nogeo/acs_2013-17_calc_tract.csv", col_types = cols(NAME.x = "c"))
intdata <- readRDS("./data/working/intervals/data_int.Rds")


#
# Join ---------------------------------------------------------------------------
#

data <- left_join(intdata, acsdata)


#
# Prepare ---------------------------------------------------------------------------
#

# Missingness
data <- data %>% select(urbanicity, State, primRUCA, secRUCA, acs_within_fcc, population, area, hs_or_less,
                        poverty, age_65_older, hispanic, black, family, foreign, workfromhome, longcommute, 
                        assistance, unemploy, vacant, renters, yearbuilt, rentburden, nointernet) %>% st_set_geometry(NULL)
gg_miss_var(data)

data <- data %>% select(-rentburden, -yearbuilt, -longcommute, -workfromhome)
gg_miss_var(data)

# Drop 15 missing unemployment and 2 missing HS or less
data <- data %>% filter(!is.na(unemploy) & !is.na(hs_or_less))
gg_miss_var(data)

# Split
data_rural <- data %>% filter(urbanicity == "Rural") %>% select(-urbanicity)
data_urban <- data %>% filter(urbanicity != "Rural") %>% select(-urbanicity)
data <- data %>% select(-urbanicity)

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
n_features <- length(setdiff(names(data_train), "acs_within_fcc"))

discr_m1 <- ranger(
  acs_within_fcc ~ ., 
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

# Grid search setup
hyper_grid <- expand.grid(
  num.trees = n_features * c(seq(5, 17, 4)),           # number of trees
  mtry = floor(n_features * c(seq(0.01, 0.50, 0.25))),  # split rule
  min.node.size = c(seq(1, 10, 4)),                      # tree complexity
  replace = c(TRUE, FALSE),                               # sampling scheme
  sample.fraction = c(seq(0.05, 1, 0.5)),                    # sampling scheme
  rmse = NA                                               # results placeholder
)

# Full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = acs_within_fcc ~ ., 
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
  acs_within_fcc ~ ., 
  data = data_train,
  num.trees = 289, 
  mtry = 4, 
  min.node.size = 5, 
  replace = TRUE,
  sample.fraction = 0.55, 
  importance = "permutation",
  seed = 2410
)

discr_m1imp <- ranger(
  acs_within_fcc ~ ., 
  data = data_train,
  num.trees = 289, 
  mtry = 4, 
  min.node.size = 5, 
  replace = TRUE,
  sample.fraction = 0.55, 
  importance = "impurity",
  seed = 2410
)

p1 <- vip::vip(discr_m1perm, bar = FALSE) + 
   ggtitle("RFC predicting FCC-ACS estimate congruence (all tracts):\nPermutation-based variable importance") + 
   labs(caption = "Note: RFC = Random forest classification.\nFCC = Federal Communications Commission.\nACS = American Community Survey.")
p2 <- vip::vip(discr_m1imp, bar = FALSE) +
  ggtitle("\nImpurity-based variable importance")  + 
  labs(caption = "Note: RFC = Random forest classification.\nFCC = Federal Communications Commission.\nACS = American Community Survey.")

tracts_all_imp <- gridExtra::grid.arrange(p1, p2, nrow = 1)

# Predict
preds <- predict(discr_m1perm, data_test)
preds <- data.frame(preds$predictions)

comparison <- cbind(acs_within_fcc = data_test$acs_within_fcc, preds = preds$preds.predictions)
comparison <- as.data.frame(comparison)
head(comparison)

round(prop.table(table(comparison$acs_within_fcc, comparison$preds)), 4)


#
# Random forest regression: RURAL ---------------------------------------------------------------------------
#

# Sample model
n_features <- length(setdiff(names(data_train_rural), "difference_availability"))

discr_m1 <- ranger(
  acs_within_fcc ~ ., 
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
    formula         = acs_within_fcc~ ., 
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
  acs_within_fcc ~ ., 
  data = data_train_rural,
  num.trees = 180, 
  mtry = 7, 
  min.node.size = 8.2, 
  replace = TRUE,
  sample.fraction = 0.45, 
  importance = "permutation",
  seed = 2410
)

discr_m1imp <- ranger(
  acs_within_fcc ~ ., 
  data = data_train_rural,
  num.trees = 180, 
  mtry = 7, 
  min.node.size = 8.2, 
  replace = TRUE,
  sample.fraction = 0.45, 
  importance = "impurity",
  seed = 2410
)

p1 <- vip::vip(discr_m1perm, bar = FALSE) + 
  ggtitle("RFC predicting FCC-ACS estimate congruence (rural tracts):\nPermutation-based variable importance") + 
  labs(caption = "Note: RFC = Random forest classification.\nFCC = Federal Communications Commission.\nACS = American Community Survey.")
p2 <- vip::vip(discr_m1imp, bar = FALSE) + 
  ggtitle("\nImpurity-based variable importance")  + 
  labs(caption = "Note: RFC = Random forest classification.\nFCC = Federal Communications Commission.\nACS = American Community Survey.")

tracts_all_imp <- gridExtra::grid.arrange(p1, p2, nrow = 1)

# Predict
preds <- predict(discr_m1perm, data_test_rural)
preds <- data.frame(preds$predictions)

comparison <- cbind(acs_within_fcc = data_test_rural$acs_within_fcc, preds = preds$preds.predictions)
comparison <- as.data.frame(comparison)
head(comparison)

round(prop.table(table(comparison$acs_within_fcc, comparison$preds)), 4)


#
# Random forest regression: URBAN ---------------------------------------------------------------------------
#

# Sample model
n_features <- length(setdiff(names(data_train_urban), "difference_availability"))

discr_m1 <- ranger(
  acs_within_fcc ~ ., 
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
  num.trees = n_features * c(seq(5, 17, 4)),           # number of trees
  mtry = floor(n_features * c(seq(0.01, 0.50, 0.25))),  # split rule
  min.node.size = c(seq(1, 10, 4)),                      # tree complexity
  replace = c(TRUE, FALSE),                               # sampling scheme
  sample.fraction = c(seq(0.05, 1, 0.5)),                    # sampling scheme
  rmse = NA                                               # results placeholder
)

# Full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = acs_within_fcc ~ ., 
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
  acs_within_fcc ~ ., 
  data = data_train_urban,
  num.trees = 234, 
  mtry = 4, 
  min.node.size = 1, 
  replace = TRUE,
  sample.fraction = 0.55, 
  importance = "permutation",
  seed = 2410
)

discr_m1imp <- ranger(
  acs_within_fcc ~ ., 
  data = data_train_urban,
  num.trees = 234, 
  mtry = 4, 
  min.node.size = 1, 
  replace = TRUE,
  sample.fraction = 0.55, 
  importance = "impurity",
  seed = 2410
)

p1 <- vip::vip(discr_m1perm, bar = FALSE) + 
  ggtitle("RFC predicting FCC-ACS estimate congruence (urban tracts):\nPermutation-based variable importance") + 
  labs(caption = "Note: RFC = Random forest classification.\nFCC = Federal Communications Commission.\nACS = American Community Survey.")
p2 <- vip::vip(discr_m1imp, bar = FALSE) + 
  ggtitle("\nImpurity-based variable importance")  + 
  labs(caption = "Note: RFC = Random forest classification.\nFCC = Federal Communications Commission.\nACS = American Community Survey.")

tracts_all_imp <- gridExtra::grid.arrange(p1, p2, nrow = 1)

# Predict
preds <- predict(discr_m1perm, data_test_urban)
preds <- data.frame(preds$predictions)

comparison <- cbind(acs_within_fcc = data_test_urban$acs_within_fcc, preds = preds$preds.predictions)
comparison <- as.data.frame(comparison)
head(comparison)

round(prop.table(table(comparison$acs_within_fcc, comparison$preds)), 4)