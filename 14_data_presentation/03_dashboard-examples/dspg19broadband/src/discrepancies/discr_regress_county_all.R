library(dplyr)
library(stargazer)
library(ggfortify)


#
# Read in data & prepare -------------------------------------------------------------------------------
#

# FCC
fcc_original <- read.csv("./data/working/fcc_processed_county_25.csv")
fcc <- fcc_original[, c("county", "availability_adv", "Population_2010", "RUCC_2013")]
fcc$county <- ifelse(nchar(fcc$county) == 4, paste0("0", fcc$county), fcc$county)
colnames(fcc)[colnames(fcc) == "county"] <- "FIPS"
fcc$availability_adv = fcc$availability_adv*100

# ACS
acs_original <- read.csv("./data/working/summary_acs_county.csv")
acs <- acs_original[, c("FIPS", "B28002_007_per")]
acs$FIPS <- ifelse(nchar(acs$FIPS) == 4, paste0("0", acs$FIPS), acs$FIPS)

# Microsoft
microsoft_original <- read.csv("./data/original/microsoft/microsoft.csv")
ms <- microsoft_original
ms$COUNTY.ID <- as.character(as.integer(ms$COUNTY.ID))
ms$COUNTY.ID <- ifelse(nchar(ms$COUNTY.ID) == 4, paste0("0", ms$COUNTY.ID), ms$COUNTY.ID)
colnames(ms)[colnames(ms) == "COUNTY.ID"] <- "FIPS"
ms$BROADBAND.USAGE <- as.character(as.factor(ms$BROADBAND.USAGE))
ms$BROADBAND.USAGE <- as.numeric(as.character(ms$BROADBAND.USAGE))
ms$BROADBAND.USAGE <- ms$BROADBAND.USAGE*100
ms <- ms[, c("FIPS", "BROADBAND.USAGE")]


#
# Merge ----------------------------------------------------------------------------------------------
#

# Notes on merge: see county_discrepancy_aland.R
# 02270 is an old FIPS code
# 46113 is an old FIPS code
# 51515 city is no longer a city, is now part of a county
# FCC includes American Samoa, Guam, Northern Mariana Islands, Puerto Rico, Virgin Islands

discr <- merge(fcc, acs, by = "FIPS", all.y=TRUE)
discr <- merge(discr, ms, by = "FIPS", all.y = TRUE)


#
# Create discrepancy measures, format RUCC codes ------------------------------------------------------
#

# RUCC dichotomization
discr$ru_binary <- ifelse(discr$RUCC_2013 > 3, "nonmetro", "metro")
discr$RUCC_2013 <- as.numeric(discr$RUCC_2013)

# FCC-ACS discrepancy
discr$dis_rel_fcc_acs <- round((discr$availability_adv) - discr$B28002_007_per, 1)
discr$dis_abs_fcc_acs <- abs(discr$dis_rel_fcc_acs)

# FCC-MS discrepancy
discr$dis_rel_fcc_ms <- round((discr$availability_adv) - discr$BROADBAND.USAGE, 1)
discr$dis_abs_fcc_ms <- abs(discr$dis_rel_fcc_ms)


#
# Add sociodemographic data ---------------------------------------------------------------------------
#

# Note: Proportions (x divided by total for the table) and percentages calculated elsewhere. See acs_transform.R.

# Unemployed
empl <- read.csv("./data/working/acs_transform/acs_empl.csv")
empl$GEOID <- as.character(as.integer(empl$GEOID))
empl$GEOID <- ifelse(nchar(empl$GEOID) == 4, paste0("0", empl$GEOID), empl$GEOID)
discr <- merge(discr, empl[,c("B23025_005_per", "GEOID")], by.x = "FIPS", by.y="GEOID", all.x = TRUE)
colnames(discr)[colnames(discr) == "B23025_005_per"] <- "unempl"

# Hispanic
ethn <- read.csv("./data/working/acs_transform/acs_ethn.csv")
ethn$GEOID <- as.character(as.integer(ethn$GEOID))
ethn$GEOID <- ifelse(nchar(ethn$GEOID) == 4, paste0("0", ethn$GEOID), ethn$GEOID)
discr <- merge(discr, ethn[, c("B03003_003_per", "GEOID")], by.x = "FIPS", by.y="GEOID", all.x = TRUE)
colnames(discr)[colnames(discr) == "B03003_003_per"] <- "his_lat"

# Minority
race <- read.csv("./data/working/acs_transform/acs_race.csv")
race$minority_per <- race$B02001_003_per + race$B02001_004_per + race$B02001_005_per + race$B02001_006_per + race$B02001_007_per + race$B02001_008_per
race$GEOID <- as.character(as.integer(race$GEOID))
race$GEOID <- ifelse(nchar(race$GEOID) == 4, paste0("0", race$GEOID) ,race$GEOID)
discr <- merge(discr, race[, c("GEOID", "minority_per")], by.x = "FIPS", by.y = "GEOID", all.x = TRUE)

# Poverty
pov <- read.csv("./data/working/acs_transform/acs_pov.csv")
pov$under_1 <- pov$B17026_002_per + pov$B17026_003_per + pov$B17026_004_per
pov$GEOID <- as.character(as.integer(pov$GEOID))
pov$GEOID <- ifelse(nchar(pov$GEOID) == 4, paste0("0", pov$GEOID), pov$GEOID)
discr <- merge(discr, pov[, c("GEOID", "under_1")], by.x = "FIPS", by.y = "GEOID", all.x = TRUE)

# Education
edu <- read.csv("./data/working/acs_transform/acs_edu.csv")
edu$no_hs <- edu$B15003_002_per + edu$B15003_003_per + edu$B15003_004_per + edu$B15003_005_per + edu$B15003_006_per + edu$B15003_007_per + edu$B15003_008_per + edu$B15003_009_per + edu$B15003_010_per + edu$B15003_011_per + edu$B15003_012_per + edu$B15003_013_per + edu$B15003_014_per + edu$B15003_015_per + edu$B15003_016_per
edu$GEOID <- as.character(as.integer(edu$GEOID))
edu$GEOID <- ifelse(nchar(edu$GEOID) == 4, paste0("0", edu$GEOID), edu$GEOID)
discr <- merge(discr, edu[, c("GEOID", "no_hs")], by.x = "FIPS", by.y = "GEOID", all.x = TRUE)


#
# Regression: FCC-ACS ---------------------------------------------------------------------------
#

hist(discr$dis_abs_fcc_acs)

# FCC-ACS absolute discrepancy all counties
reg_fccacs1all <- lm(dis_abs_fcc_acs ~ ru_binary,
                  data = discr)
reg_fccacs2all <- lm(dis_abs_fcc_acs ~ ru_binary + unempl + his_lat + minority_per + under_1 + no_hs,
                  data = discr)
stargazer(reg_fccacs1all, reg_fccacs2all, no.space = TRUE, digits = 2, type = "text")

# FCC-ACS absolute discrepancy nonmetro counties
reg_fccacs1nonm <- lm(dis_abs_fcc_acs ~ RUCC_2013, 
                  data = discr[discr$ru_binary == "nonmetro", ])
reg_fccacs2nonm <- lm(dis_abs_fcc_acs ~ RUCC_2013 + unempl + his_lat + minority_per + under_1 + no_hs,
                  data = discr[discr$ru_binary == "nonmetro", ])
stargazer(reg_fccacs1nonm, reg_fccacs2nonm, no.space = TRUE, digits = 2, type = "text")

# FCC-ACS absolute discrepancy metro counties
reg_fccacs1m <- lm(dis_abs_fcc_acs ~ RUCC_2013, 
                  data = discr[discr$ru_binary == "metro", ])
reg_fccacs2m <- lm(dis_abs_fcc_acs ~ RUCC_2013 + unempl + his_lat + minority_per + under_1 + no_hs,
                  data = discr[discr$ru_binary == "metro", ])
stargazer(reg_fccacs1m, reg_fccacs2m, no.space = TRUE, digits = 2, type = "text")

stargazer(reg_fccacs1all, reg_fccacs2all, reg_fccacs1nonm, reg_fccacs2nonm, reg_fccacs1m, reg_fccacs2m, no.space = TRUE, digits = 2, type = "text",
          column.labels = c("All counties", " Nonmetro counties", "Metro counties"), column.separate = c(2, 2, 2))

# Diagnostics
autoplot(reg_fccacs2all)
# Residuals vs. fitted: A horizontal line with no pattern indicates a linear reationship. --> Pretty good.
# Normal Q-Q: Residual points following the dashed line indicate normal residual distribution. --> Pretty good, off at extremes.
# Scale-Location: Horizontal line with equal point spread indicates homoskedasticity. --> Not the worst. 

autoplot(reg_fccacs2nonm)
autoplot(reg_fccacs2m)


#
# Regression: FCC-MS ---------------------------------------------------------------------------
#

hist(discr$dis_abs_fcc_ms)

# FCC-MS absolute discrepancy all counties
reg_fccms1all <- lm(dis_abs_fcc_ms ~ ru_binary,
                     data = discr)
reg_fccms2all <- lm(dis_abs_fcc_ms ~ ru_binary + unempl + his_lat + minority_per + under_1 + no_hs,
                     data = discr)
stargazer(reg_fccms1all, reg_fccms2all, no.space = TRUE, digits = 2, type = "text")

# FCC-MS absolute discrepancy nonmetro counties
reg_fccms1nonm <- lm(dis_abs_fcc_ms ~ RUCC_2013, 
                      data = discr[discr$ru_binary == "nonmetro", ])
reg_fccms2nonm <- lm(dis_abs_fcc_ms ~ RUCC_2013 + unempl + his_lat + minority_per + under_1 + no_hs,
                      data = discr[discr$ru_binary == "nonmetro", ])
stargazer(reg_fccms1nonm, reg_fccms2nonm, no.space = TRUE, digits = 2, type = "text")

# FCC-MS absolute discrepancy metro counties
reg_fccms1m <- lm(dis_abs_fcc_ms ~ RUCC_2013, 
                   data = discr[discr$ru_binary == "metro", ])
reg_fccms2m <- lm(dis_abs_fcc_ms ~ RUCC_2013 + unempl + his_lat + minority_per + under_1 + no_hs,
                   data = discr[discr$ru_binary == "metro", ])
stargazer(reg_fccms1m, reg_fccms2m, no.space = TRUE, digits = 2, type = "text")

stargazer(reg_fccms1all, reg_fccms2all, reg_fccms1nonm, reg_fccms2nonm, reg_fccms1m, reg_fccms2m, no.space = TRUE, digits = 2, type = "text",
          column.labels = c("All counties", " Nonmetro counties", "Metro counties"), column.separate = c(2, 2, 2))

# Diagnostics
autoplot(reg_fccms2all)
# Residuals vs. fitted: A horizontal line with no pattern indicates a linear reationship. --> Not the worst.
# Normal Q-Q: Residual points following the dashed line indicate normal residual distribution. --> Pretty good, some oddities at lower extremes.
# Scale-Location: Horizontal line with equal point spread indicates homoskedasticity. --> Pretty good.

autoplot(reg_fccms2nonm)
autoplot(reg_fccms2m)


#
# Comparison -----------------------------------------------------------------------------------------------
#


# FCC-ACS vs FCC-MS
stargazer(reg_fccacs2all, reg_fccms2all, reg_fccacs2nonm, reg_fccms2nonm, reg_fccacs2m, reg_fccms2m, no.space = TRUE, digits = 2, type = "text",
          column.labels = c("All counties", " Nonmetro counties", "Metro counties"), column.separate = c(2, 2, 2))


#
# Try out caret -----------------------------------------------------------------------------------------------
#

library(caret)

discr <- discr %>% filter(!is.na(dis_abs_fcc_acs))

# create training set indices with 80% of data
set.seed(20190711)  # For reproducibility
# Create index for testing and training data
indices_train <- createDataPartition(y = discr$dis_abs_fcc_acs, p = 0.8, list = FALSE)

# preprocess
# subset iris data to training
training <- discr[indices_train, ]
# subset the rest to test
testing <- discr[-indices_train, ]

# define crossvalidation
control <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"

# GLM
fit.glm <- train(dis_abs_fcc_acs ~ ru_binary + unempl + his_lat + minority_per + under_1 + no_hs, data=training,
                         method = "glm", metric = metric, trControl = control)
# Random forest
fit.rf <- train(dis_abs_fcc_acs ~ ru_binary + unempl + his_lat + minority_per + under_1 + no_hs, data=training,
                        method = "rf", metric = metric, trControl = control)

# SVM
fit.svm <- train(dis_abs_fcc_acs ~ ru_binary + unempl + his_lat + minority_per + under_1 + no_hs, data=training,
                 method = "svmLinear", metric = metric, trControl = control)

