# try to predict discrepency in FCC data (FCC-ACS, FCC-Microsoft)
# Random Forest approach; report AUC, accuracy/precision
#   a) Census Tract level (72,438 data points), ACS (availability, subscription)
#   b) County level (~3,000 data points), ACS (availability,subscription)
#   c) County level (~3,000 data points), Microsoft (availability, subscription)

library(dplyr)
library(here)
library(data.table)
library(maditr)

options(scipen = 999)

# read in the final dataset (look in app.R)
acs_file <- here("data", "working", "summary_acs_census_tract.csv")
acs <- fread(acs_file, colClasses=c(state="character",county="character",census_tract="character"))
fcc_file <- here("data", "working", "fcc_processed_tract_25.csv")
fcc <- fread(fcc_file, colClasses=c(state_fips="character",county_short="character",county = "character",tract_short="character", tract="character"))   

fcc_acs = merge(fcc, acs, by.x = c('state_fips', 'county_short', 'tract_short'), by.y = c('state', 'county', 'census_tract')) %>%
  dt_filter(state_fips==state_fips)  
names(fcc_acs)

fcc_acs$fcc_acs_difference <- fcc_acs$availability_adv*100 - fcc_acs$B28002_007_per
# hist(fcc_acs$fcc_acs_difference) # predict this!

# attach *other* ACS varaibles; we only have these at the County level, for now (but I had them by tract)
acs_vars <- read.csv("./data/working/brookingsOLS/broadband_acs_by_census_tract_2015.csv")
# join to brookingsOLS/broadband_acs_by_census_tract_2015.csv
fcc_acs$tract <- as.numeric(fcc_acs$tract)
fcc_acs_final <- merge(fcc_acs, acs_vars, by.x = "tract", by.y = "GEOID") 

fcc_acs_final$fcc_acs_difference_subscription <- fcc_acs_final$subscription_continuous*100 - fcc_acs$B28002_007_per

# create the final dataset
rf_data <- fcc_acs_final %>% dplyr::select(
  difference_availability = fcc_acs_difference,
  difference_subscription = fcc_acs_difference_subscription,
  availability = availability_adv,
  subscription = subscription_continuous,
  acsbroadband = B28002_007_per,
  state = STATEFP,
  population,
  hs_or_less,
  poverty,
  age_65_older,
  hispanic,
  black,
  density,
  family,
  foreign,
  rural,
  rucc = RUCC_2013
)
rf_data_full <- rf_data[rowSums(is.na(rf_data)) == 0,]

rf_data_availability <- rf_data_full[,-"difference_subscription"]
rf_data_subscription <- rf_data_full[,-"difference_availability"]

# ---------------------------------------
# first, fit a random forest to predict the difference between ACS responses and FCC subscription rate
library(caret)

inTrain <- createDataPartition(rf_data_subscription$difference_subscription, p = .8, list = FALSE)
train <- rf_data_subscription[inTrain,]
test <- rf_data_subscription[-inTrain,]

mtry_def <- 3 #floor(sqrt(ncol(train))*.75) # How many columns to select in each bootstrap sample?
t_grid <- expand.grid(mtry= c(mtry_def))

model.rf <- train(difference_subscription ~ .,
                  data = train,
                  method = "rf",
                  ntree = 50, # How many trees to grow in total?
                  tuneGrid = t_grid)

print(model.rf)

predictions <- predict(model.rf, test)
RMSE <- sqrt(sum((predictions - test$difference_subscription)^2)/length(predictions))
print(RMSE)
# RMSE: mean %error between adjusted FCC and ACS
# RMSE is 7.88%

plot(model.rf$finalModel) # model error vs #trees

save.image("predictFCC.RData")

model.rf.imp <- train(difference_subscription ~ .,
                      data = train,
                      method = "rf",
                      ntree = 50, # How many trees to grow in total?
                      tuneGrid = t_grid,
                      importance = TRUE)

predictions <- predict(model.rf.imp, test)
RMSE <- sqrt(sum((predictions - test$difference_subscription)^2)/length(predictions))
print(RMSE)
# R^2 = 0.83, RMSE = 4%


# variable importance
varImp(model.rf) # returns importance functions for each variables

save.image("predictFCC.RData")

# ---------------------------------------
# then fit a random forest to predict the difference between ACS responses and FCC availability
inTrain2 <- createDataPartition(rf_data_availability$difference_availability, p = .8, list = FALSE)
train2 <- rf_data_availability[inTrain2,]
test2 <- rf_data_availability[-inTrain2,]

mtry_def <- 3 #floor(sqrt(ncol(train))*.75) # How many columns to select in each bootstrap sample?
t_grid <- expand.grid(mtry= c(mtry_def))

model.rf2 <- train(difference_availability ~ .,
                  data = train2,
                  method = "rf",
                  ntree = 50, # How many trees to grow in total?
                  tuneGrid = t_grid)

print(model.rf2)

predictions2 <- predict(model.rf2, test2)
RMSE2 <- sqrt(sum((predictions2 - test2$difference_availability)^2)/length(predictions2))
print(RMSE2)
# 7.96%
# R^2 = 0.87

# RMSE: mean %error between adjusted FCC and ACS
plot(model.rf2$finalModel) # model error vs #trees

save.image("predictFCC.RData")

model.rf.imp2 <- train(difference_availability ~ .,
                      data = train2,
                      method = "rf",
                      ntree = 50, # How many trees to grow in total?
                      tuneGrid = t_grid,
                      importance = TRUE)

# variable importance
varImp(model.rf.imp2) # returns importance functions for each variables

save.image("predictFCC.RData")

# unadjusted vs adjusted availability differences (%)
plot(density(test2$difference_availability),xlim=c(-100,100),ylim=c(0,0.08),col=2,
     xlab="Percent Difference between FCC and ACS (R^2=0.87, RMSE=8%)",main="Model Adjusted FCC Availability")
lines(density(predictions2 - test2$difference_availability))

# --------------------------------------------------
# Is the broadband data more reliable on a subset?
load("predictFCC.RData")

# Random Forest prediction error by state, RUCC, demographic
rf.fit <- test2
rf.fit$resid <- predictions2 - test2$difference_availability
rf.fit$absresid <- abs(rf.fit$resid)
View(cor(rf.fit))

state.fit <- rf.fit %>% group_by(state) %>% summarize(prederr = median(abs(resid))) %>% arrange(prederr)
library(maps)
data(state.fips)
state.fit <- state.fit %>% left_join(unique(state.fips %>% dplyr::select(fips,abb)),by=c("state"="fips"))
state.fit$abb[state.fit$state==2] <- "AK"; state.fit$abb[state.fit$state==15] <- "HI"

pdf("~/git/DSPG Broadband/random_forest_residuals.pdf",width=10,height=8)
par(mfrow=c(2,2))
par(mar=c(5,5,0.5,3))
plot( rf.fit %>% group_by(rucc) %>% summarize(median(abs(resid))), xlab="RUCC Code", ylab="Median Prediction Error" ) # 0.17

plot( 1:51, state.fit$prederr, xlab="State", ylab="Median Prediction Error", xaxt='n' ) # more than twice as large in some states
axis(1,at=1:51,labels=state.fit$abb,las=2,cex.axis=0.5)

plot( rf.fit$population, abs(rf.fit$resid), xlab="Population", ylab="Prediction Error",pch=20,cex=0.1,ylim=c(0,20),xlim=c(0,20000)) # -0.10
loessdat <- rf.fit %>% dplyr::select(absresid,population) %>% arrange(population)
lines(loessdat$population, loess(absresid~population,data=loessdat)$fitted,col=2)

plot( rf.fit$hs_or_less, abs(rf.fit$resid), xlab="Highschool or Less", ylab="Prediction Error",pch=20,cex=0.1,ylim=c(0,20)) # 0.18
loessdat <- rf.fit %>% dplyr::select(absresid,hs_or_less) %>% arrange(hs_or_less)
lines(loessdat$hs_or_less, loess(absresid~hs_or_less,data=loessdat)$fitted,col=2)
dev.off()

