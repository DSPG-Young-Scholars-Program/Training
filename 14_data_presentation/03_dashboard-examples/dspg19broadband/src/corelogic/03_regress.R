library(RPostgreSQL)
library(DBI)
library(sf)
library(here)
library(data.table)
library(dplyr)
library(stargazer)
library(stringr)
library(ggfortify)


#
# Connect to database ---------------------------------------------------------------------
#

con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                       dbname = "corelogic",
                       host = "localhost",
                       port = "5436",
                       user = Sys.getenv("db_userid"),
                       password = Sys.getenv("db_pwd"))


#
# Get CoreLogic for VA --------------------------------------------------------------------------------
#

# Raw data for whole country 2014
deed14us <- dbGetQuery(con, "SELECT * FROM usda_deed_2014 LIMIT 1")
colnames(deed14us)

# Data for Virginia with tract info and geometry (does not include 43 tracts that had no properties with lat/lon)
deed14va <- st_read(con, query = "SELECT * FROM usda_deed_2014_51_tract LIMIT 1")
colnames(deed14va)

# Median sales per tract in Virginia (includes all 1900 tracts with those with no properties to calculate set to NA)
deed14vasale <- st_read(con, query = "SELECT * FROM usda_deed_2014_51_median_sales_tract LIMIT 1")
colnames(deed14vasale)

clva <- st_read(con, query = "SELECT * FROM usda_deed_2014_51_median_sales_tract")


# Get tract FCC data and ACS data and filter to VA -------------------------------------------------------------------
#

# Get FCC file
fcc_file <- here("data", "working", "fcc_processed_tract_25.csv")
fcc <- fread(fcc_file, colClasses = c(state = "character", county = "character", tract = "character")) 

fccva <- fcc %>% filter(State == "VA")

# Get ACS 2012-14 file
acs_file <- here("data", "working", "acs_2012-16", "acs_2012-16_calc_tract.csv")
acs <- fread(acs_file) 

acsva <- acs %>% filter(str_detect(NAME, ", Virginia")) # Virginia has 1,907 census tracts - correct


#
# Link tract level FCC VA, CL VA, ACS VA ------------------------------------------------------------------------
#

# FCC and CL
head(fccva)
head(clva)

sum(fcc$tract %in% clva$GEOID)
sum(clva$GEOID %in% fcc$tract)

fcc_cl <- merge(fccva, clva, by.x = "tract", by.y = "GEOID")

fcc_cl <- st_as_sf(fcc_cl)
plot(st_geometry(fcc_cl))

#FCC-CL and ACS
head(fcc_cl)
head(acsva)

fcc_cl$tract <- as.numeric(fcc_cl$tract)
acsva$GEOID <- as.numeric(acsva$GEOID)

sum(fcc_cl$tract %in% acsva$GEOID)
sum(acsva$GEOID %in% fcc_cl$tract)

fcc_cl_acs <- merge(fcc_cl, acsva, by.x = "tract", by.y = "GEOID")


#
# Clean up some variables -----------------------------------------------------------------------------
#

# Convert availability proportion to percentages to faciliate interpretation
fcc_cl_acs$availability_adv <- fcc_cl_acs$availability_adv*100

# Dichotomize urban/rural
fcc_cl_acs$RUCC_2013 <- as.numeric(fcc_cl_acs$RUCC_2013)
fcc_cl_acs$ru_binary <- ifelse(fcc_cl_acs$RUCC_2013 > 3, "nonmetro", "metro")

# Convert all else to % to facilitate interpretation
fcc_cl_acs$hs_or_less <- fcc_cl_acs$hs_or_less * 100
fcc_cl_acs$poverty <- fcc_cl_acs$poverty * 100
fcc_cl_acs$age_65_older <- fcc_cl_acs$age_65_older * 100
fcc_cl_acs$hispanic <- fcc_cl_acs$hispanic * 100
fcc_cl_acs$black <- fcc_cl_acs$black * 100
fcc_cl_acs$family <- fcc_cl_acs$family * 100
fcc_cl_acs$foreign <- fcc_cl_acs$foreign * 100


#
# Regress -----------------------------------------------------------------------------
#

# Transform outcome
fcc_cl_acs$log_mediansale <- log(fcc_cl_acs$median_sale_amount)

# Interpretation note: Exponentiate coefficients. Then, a 1 unit increase in the indep var is associated with a % increase in the dep var. 
# Exponentiate the coefficient, subtract one from this number, and multiply by 100. 
# This gives the percent increase (or decrease) in the response for every one-unit increase in the independent variable. 
myfunction <- function(x){
  (exp(x)-1)*100
}

# All tracts
reg_va1all <- lm(log_mediansale ~ ru_binary,
                     data = fcc_cl_acs)
reg_va2all <- lm(log_mediansale ~ availability_adv,
                     data = fcc_cl_acs)
reg_va3all <- lm(log_mediansale ~ ru_binary + availability_adv,
              data = fcc_cl_acs)
reg_va4all <- lm(log_mediansale ~ ru_binary + availability_adv + hs_or_less + poverty + age_65_older + hispanic + black + density + family + foreign,
              data = fcc_cl_acs)

robust_se1all <- as.vector(summary(reg_va1all, robust = T)$coefficients[, "Std. Error"])
robust_se2all <- as.vector(summary(reg_va2all, robust = T)$coefficients[, "Std. Error"])
robust_se3all <- as.vector(summary(reg_va3all, robust = T)$coefficients[, "Std. Error"])
robust_se4all <- as.vector(summary(reg_va4all, robust = T)$coefficients[, "Std. Error"])

stargazer(reg_va1all, reg_va2all, reg_va3all, reg_va4all, no.space = TRUE, digits = 2, type = "text", star.cutoffs = c(0.05, 0.01, 0.001), 
          se = list(robust_se1all, robust_se2all, robust_se3all, robust_se4all))
stargazer(reg_va1all, reg_va2all, reg_va3all, reg_va4all, no.space = TRUE, digits = 2, type = "text", star.cutoffs = c(0.05, 0.01, 0.001), 
          se = list(robust_se1all, robust_se2all, robust_se3all, robust_se4all), apply.coef = myfunction) # for interepretability

# Tracts in metro counties
reg_va1m <- lm(log_mediansale ~ RUCC_2013,
              data = fcc_cl_acs[fcc_cl_acs$ru_binary == "metro", ])
reg_va2m <- lm(log_mediansale ~ availability_adv,
              data = fcc_cl_acs[fcc_cl_acs$ru_binary == "metro", ])
reg_va3m <- lm(log_mediansale ~ RUCC_2013 + availability_adv,
              data = fcc_cl_acs[fcc_cl_acs$ru_binary == "metro", ])
reg_va4m <- lm(log_mediansale ~ RUCC_2013 + availability_adv + hs_or_less + poverty + age_65_older + hispanic + black + density + family + foreign,
              data = fcc_cl_acs[fcc_cl_acs$ru_binary == "metro", ])

robust_se1m <- as.vector(summary(reg_va1m, robust = T)$coefficients[, "Std. Error"])
robust_se2m <- as.vector(summary(reg_va2m, robust = T)$coefficients[, "Std. Error"])
robust_se3m <- as.vector(summary(reg_va3m, robust = T)$coefficients[, "Std. Error"])
robust_se4m <- as.vector(summary(reg_va4m, robust = T)$coefficients[, "Std. Error"])

stargazer(reg_va1m, reg_va2m, reg_va3m, reg_va4m, no.space = TRUE, digits = 2, type = "text", star.cutoffs = c(0.05, 0.01, 0.001), 
          se = list(robust_se1m, robust_se2m, robust_se3m, robust_se4m))
stargazer(reg_va1m, reg_va2m, reg_va3m, reg_va4m, no.space = TRUE, digits = 2, type = "text", star.cutoffs = c(0.05, 0.01, 0.001), 
          se = list(robust_se1m, robust_se2m, robust_se3m, robust_se4m), apply.coef = myfunction) # for interepretability

# Tracts in nonmetro counties
reg_va1n <- lm(log_mediansale ~ RUCC_2013,
              data = fcc_cl_acs[fcc_cl_acs$ru_binary == "nonmetro", ])
reg_va2n <- lm(log_mediansale ~ availability_adv,
              data = fcc_cl_acs[fcc_cl_acs$ru_binary == "nonmetro", ])
reg_va3n <- lm(log_mediansale ~ RUCC_2013 + availability_adv,
              data = fcc_cl_acs[fcc_cl_acs$ru_binary == "nonmetro", ])
reg_va4n <- lm(log_mediansale ~ RUCC_2013 + availability_adv + hs_or_less + poverty + age_65_older + hispanic + black + density + family + foreign,
              data = fcc_cl_acs[fcc_cl_acs$ru_binary == "nonmetro", ])


robust_se1n <- as.vector(summary(reg_va1n, robust = T)$coefficients[, "Std. Error"])
robust_se2n <- as.vector(summary(reg_va2n, robust = T)$coefficients[, "Std. Error"])
robust_se3n <- as.vector(summary(reg_va3n, robust = T)$coefficients[, "Std. Error"])
robust_se4n <- as.vector(summary(reg_va4n, robust = T)$coefficients[, "Std. Error"])

stargazer(reg_va1n, reg_va2n, reg_va3n, reg_va4n, no.space = TRUE, digits = 2, type = "text", star.cutoffs = c(0.05, 0.01, 0.001), 
          se = list(robust_se1n, robust_se2n, robust_se3n, robust_se4n))
stargazer(reg_va1n, reg_va2n, reg_va3n, reg_va4n, no.space = TRUE, digits = 2, type = "text", star.cutoffs = c(0.05, 0.01, 0.001), 
          se = list(robust_se1n, robust_se2n, robust_se3n, robust_se4n), apply.coef = myfunction) # for interepretability


#
# Diagnostics -----------------------------------------------------------------------------
#

# All
autoplot(reg_va4all)
# Residuals vs. fitted: A horizontal line with no pattern indicates a linear reationship. --> Pretty good.
# Normal Q-Q: Residual points following the dashed line indicate normal residual distribution. --> Fail at extremes.
# Scale-Location: Horizontal line with equal point spread indicates homoskedasticity. --> Pretty good.
# Residuals vs leverage: Ok?

# Metro
autoplot(reg_va4m)
# Residuals vs. fitted: A horizontal line with no pattern indicates a linear reationship. --> Pretty good!
# Normal Q-Q: Residual points following the dashed line indicate normal residual distribution. --> Fail at extremes.
# Scale-Location: Horizontal line with equal point spread indicates homoskedasticity. -->  Pretty good.
# Residuals vs leverage: Ok?

# Nonmetro
autoplot(reg_va4n)
# Residuals vs. fitted: A horizontal line with no pattern indicates a linear reationship. --> Pretty good.
# Normal Q-Q: Residual points following the dashed line indicate normal residual distribution. --> Fail at extremes.
# Scale-Location: Horizontal line with equal point spread indicates homoskedasticity. --> Sort of good?
# Residuals vs leverage: Hmm.


#
# Disconnect -----------------------------------------------------------------------------
#

dbDisconnect(con)
