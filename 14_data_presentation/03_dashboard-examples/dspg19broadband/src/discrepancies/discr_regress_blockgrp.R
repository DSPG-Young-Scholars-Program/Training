library(dplyr)
library(stargazer)
library(ggfortify)
library(data.table)
library(here)

#
# Read in data & prepare -------------------------------------------------------------------------------
#

# Block group ACS
acs_file <- here("data", "working", "summary_acs.csv")
acs <- fread(acs_file, colClasses = c(state = "character", county = "character", census_tract = "character", block_group = "character"))

# Block group FCC
fcc_file <- here("data", "working", "fcc_processed_25.csv")
fcc <- fread(fcc_file, colClasses = c(state = "character", county = "character", tract = "character", block_group = "character")) 

# Merge (alternatively, bg and GEOID)
fcc_acs <- merge(fcc, acs, by.x = c("state", "county", "tract", "block_group"), 
                 by.y = c("state", "county", "census_tract", "block_group"))


#
# Create discrepancy measures, format RUCC codes ------------------------------------------------------
#

# RUCC dichotomization
fcc_acs$ru_binary <- ifelse(fcc_acs$RUCC_2013 > 3, "nonmetro", "metro")
fcc_acs$RUCC_2013 <- as.numeric(fcc_acs$RUCC_2013)

# FCC-ACS discrepancy
# fcc_acs$availability_cons = (see app.R, FCC_25mbps_bg.Rmd) This metric utilizes the Maximum advertised downstream speed/bandwidth offered by the provider in the block for Consumer service from FCC form 477 as well as the population data from the Decennial Census to calculate the proportion of the Block Group population that has access to at least 1 provider that offers at least 25 Mbps maximum advertised downstream speed/bandwidth
# fcc_acs$B28002_007_per = (see app.R) ACS Coverage: Broadband (Excluding Cellular/Satellite) (007)

fcc_acs$fccmetric <- round(fcc_acs$availability_cons*100, 0)
fcc_acs$acsmetric <- round(fcc_acs$B28002_007_per, 0)

fcc_acs$dis_rel_fcc_acs <- fcc_acs$fccmetric - fcc_acs$acsmetric
fcc_acs$fcc_over <- ifelse(fcc_acs$fccmetric > fcc_acs$acsmetric, 1, 0)
fcc_acs$fcc_under <- ifelse(fcc_acs$fccmetric < fcc_acs$acsmetric, 1, 0)
fcc_acs$fcc_equal <- ifelse(fcc_acs$fccmetric == fcc_acs$acsmetric, 1, 0)

fcc_acs <- fcc_acs %>% mutate(discrepancy = case_when(fcc_over == 1 & fcc_under == 0 & fcc_equal == 0 ~ "Over",
                                                      fcc_over == 0 & fcc_under == 0 & fcc_equal == 1 ~ "Equal",
                                                      fcc_over == 0 & fcc_under == 1 & fcc_equal == 0 ~ "Under"))
fcc_acs$discrepancy <- as.factor(fcc_acs$discrepancy)
fcc_acs$discrepancy <- ordered(fcc_acs$discrepancy, levels = c("Under", "Equal", "Over"))


#
# Add sociodemographics at block level
#

