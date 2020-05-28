library(rgdal)
library(tidycensus)
library(tigris)
library(acs)
library(rgeos)
library(raster)
library(dplyr)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


#
# Get variable data for geography & transform -------------------------------------------------------------------------------------------------------------
#

# State FIPS
state_fips <- unique(fips_codes$state)[1:51]

# ACS variables
acsvars <- c("B15003_001","B15003_002","B15003_003","B15003_004","B15003_005","B15003_006","B15003_007","B15003_008","B15003_009", # educational attainment for 25+, sum(cat2-cat18)/total
             "B15003_010","B15003_011","B15003_012","B15003_013","B15003_014","B15003_015","B15003_016","B15003_017","B15003_018", # educational attainment for 25+, sum(cat2-cat18)/total
             "B17020_001","B17020_002",                                                                                            # income under poverty level, cat2/total
             "B01001_001","B01001_020","B01001_021","B01001_022","B01001_023","B01001_024","B01001_025",                           # 65+ population, male65+ plus female65+ / total
             "B01001_044","B01001_045","B01001_046","B01001_047","B01001_048","B01001_049",                                        # 65+ population, male65+ plus female65+ / total
             "B03003_001","B03003_003",                                                                                            # hispanic, hispanic/total
             "B02001_001","B02001_003",                                                                                            # black, black/total
             "B09019_002","B09019_003",                                                                                            # family households, in family households/in households
             "B05002_001","B05002_013")                                                                                            # foreign born, foreign/total

# Get tract-level variables from ACS 2012-2016 (5-year)
acs_est <- get_acs(geography = "tract", state = state_fips[1], variables = acsvars, year = 2016, survey = "acs5", cache_table = TRUE, output = "wide")
for(i in 2:length(state_fips)){
  tmp <- get_acs(geography = "tract", state = state_fips[i], variables = acsvars, year = 2016, survey = "acs5", cache_table = TRUE, output = "wide")
  acs_est <- rbind(acs_est, tmp)
}

# Calculate variables: tract area for population density
census_tracts <- tracts(state = state_fips[1], year = 2016)
for(i in 2:length(state_fips)){
  tmp <- tracts(state = state_fips[i], year = 2016)
  census_tracts <- rbind(census_tracts, tmp)
}

tract_area <- data.frame(GEOID = census_tracts@data$GEOID, area_sqmi = area(census_tracts)/2589988)

# Calculate variables: rates / % population
acs_est2 <- acs_est %>% left_join(tract_area, by = "GEOID")
acs_estimates <- acs_est2 %>% transmute(
  GEOID = GEOID,
  NAME = NAME,
  population = B01001_001E,
  hs_or_less = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E + B15003_010E +
                  B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E) / B15003_001E,
  poverty = B17020_002E / B17020_001E,
  age_65_older = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
                    B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  density = B01001_001E / area_sqmi,
  family = B09019_003E / B09019_002E,
  foreign = B05002_013E / B05002_001E
)


#
# Write out ---------------------------------------------------------------------------------------------------------------
#

write.csv(acs_est2, file = "./data/working/acs_2012-16/acs_2012-16_raw_tract.csv", row.names = F)
write.csv(acs_estimates, file = "./data/working/acs_2012-16/acs_2012-16_calc_tract.csv", row.names = F)
