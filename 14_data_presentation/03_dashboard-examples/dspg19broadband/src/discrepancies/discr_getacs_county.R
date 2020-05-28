library(rgdal)
library(tidycensus)
library(tigris)
library(acs)
library(rgeos)
library(raster)
library(dplyr)
library(sf)


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
             "B05002_001","B05002_013",                                                                                            # foreign born, foreign/total
             "B08006_001","B08006_017",                                                                                            # works from home, workers from home / total -- 017 / 001
             "B08303_001","B08303_008","B08303_009","B08303_010","B08303_011","B08303_012","B08303_013",                           # travel time to work, 30min+ / total (008 to 013 / 001)
             "B19058_001","B19058_002",                                                                                            # households with public assistance or food stamps/snap, with support / total (002 / 001)
             "B23025_001","B23025_002",                                                                                            # proportion in labor force / total (002 / 001)
             "B25002_001","B25002_003",                                                                                            # proportion vacant housing units, vacant / total -- 003 / 001
             "B25003_001","B25003_003",                                                                                            # proportion renters, renters  /total -- 003 / 001
             "B25035_001",                                                                                                         # median year structure built
             "B25071_001",                                                                                                         # median gross rent as percentage of HH income
             "B28011_001", "B28011_008")                                                                                           # no internet access, no access / total (008 / 001)
             
             
# Get county-level variables from ACS 2013-2017 (5-year)
acs_est <- get_acs(geography = "county", state = state_fips[1], variables = acsvars, year = 2017, survey = "acs5", cache_table = TRUE, output = "wide", geometry = TRUE,
                   keep_geo_vars = TRUE)
for(i in 2:length(state_fips)){
  tmp <- get_acs(geography = "county", state = state_fips[i], variables = acsvars, year = 2017, survey = "acs5", cache_table = TRUE, output = "wide", geometry = TRUE,
                 keep_geo_vars = TRUE)
  acs_est <- rbind(acs_est, tmp)
}

# Calculate variables: county area for population density
census_counties <- counties(state = state_fips[1], year = 2017)
for(i in 2:length(state_fips)){
  tmp <- counties(state = state_fips[i], year = 2017)
  census_counties <- rbind(census_counties, tmp)
}

county_area <- data.frame(GEOID = census_counties@data$GEOID, area_sqmi = area(census_counties)/2589988)  # convert sqm to square miles: divide by 2,589,988
county_area$GEOID <- as.character(county_area$GEOID)

# Calculate variables: rates / % population
acs_est2 <- acs_est %>% left_join(county_area, by = "GEOID")
acs_estimates <- acs_est2 %>% transmute(
  GEOID = GEOID,
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  AFFGEOID = AFFGEOID,
  ALAND = ALAND,
  AWATER = AWATER,
  LSAD = LSAD,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  area_sqmi = area_sqmi,
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
  foreign = B05002_013E / B05002_001E,
  workfromhome = B08006_017E / B08006_001E,     
  longcommute = (B08303_008E + B08303_009E + B08303_010E + B08303_011E + B08303_012E + B08303_013E) / B08303_001E,
  assistance = B19058_002E / B19058_001E,
  laborforce = B23025_002E / B23025_001E,  
  vacant = B25002_003E / B25002_001E,
  renters = B25003_003E / B25003_001E,  
  yearbuilt = B25035_001E,
  rentburden = B25071_001E,
  nointernet = B28011_008E / B28011_001E,
  geometry = geometry
)



#
# Write out ---------------------------------------------------------------------------------------------------------------
#

# With geography
st_write(acs_estimates, "./data/working/acs_2013-17/calc/acs_2013-17_calc_count.shp", driver = "ESRI Shapefile")  # create to a shapefile 

# Data only
st_geometry(acs_estimates) <- NULL

write.csv(acs_est2, file = "./data/working/acs_2013-17/nogeo/acs_2013-17_raw_county.csv", row.names = F)
write.csv(acs_estimates, file = "./data/working/acs_2013-17/nogeo/acs_2013-17_calc_county.csv", row.names = F)
