# scrape ACS data
library(tidycensus)
library(acs)
library(tigris)
library(purrr)
library(dplyr)
library(data.table)
library(readr)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0")

#B02001 RACE 
my_states <- unique(fips_codes$state)[1:51]
my_counties <- fips_codes %>% filter(state %in% my_states)
my_variables_B02001 <- c("B02001_001", "B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007", "B02001_008", "B02001_009", "B02001_010")

county_state <- tigris::counties(
  state = my_states,
  cb = TRUE,
  resolution = "20m",
  year = "2017",
  class = "sf"
)

race_list <- list()
for(i in 1:3142){
  try(
    race_list[[i]] <- get_acs(
      geography = "county",
      variables = my_variables_B02001 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

# get index of counties for which the API call failed
ind_failed <- which( sapply(race_list,is.null) )
# retry
for(i in ind_failed){
  try(
    race_list[[i]] <- get_acs(
      geography = "county",
      variables = my_variables_B02001 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

race <- rbindlist(race_list)

write.csv(race,"data/original/acs5yr/acsB02001.csv")
