# scrape ACS data
library(tidycensus)
library(acs)
library(tigris)
library(purrr)
library(dplyr)
library(data.table)
library(readr)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0")

#PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD
my_states <- unique(fips_codes$state)[1:51]
my_counties <- fips_codes %>% filter(state %in% my_states)
my_variables_B28002 <- c("B28002_001", "B28002_002", "B28002_003", "B28002_004",
                         "B28002_005", "B28002_006", "B28002_007", "B28002_008", "B28002_009",
                         "B28002_010", "B28002_011", "B28002_012", "B28002_013")

county_state <- tigris::counties(
  state = my_states,
  cb = TRUE,
  resolution = "20m",
  year = "2017",
  class = "sf"
)

internet_sub_bbdata_list <- list()
for(i in 1:3142){
  try(
    internet_sub_bbdata_list[[i]] <- get_acs(
      geography = "block group",
      variables = my_variables_B28002 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

# get index of counties for which the API call failed
ind_failed <- which( sapply(internet_sub_bbdata_list,is.null) )
# retry
for(i in ind_failed){
  try(
    internet_sub_bbdata_list[[i]] <- get_acs(
      geography = "block group",
      variables = my_variables_B28002 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

internet_sub_bbdata <- rbindlist(internet_sub_bbdata_list)

# write_csv(internet_sub_bbdata,"~/../sdad/project_data/usda/dspg2019broadband/working/acs5yr/acsB28002.csv")

