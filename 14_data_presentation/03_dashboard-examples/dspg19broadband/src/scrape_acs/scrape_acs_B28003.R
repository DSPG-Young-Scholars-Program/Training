#scrape ACS data
library(tidycensus)
library(acs)
library(tigris)
library(purrr)
library(dplyr)
library(data.table)
library(readr)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0")

#PRESENCE COMPUTER AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD
my_states <- unique(fips_codes$state)[1:51]
my_counties <- fips_codes %>% filter(state %in% my_states)
my_variables_B28003 <- c("B28003_001", "B28003_002", "B28003_003", "B28003_004",
                         "B28003_005", "B28003_006")

county_state <- tigris::counties(
  state = my_states,
  cb = TRUE,
  resolution = "20m",
  year = "2017",
  class = "sf"
)

computer_internet_sub_bbdata_list <- list()
for(i in 1:3142){
  try(
    computer_internet_sub_bbdata_list[[i]] <- get_acs(
      geography = "block group",
      variables = my_variables_B28003 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

# get index of counties for which the API call failed
ind_failed <- which( sapply(computer_internet_sub_bbdata_list,is.null) )
# retry
for(i in ind_failed){
  try(
    computer_internet_sub_bbdata_list[[i]] <- get_acs(
      geography = "block group",
      variables = my_variables_B28003 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

computer_internet_sub_bbdata <- rbindlist(computer_internet_sub_bbdata_list)

write_csv(computer_internet_sub_bbdata,"~/dspg19broadband/data/working/acs5yr/acsB28003.csv")
