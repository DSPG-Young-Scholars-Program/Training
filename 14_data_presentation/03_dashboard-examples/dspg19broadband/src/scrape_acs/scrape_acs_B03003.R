# scrape ACS data
library(tidycensus)
library(acs)
library(tigris)
library(purrr)
library(dplyr)
library(data.table)
library(readr)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0")

#B03003 HISPANIC OR LATINO ORIGIN  
my_states <- unique(fips_codes$state)[1:51]
my_counties <- fips_codes %>% filter(state %in% my_states)
my_variables_B03003 <- c("B03003_001", "B03003_002", "B03003_003")

county_state <- tigris::counties(
  state = my_states,
  cb = TRUE,
  resolution = "20m",
  year = "2017",
  class = "sf"
)

hispanic_list <- list()
for(i in 1:3142){
  try(
    hispanic_list[[i]] <- get_acs(
      geography = "county",
      variables = my_variables_B03003 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

# get index of counties for which the API call failed
ind_failed <- which( sapply(hispanic_list,is.null) )
# retry
for(i in ind_failed){
  try(
    hispanic_list[[i]] <- get_acs(
      geography = "county",
      variables = my_variables_B03003 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

hispanic <- rbindlist(hispanic_list)

#write.csv(hispanic,"data/original/acs5yr/acsB03003.csv")
