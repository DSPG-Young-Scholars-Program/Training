# scrape ACS data
library(tidycensus)
library(acs)
library(tigris)
library(purrr)
library(dplyr)
library(data.table)
library(readr)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0")

#B23025 EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER  
my_states <- unique(fips_codes$state)[1:51]
my_counties <- fips_codes %>% filter(state %in% my_states)
my_variables_B23025 <- c("B23025_001", "B23025_002", "B23025_003", "B23025_004", "B23025_005", "B23025_006", "B23025_007")

county_state <- tigris::counties(
  state = my_states,
  cb = TRUE,
  resolution = "20m",
  year = "2017",
  class = "sf"
)

employment_list <- list()
for(i in 1:3142){
  try(
    employment_list[[i]] <- get_acs(
      geography = "county",
      variables = my_variables_B23025 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

# get index of counties for which the API call failed
ind_failed <- which( sapply(employment_list,is.null) )
# retry
for(i in ind_failed){
  try(
    employment_list[[i]] <- get_acs(
      geography = "county",
      variables = my_variables_B23025 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

employment <- rbindlist(employment_list)
write.csv(employment,"data/original/acs5yr/acsB23025.csv")