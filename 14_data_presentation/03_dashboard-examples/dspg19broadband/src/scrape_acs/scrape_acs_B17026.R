# scrape ACS data
library(tidycensus)
library(acs)
library(tigris)
library(purrr)
library(dplyr)
library(data.table)
library(readr)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0")

#B17026 RATIO OF INCOME TO POVERTY LEVEL OF FAMILIES IN THE PAST 12 MONTHS
my_states <- unique(fips_codes$state)[1:51]
my_counties <- fips_codes %>% filter(state %in% my_states)
my_variables_B17026 <- c("B17026_001", "B17026_002", "B17026_003", "B17026_004", "B17026_005", "B17026_006", "B17026_007", "B17026_008", "B17026_009", "B17026_010", "B17026_011", "B17026_012", "B17026_013")

county_state <- tigris::counties(
  state = my_states,
  cb = TRUE,
  resolution = "20m",
  year = "2017",
  class = "sf"
)

poverty_list <- list()
for(i in 1:3142){
  try(
    poverty_list[[i]] <- get_acs(
      geography = "county",
      variables = my_variables_B17026 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

# get index of counties for which the API call failed
ind_failed <- which( sapply(poverty_list,is.null) )
# retry
for(i in ind_failed){
  try(
    poverty_list[[i]] <- get_acs(
      geography = "county",
      variables = my_variables_B17026 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

poverty <- rbindlist(poverty_list)

write.csv(poverty,"data/original/acs5yr/acsB17026.csv")
