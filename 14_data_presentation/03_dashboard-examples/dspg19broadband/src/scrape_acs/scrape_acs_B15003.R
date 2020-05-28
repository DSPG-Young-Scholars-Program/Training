# scrape ACS data
library(tidycensus)
library(acs)
library(tigris)
library(purrr)
library(dplyr)
library(data.table)
library(readr)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0")

#B15003 EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER 
my_states <- unique(fips_codes$state)[1:51]
my_counties <- fips_codes %>% filter(state %in% my_states)
my_variables_B15003 <- c("B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019", "B15003_020", "B15003_021", "B15003_022", "B15003_023", "B15003_024", "B15003_025")

county_state <- tigris::counties(
  state = my_states,
  cb = TRUE,
  resolution = "20m",
  year = "2017",
  class = "sf"
)

education_list <- list()
for(i in 1:3142){
  try(
    education_list[[i]] <- get_acs(
      geography = "county",
      variables = my_variables_B15003 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

# get index of counties for which the API call failed
ind_failed <- which( sapply(education_list,is.null) )
# retry
for(i in ind_failed){
  try(
    education_list[[i]] <- get_acs(
      geography = "county",
      variables = my_variables_B15003 ,
      state = county_state$STATEFP[i],
      county = county_state$COUNTYFP[i],
      year = 2017,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

education <- rbindlist(education_list)

write.csv(education,"data/original/acs5yr/acsB15003.csv")
x <- read.csv("data/original/acs5yr/acsB15003.csv")
