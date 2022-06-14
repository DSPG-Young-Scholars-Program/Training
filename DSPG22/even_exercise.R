# even exercise
# Find number of individuals with income to poverty line ratio below 50% in Arlington, VA in 2018 by block group

# packages 
library(tidycensus)
library(dplyr)
library(ggplot2)
library(viridis)

# installed Census API key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# ACS variables
acs_vars <- c(
  # RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS 
  # 0.50
  "C17002_002"
)

# get data
acs_data <- get_acs(geography = "block group",   # <-------- what geography? 
                    state = 51,           # <-------- in which state?
                    county = 013,
                    variables = acs_vars, # <-------- what variables?
                    year = 2018,          # <-------- in what year?
                    survey = "acs5",      # <-------- which survey?
                    cache_table = TRUE,   # <-------- cache the selected data for future faster access?
                    output = "wide",      # <-------- variables as columns or rows? 
                    geometry = TRUE,      # <-------- include geography geometry? 
                    keep_geo_vars = FALSE)

# below 0.5 poverty line
belowFPL_50 <- acs_data %>% transmute(
  tract = NAME,
  geoid = GEOID,
  geometry = geometry,
  belowFPL_50 = C17002_002E)

# check with a plot
ggplot() +  
  geom_sf(data = belowFPL_50, size = 0.2, aes(fill = belowFPL_50, geometry = geometry)) +
  scale_fill_viridis(name="Income to FPL ratio below 50%", na.value = "grey50") +
  xlab("longitude") + ylab("latitude")
