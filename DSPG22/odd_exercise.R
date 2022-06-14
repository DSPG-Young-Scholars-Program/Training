# odd exercise
# Find how many single women with kids were receiving SNAP benefits in Fairfax, VA in 2019 by census tract 

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
  # female householder, no spouse present, with children under 18 years 
  # received Food Stamps/SNAP in the past 12 months
  "B22002_007"
)

# get data
acs_data <- get_acs(geography = "tract",   # <-------- what geography? 
                    state = 51,           # <-------- in which state?
                    county = 059,
                    variables = acs_vars, # <-------- what variables?
                    year = 2019,          # <-------- in what year?
                    survey = "acs5",      # <-------- which survey?
                    cache_table = TRUE,   # <-------- cache the selected data for future faster access?
                    output = "wide",      # <-------- variables as columns or rows? 
                    geometry = TRUE,      # <-------- include geography geometry? 
                    keep_geo_vars = FALSE)

# number of single females with kids receiving SNAP
single_SNAP <- acs_data %>% transmute(
  tract = NAME,
  geoid = GEOID,
  geometry = geometry,
  single_women_kids_SNAP = B22002_007E)

# check with a plot
ggplot() +  
  geom_sf(data = single_SNAP, size = 0.2, aes(fill = single_women_kids_SNAP, geometry = geometry)) +
  scale_fill_viridis(name="Single female with kids receiving SNAP", na.value = "grey50") +
  xlab("longitude") + ylab("latitude")



