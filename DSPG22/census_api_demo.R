# Example Census API Request

# packages 
library(tidycensus)
library(dplyr)
library(ggplot2)
library(viridis)

# installed Census API key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# if not using/don't have the environment file
# census_api_key("PASTE_YOUR_KEY_HERE", install=TRUE)

# variables search help
# all variables in ACS 2019
all_vars_acs5 <- 
  load_variables(year = 2019, dataset = "acs5")

# filter variables by concept 
all_vars_acs5 %>% 
  filter(concept == "SEX BY AGE")

################################################################################
# FIND PERCENT OF FEMALE POPULATION AGE 65+ in ALL COUNTIES IN VIRGINIA IN 2015 
################################################################################

# ACS variables
acs_vars <- c(
  # total pop
  "B01003_001",
  # females 65+
  "B01001_044", "B01001_045", 'B01001_046', "B01001_047", "B01001_048", "B01001_049")

# get data for Virginia counties in 2015
acs_data <- get_acs(geography = "county",   # <-------- what geography? 
                      state = 51,           # <-------- in which state?
                      #county = 013,
                      variables = acs_vars, # <-------- what variables?
                      year = 2015,          # <-------- in what year?
                      survey = "acs5",      # <-------- which survey?
                      cache_table = TRUE,   # <-------- cache the selected data for future faster access?
                      output = "wide",      # <-------- variables as columns or rows? 
                      geometry = TRUE,      # <-------- include geography geometry? 
                      keep_geo_vars = FALSE)

# get the proportion of females 65+
females_65plus <- acs_data %>% transmute(
    county = NAME,
    geometry = geometry,
    female65plus = (B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E)
    / B01003_001E * 100)
  
# plot 
ggplot() +  
  geom_sf(data = females_65plus, size = 0.2, aes(fill = female65plus, geometry = geometry)) +
  scale_fill_viridis(name="Female Age 65+, %", na.value = "grey50") +
  xlab("longitude") + ylab("latitude")



