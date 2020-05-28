library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(xml2)
library(purrr)
library(DescTools)

# Daniel helped me with the "assign" part. Thank you Daniel :)
# File will write out to /src/us_cities.csv (assuming Rproj).


#
#------------------------------------ Get list of state names
#

# Get list of all states
states_url <- read_html("https://simple.wikipedia.org/wiki/List_of_U.S._states")
states <- states_url %>% 
  html_table(fill = TRUE)
states <- states[[1]] 

# Get state names only
state_names <- states$`State Name`

# Fix blank space to - for URLs
state_names <- str_replace_all(state_names, " ", "-")

# Make state URLs
state_list <- str_c("https://broadbandnow.com/", state_names)


#
#------------------------------------ Get broadband information at city level by state
#

# For each state: read the URL, then
# 1) select only the third table (city table), add state variable and remove empty column, and assign to a dataframe
# 2) pull sparkline data, get it out of a list, drop the class column, and reverse the order of sparkline columns to match the online order
# 3) cbind and save to df

for (val_i in 1:length(state_list)) {
  cities <- read_html(state_list[val_i])
  
  scrape1 <- cities %>% 
    html_table(fill = TRUE) %>%
    .[[3]] %>%
    mutate(state = str_to_lower(state_names[val_i])) %>%
    select(-`Average speed over time`)
  
  scrape2 <- cities %>% 
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "speed-inlinesparkline", " " ))]') %>% 
    map(xml_attrs) %>% 
    map_df(~as.list(.)) %>%
    select(-class) %>%
    Rev(margin = 2)
  
  df <- cbind(scrape1, scrape2)
  assign(paste("frame", str_to_lower(state_names[val_i]), sep = "_"), df)
}


#
#------------------------------------ Merge and clean
#

# Make a list of dataframes and bind
dflist <- lapply(ls(pattern = "frame_"), function(x) if (class(get(x)) == "data.frame") get(x))
us_cities <- do.call("rbind", dflist)

# Rename columns
datavars <- paste("data-m", 12:1, sep = "")
speedvars <- paste("speed", 12:1, sep = "")

oldnames = c("City", "Broadband Coverage", "# of Providers", datavars)
newnames = c("city", "coverage","providernum", speedvars)

us_cities <- us_cities %>% rename_at(vars(oldnames), ~ newnames)

# Clean values (lowercase, unnecessary characters, fill spaces)
us_cities$coverage <- str_replace_all(us_cities$coverage, "%", "")
us_cities$providernum <- str_replace_all(us_cities$providernum, " providers", "")
us_cities$city <- str_replace_all(us_cities$city, " ", "-")
us_cities$city <- str_to_lower(us_cities$city)

# Convert from character to factor/numeric
tonumeric <- c("coverage", "providernum", speedvars)
us_cities[, tonumeric] <- sapply(us_cities[, tonumeric], as.numeric)

us_cities$state <- as.factor(us_cities$state)


#
#------------------------------------ Write out 
#

write_csv(us_cities, path = "./src/us_cities.csv", append = FALSE, col_names = TRUE)


#
#------------------------------------ Clean up workspace
#

remove(list = ls())
