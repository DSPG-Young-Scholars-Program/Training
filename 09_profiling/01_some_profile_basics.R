library(dplyr)
if(!require(inspectdf)){
  install.packages("inspectdf")
  library(inspectdf)
}

# Check if data exists
if (exists("final_dt") == FALSE) source("07_profiling/00_read_combine_MOST_USEFUL.R")

# Five Number Numeric Summary
summary(final_dt)

# Structure of the data set
str(final_dt)

# Inspect categorical variables
cat_chart <- 
  final_dt %>% 
  select(c(`Selling Agency`, `Property Type`, `Zoning`)) %>% 
  inspect_cat() %>% 
  show_plot(high_cardinality = 1)
