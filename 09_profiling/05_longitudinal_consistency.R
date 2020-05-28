library(data.table)

# Check if data exists
if (exists("final_dt") == FALSE) source("07_profiling/00_read_combine_MOST_USEFUL.R")

# Does any property have multiple values for Year Built?
final_dt[, .(cnt = length(unique(`Year Built`))), .(`Parcel ID`)][order(-cnt)]


