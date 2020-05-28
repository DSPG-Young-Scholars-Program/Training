library(data.table)

# Check if data exists
if (exists("final_dt") == FALSE) source("07_profiling/00_read_combine_MOST_USEFUL.R")

# Valid zoning codes
zonig_codes <- c("A-1", "R-1", "R-2", "R-3", "R-4", "R-5", "R-6", "R-7", "R-8", "LB", "B-1", "M-1", "M-2", "RT", "PUD", "MU", "PL", "EO")

# Checking for invalid values in the zoning codes using '%in%'
vv <- final_dt$Zoning %in% zonig_codes

num_valid_codes <- length(vv[vv==TRUE])

pct_valid_codes <- (num_valid_codes/length(vv))*100

print(pct_valid)

# Check for valid lengths using data.table
num_valid_nchar <- final_dt[nchar(`Parcel ID`) == 10, .N]

pct_valid_nchar <- (num_valid_nchar/nrow(final_dt))*100

print(pct_valid_nchar)
