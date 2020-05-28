# Read and Combine Multiple CSVs
library(data.table)

# get list of file paths
paths <- list.files(path = "data/MLS/", pattern = "*.csv", full.names = TRUE)

if (exists("final_dt")) rm(final_dt)

for (p in paths) {
  dt <- fread(p)
  
  if(exists("final_dt")) {
    final_dt <- rbindlist(list(final_dt, dt)) 
  } else {
    final_dt <- dt
  }
}
