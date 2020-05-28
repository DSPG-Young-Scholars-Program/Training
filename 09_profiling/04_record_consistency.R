# THIS IS REALLY DANG SLOW

library(zipcode)
library(data.table)

# Check if data exists
if (exists("final_dt") == FALSE) source("07_profiling/00_read_combine_MOST_USEFUL.R")

# Load zipcode data
data("zipcode")

zip2state <- function(zipcd, zipcds = zipcode) {
  dt <- data.table::setDT(zipcds)
  unique(dt[zip == zipcd, state])
  #state <- zipcds %>% filter(zip == zipcd) %>% select("state") %>% unique()
  #state[1]
}

zip2state(22207, zipcode)


newv <- logical(nrow(final_dt))

for (i in 1:nrow(final_dt)) {
  if (length(final_dt[i, zip2state(`Zip Code`)]) != 0 & length(final_dt[i, `State`]) !=0) {
    if (final_dt[i, zip2state(`Zip Code`)] == final_dt[i, `State`]) {
      newv[i] <- TRUE
    } else {
      newv[1] <- FALSE
    }
  }
}

final_dt$zip_state_mtch <- newv

final_dt[zip_state_mtch == FALSE, .(`Zip Code`, `State`)]


