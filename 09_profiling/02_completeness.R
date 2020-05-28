library(data.table)

# Check if data exists
if (exists("final_dt") == FALSE) source("07_profiling/00_read_combine_MOST_USEFUL.R")

# a function for detecting all forms of 'blank'
is_blank <- function(x) {
  return(is.null(x) |
           length(x) == 0 |
           is.na(x) |
           x == "")
}

# Number of cell values missing per row
rowSums(is_blank(final_dt))

# Number of cell values missing per column
colSums(is_blank(final_dt))

# Create better visualization
df <- as.data.frame(colSums(is_blank(final_dt)))
dt <- setDT(df, keep.rownames=TRUE)
colnames(dt) <- c("item", "empties")
dt <- dt[order(-empties)]
View(dt)