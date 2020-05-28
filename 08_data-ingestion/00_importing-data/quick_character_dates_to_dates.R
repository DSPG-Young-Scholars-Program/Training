my_dates <- sample(seq(as.Date('1900/01/01'), as.Date('2000/01/01'), by="day"), 10000000, replace = TRUE)
my_dates_char <- as.character(my_dates)

quick_date <- function(x, ...) {
  if (anyDuplicated(x)) {
    ux <- unique(x)
    idx <- match(x, ux)
    y <- as.Date.character(ux, ...)
    return(y[idx])
  }
  as.Date.character(x, ...)
}

my_dates_date <- quick_date(my_dates_char)

my_dates_date <- as.Date(my_dates_char)
