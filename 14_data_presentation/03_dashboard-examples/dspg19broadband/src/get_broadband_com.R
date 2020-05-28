va_zips <- data.table::fread("src/va_zips.csv")

get_broadband_by_zip <- function(zip = 0) {
  if (zip == 0) {
    print("zip not provided")
    return()
  }
  else z <- zip
  c <- va_zips[zip==z, city]
  if (length(c) == 0) { 
    print("not a valid zip") 
    return()
  }
  url <- sprintf("https://broadbandnow.com/Virginia/%s?zip=%s", c, z)
  
  . <- ""
  tryCatch(
  . <- xml2::read_html(url)
  , error=function(e){})
  
  if (. == "") return() 
  else {
    . <- rvest::html_nodes(., 'table')
    . <- rvest::html_table(.)
    df <- .[[1]]
    df$zipcode <- as.numeric(z)
    dt <- data.table::setDT(df)
    dt  
  }
}

get_broadband_by_zips <- function(zips = c(0)) {
  if (zips[1] == 0) return("vector of zips not provided")
  lapply(zips, get_broadband_by_zip)
}


l <- get_broadband_by_zips(c(24150, 24136, 24128, 24060, 24127, 24070, 27087, 24153))
ldt <- data.table::rbindlist(l)


