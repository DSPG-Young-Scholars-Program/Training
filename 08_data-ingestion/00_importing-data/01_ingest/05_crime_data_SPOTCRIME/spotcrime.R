get_spotcrime_urls <- function(url, pattern, prefix = "") {
  print(url)
  if (prefix == "") prefix = url
  . <- xml2::read_html(url)
  . <- rvest::html_nodes(., 'a')
  . <- rvest::html_attr(., 'href')
  . <- stringr::str_match(., pattern)[,2]
  . <- .[!is.na(.)]
  . <- paste0(prefix, .)
}


# Function to Parse a Webpage for Information Items ----------------------------------------------

get_spotcrime_details <- function(url) {
  print(url)
  browser()
  out <- tryCatch(
    {
      crime_details <- xml2::read_html(url)
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      if (exists("crime_details")) {
        crime_type <- rvest::html_text(rvest::html_nodes(crime_details, 'dd'))[1]
        crime_date <- rvest::html_text(rvest::html_nodes(crime_details, 'dd'))[2]
        crime_data_source <- rvest::html_attr(rvest::html_nodes(rvest::html_nodes(crime_details, 'dd'), 'a'), 'href')
        crime_description <- rvest::html_text(rvest::html_nodes(crime_details, xpath = '//*[@itemprop="description"]'))
        crime_address <- rvest::html_text(rvest::html_nodes(crime_details, xpath = '//*[@itemprop="street-address"]'))
        crime_latitude <- rvest::html_attr(rvest::html_nodes(crime_details, xpath = '//*[@itemprop="latitude"]'), 'content')
        crime_longitude <- rvest::html_attr(rvest::html_nodes(crime_details, xpath = '//*[@itemprop="longitude"]'), 'content')
        
        crime_details_dt <- data.table::data.table(crime_description,
                                                   crime_type,
                                                   crime_date,
                                                   crime_address,
                                                   crime_data_source,
                                                   crime_latitude,
                                                   crime_longitude)
      } else {
        crime_details_dt <- data.table::data.table(crime_description=character(),
                                                   crime_type=character(),
                                                   crime_date=character(),
                                                   crime_address=character(),
                                                   crime_data_source=character(),
                                                   crime_latitude=character(),
                                                   crime_longitude=character())
      }
      message(paste("Processed URL:", url))
      crime_details_dt
    }
  )
  return(crime_details_dt)
}


# Function to get Census Block FIPS from lat long ----------------------------------------------
get_block_FIPS <- function(latitude = 42.0503003521937, longitude = -92.913092988386) {
  #browser()
  library(xml2)
  url <- paste0("https://geo.fcc.gov/api/census/block/find?latitude=", latitude, "&longitude=", longitude, "&format=xml")
  FIPS <- read_xml(url) %>% xml_find_all("//Block") %>% xml_attr("FIPS")
  return(FIPS)
}

# Get Daily Crime Report Page URLS for Virginia ----------------------------------------------

. <- get_spotcrime_urls(url = "https://spotcrime.com/ia/",
                        pattern = "/ia/(.*daily$)")
.
# limit urls to Arlington for testing
. <- stringr::str_match(., "(.*marshall.*)")[,2]
. <- .[!is.na(.)]

saveRDS(., "__Personal__/2019MBDSS/data/spotcrime/daily_crime_report_iowa_urls_arl.RDS")



# Get Daily Crime Report URLS for Each Locality in Iowa  ----------------------------------------------

. <- readRDS("__Personal__/2019MBDSS/data/spotcrime/daily_crime_report_iowa_urls_arl.RDS")

. <-
  lapply(
    .,
    get_spotcrime_urls,
    pattern = "/ia./(.*-[0-9][0-9]$)",
    prefix = "https://spotcrime.com/ia+/"
  )
. <- unlist(.)
. <- .[. != "https://spotcrime.com/ia+/"]

saveRDS(., "__Personal__/2019MBDSS/data/spotcrime/daily_crime_report_locality_urls_arl.RDS")



# Get Daily Crime Report Details URL for Each Crime in Each Locality in Virginia  ----------------------------------------------

. <- readRDS("__Personal__/2019MBDSS/data/spotcrime/daily_crime_report_locality_urls_arl.RDS")

. <-
  lapply(
    .[1:20],
    get_spotcrime_urls,
    pattern = "/(mobile/crime/.*)",
    prefix = "https://spotcrime.com/"
  )
. <- unlist(.)
. <- .[. != "https://spotcrime.com/"]

saveRDS(., "__Personal__/2019MBDSS/data/spotcrime/daily_crime_report_details_urls_arl.RDS")



# Get Daily Crime Report Details for Each Crime in Virginia  ----------------------------------------------

. <- readRDS("__Personal__/2019MBDSS/data/spotcrime/daily_crime_report_details_urls_arl.RDS")
#. <- .[1:20]

cl <- parallel::makeCluster(15, outfile = "")
. <- parallel::parLapply(cl, ., get_spotcrime_details)
parallel::stopCluster(cl)

daily_crime_report_details_arl <- data.table::rbindlist(.)

saveRDS(daily_crime_report_details_arl, "__Personal__/2019MBDSS/data/spotcrime/daily_crime_report_details_arl.RDS")


rds <- data.table::rbindlist(list(
  readRDS("__Personal__/2019MBDSS/data/spotcrime/daily_crime_report_details_arl_061217_060918.RDS"),
  readRDS("__Personal__/2019MBDSS/data/spotcrime/daily_crime_report_details_arl_010608_061117.RDS")
))
data.table::setnames(rds, "crime_date", "crime_date_time")

rds[nchar(crime_date_time) > 10, crime_date := lubridate::as_date(lubridate::mdy_hm(crime_date_time))]
rds[nchar(crime_date_time) > 10, crime_hour := lubridate::hour(lubridate::as_datetime(lubridate::mdy_hm(crime_date_time)))]
rds[nchar(crime_date_time) == 10, crime_date := lubridate::as_date(lubridate::mdy(crime_date_time))]
rds[nchar(crime_date_time) == 10, crime_hour := NA]
rds[, crime_year := lubridate::year(crime_date)]

rds[, longitude := as.numeric(crime_longitude)]
rds[, latitude := as.numeric(crime_latitude)]

rds_sf <- sf::st_as_sf(rds, coords = c("longitude", "latitude"))
sf::st_crs(rds_sf) <- 4326

sf::st_write(rds_sf, sdalr::con_db("sdal"), c("behavior", "va_pl_spotcrime_08_18"), overwrite = TRUE)


# Add Census Block FIPS for Each Crime ----------------------------------------------
lapply(lap_dt$crime_latitude, get_block_FIPS, longitude = lap_dt$crime_longitude)

lap_dt[, seq := seq(1:nrow(lap_dt))]

lap_dt[, geoid := get_block_FIPS(crime_latitude, crime_longitude), seq]

lap_dt[, .N, geoid]