#READ IN PACKAGES
library(httr)
library(here)
library(maptools)
library(gpclib)
library(sp)
library(sf)
library(ggplot2)
library(ggmap)
library(osmdata)
library(tidyverse)
library(tigris)
library(acs)
library(data.table)
library(reshape2)
library(tidyr)
library(ggplot2)
library(viridis)

destfile <- here("data", "original", "acs5yr", "acsB28002.csv")
sp <- fread(destfile)

sp2 <- sp[,-"NAME"]
sp2 <-sp2[,-"moe"]

sp2 <- reshape(sp2, idvar = "GEOID", timevar = "variable", direction = "wide")

#GEOID needs to be factor to merge with con
sp2$GEOID=as.factor(sp2$GEOID)
str(sp2)

percent <- function(x){
  (x/sp2$estimate.B28002_001) *100
}


sp2$estimate.B28002_002 = percent(sp2$estimate.B28002_002)
sp2$estimate.B28002_003 = percent(sp2$estimate.B28002_003)
sp2$estimate.B28002_004 = percent(sp2$estimate.B28002_004)
sp2$estimate.B28002_005 = percent(sp2$estimate.B28002_005)
sp2$estimate.B28002_006 = percent(sp2$estimate.B28002_006)
sp2$estimate.B28002_007 = percent(sp2$estimate.B28002_007)
sp2$estimate.B28002_008 = percent(sp2$estimate.B28002_008)
sp2$estimate.B28002_009 = percent(sp2$estimate.B28002_009)
sp2$estimate.B28002_010 = percent(sp2$estimate.B28002_010)
sp2$estimate.B28002_011 = percent(sp2$estimate.B28002_011)
sp2$estimate.B28002_012 = percent(sp2$estimate.B28002_012)
sp2$estimate.B28002_013 = percent(sp2$estimate.B28002_013)

con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "gis",
                      host = "postgis_1",
                      port = "5432",
                      user = "sm9dv",
                      password = "sm9dv")
mygeo <- sf::st_read(con, c("census_cb", "cb_2018_51_bg_500k"))
DBI::dbDisconnect(con)

acs_va_w_geo <- merge(sp2, mygeo, by = "GEOID")         # merge data sets
acs_va_w_geo_sf <- st_as_sf(acs_va_w_geo)                  # convert to sf

plot(acs_va_w_geo_sf[, "estimate.B28002_004"], main = "Broadband of Any Type")  
plot(acs_va_w_geo_sf[, "estimate.B28002_013"], main = "No Internet Access")


ggplot(acs_va_w_geo_sf) +
  geom_sf(aes(fill = estimate.B28002_004), size = 0.05) + 
  ggtitle('Percent of Households in Block Group with Broadband of Any Type') + 
  labs(fill = 'Percentage of Households in Block Group') +
  scale_fill_viridis(option = 'magma') + 
  theme_minimal() + theme(legend.position = 'bottom')

ggplot(acs_va_w_geo_sf) +
  geom_sf(aes(fill = estimate.B28002_013), size = 0.05) + 
  ggtitle('Percent of Households in Block Group with No Internet Access') + 
  labs(fill = 'Percentage of Households in Block Group') +
  scale_fill_viridis(option = 'magma') + 
  theme_minimal() + theme(legend.position = 'bottom')
