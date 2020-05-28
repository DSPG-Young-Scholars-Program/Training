# Define eligibility regions based on  criteria for Community Connect grants:
# Rural areas that lack any existing broadband speed of 10 Mbps downstream and 1 Mbps upstream
# (Alternate definition based on Rural Broadband Access Loan: Rural areas, at least
# 15% of hosueholds are 'underserved', no more than two 'incumbent service providers')

# Rural areas from 7 CFR 1738
# https://www.ecfr.gov/cgi-bin/text-idx?c=ecfr;sid=b895752c16da8b01975fefc818e95a6e;tpl=/ecfrbrowse/Title07/7cfr1738_main_02.tpl

# Rural area(s) means any area, as confirmed by the latest decennial census of the Bureau of the Census,
# which is not located within:
#  (i) A city, town, or incorporated area that has a population of greater than 20,000 inhabitants; or
# (ii) An urbanized area contiguous and adjacent to a city or town that has a population of
# greater than 50,000 inhabitants. For purposes of the definition of rural area, an urbanized area means a
# densely populated territory as defined in the latest decennial census of the U.S. Census Bureau.

# (i) Census Incorporated Places and Census Designated Places with population > 20,000
# https://www.census.gov/geo/maps-data/data/cbf/cbf_place.html (by state only)
# https://www.ofm.wa.gov/sites/default/files/public/legacy/pop/geographic/tiger10/metadata/city10.html

# (ii) 2010 Urbanized Areas https://www2.census.gov/geo/tiger/TIGER2010/UA/2010/

# Combine these and map urban vs rural areas in VA. Compare with existing eligiblity plot.

library(sf)
library(dplyr)
library(sp)
library(rgdal)
library(tidycensus)

census_api_key("853b2a1e71aa0aff0f3db966545e8898c73f0772")

proj4string=CRS('+proj=longlat +ellps=WGS84')

states <- readOGR(dsn="../data/cb_2017_us_state_20m",layer="cb_2017_us_state_20m")
states <- spTransform(states,proj4string)
state_VA <- states[states@data$NAME=="Virginia",]

urbanized_areas <- readOGR(dsn="../data/tl_2010_us_uac10",layer="tl_2010_us_uac10")
urbanized_areas <- spTransform(urbanized_areas,proj4string)
# filter only Ubranized Areas, exclude Urban Clusters
urbanized_areas <- urbanized_areas[urbanized_areas@data$UATYP10=="U",]

# filter Urbanized Areas in VA
test <- over(urbanized_areas,state_VA)
urbanized_areas_VA <- urbanized_areas[!is.na(test$NAME),]

#plot(state_VA)
#plot(urbanized_areas_VA,add=TRUE,col="red")

places <- readOGR(dsn="../data/gz_2010_51_160_00_500k",layer="gz_2010_51_160_00_500k")
places <- spTransform(places,proj4string)

# get population for all Places in VA and join to shapefile
place_pop <- get_decennial(geography="place",state="VA",variables="P001001",year=2010,cache_table=TRUE)
places_VA <- places@data
places_VA$GEO_ID <- paste(places_VA$GEO_ID)
places_VA$GEOID <- substr(places_VA$GEO_ID,nchar(places_VA$GEO_ID)-6,nchar(places_VA$GEO_ID))
places_VA2 <- places_VA %>% left_join(place_pop,by="GEOID")
# filter places with population > 20,000
places_over20k <- places[places_VA2$value > 20000,]

plot(state_VA)
plot(urbanized_areas_VA,add=TRUE,col="red")
plot(places_over20k,add=TRUE,col="green")

# NOTE: ineligible areas on Rural Development website looks nearly the same as
# map of 2010 urbanized areas + places



