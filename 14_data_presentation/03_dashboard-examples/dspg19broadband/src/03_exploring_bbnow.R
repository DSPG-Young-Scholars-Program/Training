library(dplyr)
library(stringr)
library(readr)
library(xml2)
library(purrr)
library(data.table)
library(ggplot2)
library(ggmap)

bbnow <- read.csv("./data/working/bbnow_cities.csv")
uscity <- read.csv("./data/working/uscities.csv")
uscities <- uscity[c(1,3:10)]
#head(bbnow)
#head(uscities)
#city_n <- uscities$city
uscities$city <- tolower(uscities$city)
uscities$city <- str_replace_all(uscities$city, "\\[upper-alpha 3\\]", "")
uscities$city <- str_replace_all(uscities$city, "\\[upper-alpha 4\\]", "")

uscities$city <- str_replace_all(uscities$city, " ", "-")

uscities$statename <- tolower(uscities$statename)
uscities$statename <- str_replace_all(uscities$statename, "\\[upper-alpha 3\\]", "")
uscities$statename <- str_replace_all(uscities$statename, "\\[upper-alpha 4\\]", "")

uscities$statename <- str_replace_all(uscities$statename, " ", "-")
head(uscities)

#uscities %>% data.table() %>% dt_mutate(city_names = tolower(city)) %>% dt_mutate(city_names =str_replace_all(city_names, "\\[upper-alpha 3\\]", ""))
#bbnow %>%
#  rename(statename = state)
#head(city_names)
#head(uscities)
df1 = data.frame(uscities)
df2 = data.frame(bbnow)
head(bbnow)

df3 <- merge(df1,df2,by.x = c("city","statename"),by.y = c("city","state"))
head(df3)

virginia_data <- df3[which(df3$statename == 'virginia')]
  
write_csv(df3, path = "./data/working/merged_bbnow.csv", append = FALSE, col_names = TRUE)
usa <- map_data("usa") # we already did this, but we can do it again
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat)) + 
  geom_point(data=df3, aes(x=df3$long, y=df3$lat), color="red")




va <- read.csv("./data/working/virginia_dat.csv")
library(tigris)
library(sf)
library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

orwa <- tigris(
  tracts("VA", cb = TRUE) 
)

ggplot(orwa) + geom_sf()

us <- states()
plot(us)

fw_zips <- zctas(cb = TRUE, starts_with = "51")
plot(fw_zips)

ri <- states()
View(ri$STATEFP)
va <- ri[ri$STATEFP == 51,]
plot(va)
