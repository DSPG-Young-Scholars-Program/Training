library(sf)
library(utils)

download.file(url = "https://www2.census.gov/geo/tiger/TIGER2018/BG/tl_2018_19_bg.zip",
              destfile = "data/Census/tiger/tl_2018_19_bg.zip")

unzip(zipfile = "data/Census/tiger/tl_2018_19_bg.zip", 
      exdir = "data/Census/tiger/tl_2018_19_bg")

# Import the file ending in .shp
bg <- sf::st_read("data/Census/tiger/tl_2018_19_bg/tl_2018_19_bg.shp")


plot(bg[, c("AWATER")])  
