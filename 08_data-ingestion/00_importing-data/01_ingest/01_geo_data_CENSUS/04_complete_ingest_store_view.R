

# DOWNLOAD FILE
download.file(url = "https://www2.census.gov/geo/tiger/TIGER2018/BG/tl_2018_19_bg.zip",
              destfile = "__Personal__/2019MBDSS/data/Census/tiger/tl_2018_19_bg.zip")

# UNZIP FILE
unzip(zipfile = "__Personal__/2019MBDSS/data/Census/tiger/tl_2018_19_bg.zip", 
      exdir = "__Personal__/2019MBDSS/data/Census/tiger/tl_2018_19_bg")

# READ FILE
bg <- sf::st_read("__Personal__/2019MBDSS/data/Census/tiger/tl_2018_19_bg/tl_2018_19_bg.shp")

# OPEN DATABASE CONNECTION
con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "user1",
                      host = "postgis",
                      port = "5432",
                      user = "user1",
                      password = "user1")

# WRITE TO DATABASE
sf::st_write(bg, con, "tl_2018_19_bg")

# READ FROM DATABASE
sql <- "SELECT * FROM tl_2018_19_bg WHERE \"COUNTYFP\" = '127'"
dat <- sf::st_read(con, query = sql)

# CLOSE DATABASE CONNECTION
DBI::dbDisconnect(con)

# PLOT DATA
plot(dat[,c("ALAND")])

# PLOT DATA FANCY
library(ggplot2)
ggplot(data = dat) +
  geom_sf(aes(fill = ALAND)) +
  scale_fill_viridis_c() +
  ggtitle("Land Area", subtitle = "Marshalltown IA")
