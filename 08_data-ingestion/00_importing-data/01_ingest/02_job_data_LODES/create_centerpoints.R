library(sf)
library(DBI)

con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                 dbname = "user1",
                 host = "postgis",
                 port = "5432",
                 user = "user1",
                 password = "user1")
bg <- st_read(con, query="select \"GEOID\" geoid, \"INTPTLAT\" lat, \"INTPTLON\" lon, geometry from tl_2018_19_bg")

bg_crs <- st_crs(bg)
bg$geometry <- NULL
bg_centerpoints <- st_as_sf(bg, coords = c("lon", "lat"), crs = bg_crs)
st_write(bg_centerpoints, con, "tl_2018_19_bg_centerpoints", overwrite = TRUE)
dbDisconnect(con)

con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                 dbname = "user1",
                 host = "postgis",
                 port = "5432",
                 user = "user1",
                 password = "user1")
blk <- st_read(con, query="select \"GEOID10\" geoid, \"INTPTLAT10\" lat, \"INTPTLON10\" lon, geometry from tl_2018_19_tabblock10")
blk_crs <- st_crs(blk)
blk$geometry <- NULL
blk_centerpoints <- st_as_sf(blk, coords = c("lon", "lat"), crs = blk_crs)
st_write(blk_centerpoints, con, "tl_2018_19_block_centerpoints")
dbDisconnect(con)
