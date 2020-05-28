# Get Census Block Geographies
con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "user1",
                      host = "postgis",
                      port = "5432",
                      user = "user1",
                      password = "user1")

# get census block group geographies
sql <- "SELECT distinct \"GEOID\" geoid, geometry
        FROM tl_2018_19_bg
        WHERE left(\"GEOID\", 5) = '19127'"
bg_geos <- sf::st_read(con, query = sql)
DBI::dbDisconnect(con)