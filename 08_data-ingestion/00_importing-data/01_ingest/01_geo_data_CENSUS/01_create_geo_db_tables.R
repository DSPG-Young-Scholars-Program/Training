download.file(url = "https://www2.census.gov/geo/tiger/TIGER2018/BG/tl_2018_19_bg.zip",
              destfile = "__Personal__/2019MBDSS/data/Census/tiger/tl_2018_19_bg.zip")

unzip(zipfile = "__Personal__/2019MBDSS/data/Census/tiger/tl_2018_19_bg.zip", 
      exdir = "__Personal__/2019MBDSS/data/Census/tiger/tl_2018_19_bg")

bg <- sf::st_read("__Personal__/2019MBDSS/data/Census/tiger/tl_2018_19_bg/tl_2018_19_bg.shp")

con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "user1",
                      host = "postgis",
                      port = "5432",
                      user = "user1",
                      password = "user1")

sf::st_write(bg, con, "tl_2018_19_bg")

DBI::dbDisconnect(con)


unzip(zipfile = "__Personal__/2019MBDSS/data/Census/tiger/tl_2018_19_tabblock10.zip",
      exdir = "__Personal__/2019MBDSS/data/Census/tiger/tl_2018_19_tabblock10")

con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "user1",
                      host = "postgis",
                      port = "5432",
                      user = "user1",
                      password = "user1")

blks <- sf::st_read("__Personal__/2019MBDSS/data/Census/tiger/tl_2018_19_tabblock10/tl_2018_19_tabblock10.shp")

sf::st_write(blks, con, "tl_2018_19_tabblock10")

DBI::dbDisconnect(con)
