con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "user1",
                      host = "postgis",
                      port = "5432",
                      user = "user1",
                      password = "user1")

sql <- "SELECT * FROM tl_2018_19_bg WHERE \"COUNTYFP\" = '127'"
dat <- sf::st_read(con, query = sql)
plot(dat[,c("ALAND")])

DBI::dbDisconnect(con)


con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "user1",
                      host = "postgis",
                      port = "5432",
                      user = "user1",
                      password = "user1")

sql <- "SELECT * FROM tl_2018_19_tabblock1 WHERE \"COUNTYFP10\" = '127'"
dat <- sf::st_read(con, query = sql)
plot(dat[,c("ALAND10")])

DBI::dbDisconnect(con)

