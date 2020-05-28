con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "user1",
                      host = "postgis",
                      port = "5432",
                      user = "user1",
                      password = "user1")

sql <- "SELECT * FROM tl_2018_19_bg"
dat <- sf::st_read(con, query = sql)
plot(dat[,c("ALAND")])

DBI::dbDisconnect(con)
