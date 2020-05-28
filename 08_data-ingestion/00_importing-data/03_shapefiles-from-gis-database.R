con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "gis",
                      host = "postgis_1",
                      port = "5432",
                      user = Sys.getenv("db_userid"),
                      password = Sys.getenv("db_pwd"))

DBI::dbListTables(con)
DBI::dbGetQuery(con, "SELECT table_schema, table_name FROM information_schema.tables ORDER BY table_schema, table_name")

mygeo <- sf::st_read(con, c("census_cb", "cb_2016_us_county_500k"))
DBI::dbDisconnect(con)

plot(mygeo[mygeo$STATEFP=="51", c("ALAND")])

plot(mygeo[mygeo$STATEFP %in% c("51", "54", "24"), c("ALAND")])



