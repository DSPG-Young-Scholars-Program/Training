get_db_conn <-
  function(db_name,
           db_host = "postgis_1",
           db_port = "5432",
           db_user = Sys.getenv("db_userid"),
           db_pass = Sys.getenv("db_pwd")) {
    RPostgreSQL::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_pass
    )
  }
