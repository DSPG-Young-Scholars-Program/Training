# connect to chosen database
con <- sdalr::con_db("arlington")

# get all schemas from database
schemas <- DBI::dbGetQuery(con, "select schema_name
from information_schema.schemata
where schema_name not like 'pg_%'
and schema_name != 'test'
and schema_name != 'information_schema'")

# get all postgresql users
users <- DBI::dbGetQuery(con, "SELECT u.usename
FROM pg_catalog.pg_user u
WHERE u.usename NOT IN ('aschroed', 'chend', 'postgres', 'bjgoode')")

# grant permissions to all users on all tables in all schemas of database
# and save queries to db_user_query.txt
for (s in schemas$schema_name) {
  for (u in users$usename) {
    line1 <- paste0("grant all on schema ", s, " to ", u, ";")
    write(line1, "db_user_query.txt", append = T)
    line2 <- paste0("grant select on all tables in schema ", s, " to ", u, ";")
    write(line2, "db_user_query.txt", append = T)
  }
}

