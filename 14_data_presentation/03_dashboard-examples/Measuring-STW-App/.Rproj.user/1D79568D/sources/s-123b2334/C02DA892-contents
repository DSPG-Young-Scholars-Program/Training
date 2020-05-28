source("db/src/db_connection.R")

# Burning Glass Jobs
library(data.table)
type <- "Skills"
yrs <- c("2007", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
for (y in yrs) {
  yr <- y
  path <- paste0("/project/biocomplexity/sdad/projects-active/ncses/stw/original/burning_glass/", type,"/", yr, "/")
  schema <- "burning_glass_jobs"
  for (i in 1:12) {
    filenum <- stringr::str_pad(i, 2, pad = "0")
    filezipname <- paste0(type, "_", yr, "-", filenum, ".zip")
    filename <- paste0(type, "_", yr, "-", filenum, ".txt")
    tablename <- tools::file_path_sans_ext(filename)
    fullfilezipname <- paste0(path, filezipname)
    fullfilename <- paste0(path, filename)
    unzip(fullfilezipname, exdir = path)
    dat <- fread(fullfilename)
    
    con <- get_db_conn()
    DBI::dbWriteTable(con, c(schema, tablename), dat, row.names = F)
    DBI::dbDisconnect(con)
    unlink(fullfilename)
  }
}

# Burning Glass Resumes
cats <- c("education", "job", "certification", "personal", "skill")
schema <- "burning_glass_resumes"
for (c in cats) {
  tbl = fread(paste0('/project/biocomplexity/sdad/projects-active/ncses/stw/original/burning_glass/Resume_Data/', c, ".csv"))
  #tbl = fread(cmd = paste0('cat /project/biocomplexity/sdad/projects-active/ncses/stw/original/burning_glass/Resume_Data/*', c, '*.gz | gunzip | grep -v "^BGT"'))
  #setnames(tbl, tblNames)
  
  con <- get_db_conn()
  DBI::dbWriteTable(con, c(schema, c), tbl, row.names = F)
  DBI::dbDisconnect(con)
}

con <- get_db_conn()
tabs <- DBI::dbGetQuery(con, "SELECT table_schema, table_name FROM information_schema.tables WHERE table_schema = 'burning_glass_jobs'")
DBI::dbDisconnect(con)

for (i in 1:nrow(tabs)) {
  tbl_name <- tabs[i, c("table_name")][[1]]
  idx_name <- paste0(tbl_name, "_BGTJobId_idx")
  qry <- paste0("CREATE INDEX \"", idx_name, "\" ON burning_glass_jobs.\"", tbl_name, "\" (\"BGTJobId\");")
  con <- get_db_conn()
  DBI::dbGetQuery(con, qry)
  DBI::dbDisconnect(con)  
}


con <- get_db_conn()
tabs <- DBI::dbGetQuery(con, "SELECT table_schema, table_name FROM information_schema.tables WHERE table_schema = 'burning_glass_resumes'")
DBI::dbDisconnect(con)

for (i in 1:nrow(tabs)) {
  tbl_name <- tabs[i, c("table_name")][[1]]
  idx_name <- paste0(tbl_name, "_BGTResId_idx")
  qry <- paste0("CREATE INDEX \"", idx_name, "\" ON burning_glass_resumes.\"", tbl_name, "\" (\"BGTResId\");")
  con <- get_db_conn()
  DBI::dbGetQuery(con, qry)
  DBI::dbDisconnect(con)  
}
