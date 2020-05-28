
# Get Crime Data

if (!file.exists("crimes.RDS")) {
  get_crime = function() {
    conn <- get_db_conn()
    output <- dbReadTable(conn = conn,
                         name = c('acpd', 'incidents_filtered')) %>%
      data.table()
    on.exit(dbDisconnect(conn = conn))
    return(value = output)
  }
  crimes_data <- get_crime()
  crimes_data$Category <- crimes_data$crime_category

  saveRDS(crimes_data, "crimes.RDS")
}

print("Loading Crimes Data File...")
crimes_data <- readRDS("crimes.RDS")

make_datatable <- function(crime_category, crimes_data) {
  setDT(crimes_data)
  c <- crimes_data[Category == crime_category, ]
  dt <-
    DT::datatable(
      c,
      extensions = c('Buttons', 'FixedColumns'),
      filter = "top",
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'print'),
        scrollX = TRUE,
        fixedColumns = TRUE
      )
    )
  dt
}
make_datatable("Aggrevated Assault", crimes_data)
