
# Get Crime Hours

if (!file.exists("crime_hours.RDS")) {
  get_crime = function() {
    conn <- get_db_conn()
    on.exit(dbDisconnect(conn))
    data.table::setDT(
      dbReadTable(conn = conn,
                  name = c('acpd', 'incidents_filtered')))
  }

  acpd_data <- get_crime() %>%
    setDT()
  acpd_data <- acpd_data[crime_category != "Traffic/Parking Violations",]
  acpd_data %>% dt_mutate(year = year(acpd_data$start))
  crime_hours <- acpd_data[, .N, list(hour, type = crime_category, year)]

  saveRDS(crime_hours, "crime_hours.RDS")
}

print("Loading Data Files...")
crime_hours <- readRDS("crime_hours.RDS")

make_heatmap <- function(crime_category, crime_hours) {
  setDT(crime_hours)
  ch <- crime_hours[type == crime_category,]
  heatmap <- plotly::plot_ly(
    y = ch$hour,
    x = ch$year,
    z = ch$N,
    type = "heatmap"
  ) %>%
    plotly::layout(
      title = paste(crime_category),
      xaxis = list(title = "Year", type = "category"),
      yaxis = list(title = "Hour of Day", type = "numeric", dtick = 1))

  heatmap
}

make_heatmap("DUI", crime_hours)
