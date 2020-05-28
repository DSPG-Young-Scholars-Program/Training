# Read In Packages
for (pkg in c("tableHTML","usmap","shinydashboard", "shiny", "leaflet", "dplyr", "httr", "here", "maptools","gpclib","sp", 'sf', 'ggplot2', 'ggmap', 
              'tidyverse', 'tigris', 'acs', 'data.table', 'maditr', 'viridis', 'ggplot2', 'usmap')) {
  library(pkg, character.only = TRUE)
}



# Create Data for Plotting
acs_fcc_shapes <- function(state, geography, r_u){
  state_fips = usmap::fips(state)
  if(geography == "Block Group"){
    acs_file <- here("data", "working", "summary_acs.csv")
    acs <- fread(acs_file, colClasses=c(state="character",county="character",census_tract="character", block_group = "character"))
    fcc_file <- here("data", "working", "fcc_processed.csv")
    fcc <- fread(fcc_file, colClasses=c(state="character",county="character",tract="character", block_group = "character")) 
  } else if(geography == "Census Tract"){
    acs_file <- here("data", "working", "summary_acs_census_tract.csv")
    acs <- fread(acs_file, colClasses=c(state="character",county="character",census_tract="character"))
    fcc_file <- here("data", "working", "fcc_processed_tract.csv")
    fcc <- fread(fcc_file, colClasses=c(state="character",county="character",tract_short="character", tract="character"))   
  }
  
  #merge fcc & acs 
  if(geography =='Block Group'){
    fcc_acs = merge(fcc, acs, by.x = c('state', 'county', 'tract', 'block_group'), by.y = c('state', 'county', 'census_tract', 'block_group')) %>% 
      dt_filter(state==state_fips)
  } else if(geography == "Census Tract") {
    fcc_acs = merge(fcc, acs, by.x = c('state', 'county', 'tract_short'), by.y = c('state', 'county', 'census_tract')) %>%
      dt_filter(state==state_fips)  
  }
  #pull shapes for state
  con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                        dbname = "gis",
                        host = "postgis_1",
                        port = "5432",
                        user = Sys.getenv("db_userid"),
                        password = Sys.getenv("db_pwd"))
  # pull shapes on tract or bl level
  if(geography =='Block Group'){
    geo = st_read(con, c("census_cb", sprintf("cb_2018_%s_bg_500k", state_fips)))
  } else if(geography == "Census Tract"){
    geo = st_read(con, c("census_cb", sprintf("cb_2018_%s_tract_500k", state_fips)))  
  }
  
  DBI::dbDisconnect(con)
  
  #merge shapes and data
  if(geography == "Block Group") {
    full <- merge(fcc_acs, geo, by.x = c('state', 'county', 'tract', 'block_group'), by.y =c('STATEFP','COUNTYFP','TRACTCE','BLKGRPCE'))
    full_sf <- full %>% dt_select(state,county,tract, block_group, consumer_has, business_has,
                                  maxaddown, maxcirdown, stateabbr, num_ppl, availability_cons,
                                  availability_bus, pcat_all_pct_min, pcat_all_pct_max, pcat_all_10x1_min,
                                  pcat_all_10x1_max, State, County_Name, Population_2010, RUCC_2013.x, 
                                  B28002_004_per, B28002_007_per, ALAND, AWATER, geometry) %>% 
      dt_mutate(rural_urban = ifelse(RUCC_2013.x < 4, 'Urban', 'Rural')) %>% rename("RUCC_2013" = "RUCC_2013.x")
    
  } else if(geography == "Census Tract"){
    full <- merge(fcc_acs, geo, by.x = c('state', 'county', 'tract_short'), by.y =c('STATEFP','COUNTYFP','TRACTCE'))
    full_sf <- full %>% dt_select(state,county,tract,consumer_has, business_has,
                                  maxaddown, maxcirdown, stateabbr, num_ppl, availability_cons,
                                  availability_bus, pcat_all_pct_min, pcat_all_pct_max, pcat_all_10x1_min,
                                  pcat_all_10x1_max, State, County_Name, Population_2010, RUCC_2013, 
                                  B28002_004_per, B28002_007_per, ALAND, AWATER, geometry) %>% 
      dt_mutate(rural_urban = ifelse(RUCC_2013 < 4, 'Urban','Rural'))
    
  }
  
  #allow to filter by rural/urban
  if(r_u == 'Rural') {
    full_sf %>% data.table() %>% dt_filter(rural_urban == 'Rural') %>% st_as_sf()
  } else if (r_u == 'Urban') {
    full_sf %>% data.table() %>% dt_filter(rural_urban == 'Urban') %>% st_as_sf()
  } else if (r_u == 'All') {
    full_sf = full_sf %>% st_as_sf()
  }
}





make_state_map <- function(stateabbr, geography, r_u){
  print("Building Map...")
  if(geography  == 'Block Group'){
    data <- acs_fcc_shapes(state, geography, r_u) %>% st_transform(4326)
    dat <- here('data', 'working', 'merged_by_rural_urban.csv')
    q <- read.csv(dat) %>% data.table() %>% dt_mutate(rural_urban = ifelse(RUCC_2013 < 4, 'Urban','Rural'))%>% dt_filter(stateid == unique(data$stateabbr)) %>% dt_filter(rural_urban == unique(data$rural_urban))
    labels <- lapply(
      paste("<strong>County: </strong>",
            data$County_Name,
            "<br />",
            "<strong>Tract: </strong>",
            data$tract,
            "<br />",
            "<strong>Block Group: </strong>",
            data$block_group,
            "<br />",
            "<strong>Land Area (square meters): </strong>",
            formatC(data$ALAND, format="f", big.mark = ",", digits = 0),
            "<br />",
            "<strong>Population (2010): </strong>",
            data$Population_2010,
            "<br />",
            "<strong>RUCC: </strong>",
            data$RUCC_2013,
            "<br />",
            "<strong>ACS Coverage: Broadband (004): </strong>",
            round(data$B28002_004_per,1),"%",
            "<br />",
            "<strong>ACS Coverage: Internet (007): </strong>",
            round(data$B28002_007_per,1),"%",
            "<br />",
            "<strong>FCC Subscription Coverage (Max): </strong>",
            round(data$pcat_all_pct_max*100,1),"%",
            "<br />",
            "<strong>FCC Subscription Coverage (Min): </strong>",
            round(data$pcat_all_pct_min*100,1),"%",
            "<br />",
            "<strong>ACS Internet In FCC Subs Bin: </strong>",
            data$B28002_007_per < data$pcat_all_pct_max*100 & data$B28002_007_per > data$pcat_all_pct_min*100,
            "<br />",
            "<strong>FCC Coverage (Consumer): </strong>",
            round(data$availability_cons*100,1),"%",
            "<br />",
            "<strong>FCC Coverage (Business): </strong>",
            round(data$availability_bus*100,1),"%",
            "<br />",
            "<strong>Percentile Discrepancy: </strong>",
            round(data$availability_cons*100 - data$B28002_007_per,1),"%"
      ),
      htmltools::HTML
    )
    qpal <- colorQuantile("YlOrRd", round(data$availability_cons*100 - data$B28002_007_per,1), n = 5)
    m = leaflet(data = data)
    m <- addPolygons(m,
                     stroke = TRUE,
                     weight = .8,
                     color = 'lightgray',
                     smoothFactor = 0.2,
                     label = labels,
                     highlight = highlightOptions(
                       weight = 5,
                       color = "black",
                       bringToFront = TRUE),
                     
                     labelOptions = labelOptions(direction = "bottom",
                                                 style = list(
                                                   "font-size" = "12px",
                                                   "border-color" = "rgba(0,0,0,0.5)",
                                                   direction = "auto"
                                                 )),
                     fillColor = ~qpal(round(data$availability_cons*100 - data$B28002_007_per,1)),
                     fillOpacity = 0.7
    )
    m <- addLegend(m,
                   position = "bottomleft", pal = qpal, values = ~(round(availability_cons*100 - B28002_007_per,1)),
                   title = "Percentile Difference: FCC v ACS",
                   opacity = 1)
    
    m <- addCircles(m,lng = q$long, lat = q$lat, label = as.character(q$city))
    m
  } else {
    data <- acs_fcc_shapes(state, geography, r_u) %>% st_transform(4326)
    labels <- lapply(
      paste("<strong>County:</strong>",
            data$County_Name,
            "<br />",
            "<strong>Tract:</strong>",
            data$tract,
            "<br />",
            "<strong>Land Area (square meters): </strong>",
            formatC(data$ALAND, format="f", big.mark = ",", digits = 0),
            "<br />",
            "<strong>Population (2010): </strong>",
            data$Population_2010,
            "<br />",
            "<strong>RUCC: </strong>",
            data$RUCC_2013,
            "<br />",
            "<strong>ACS Coverage: Broadband (004): </strong>",
            round(data$B28002_004_per,1),"%",
            "<br />",
            "<strong>ACS Coverage: Internet (007): </strong>",
            round(data$B28002_007_per,1),"%",
            "<br />",
            "<strong>FCC Subscription Coverage (Max): </strong>",
            round(data$pcat_all_pct_max*100,1),"%",
            "<br />",
            "<strong>FCC Subscription Coverage (Min): </strong>",
            round(data$pcat_all_pct_min*100,1),"%",
            "<br />",
            "<strong>ACS Internet In FCC Subs Bin: </strong>",
            data$B28002_007_per < data$pcat_all_pct_max*100 & data$B28002_007_per > data$pcat_all_pct_min*100,
            "<br />",
            "<strong>FCC Coverage (Consumer): </strong>",
            round(data$availability_cons*100,1),"%",
            "<br />",
            "<strong>FCC Coverage (Business): </strong>",
            round(data$availability_bus*100,1),"%",
            "<br />",
            "<strong>Percentile Discrepancy: </strong>",
            round(data$availability_cons*100 - data$B28002_007_per,1),"%"
      ),
      htmltools::HTML
    )
    qpal <- colorQuantile("YlOrRd", round(data$availability_cons*100 - data$B28002_007_per,1), n = 5)
    m = leaflet(data = data)
    m <- addPolygons(m,
                     stroke = TRUE,
                     weight = 1,
                     color = "lightgray",
                     smoothFactor = 0.2,
                     label = labels,
                     highlight = highlightOptions(
                       weight = 5,
                       color = "black",
                       bringToFront = TRUE),
                     
                     labelOptions = labelOptions(direction = "bottom",
                                                 style = list(
                                                   "font-size" = "12px",
                                                   "border-color" = "rgba(0,0,0,0.5)",
                                                   direction = "auto",
                                                   offset = c(1, 5)
                                                 )),
                     fillColor = ~qpal(round(data$availability_cons*100 - data$B28002_007_per,1)),
                     fillOpacity = 0.7
    ) 
    
    labelOptions = labelOptions(direction = "bottom",
                                style = list(
                                  "font-size" = "12px",
                                  "border-color" = "rgba(0,0,0,0.5)",
                                  direction = "auto"
                                ))
    fillColor = ~qpal(round(data$availability_cons*100 - data$B28002_004_per,1))
    fillOpacity = 0.7
    m <- addLegend(m,
                   position = "bottomleft", pal = qpal, values = ~(round(availability_cons*100 - B28002_007_per,1)),
                   title = "Percentile Difference: FCC v ACS",
                   opacity = 1)
    m <- addCircles(m,lng = q$long, lat = q$lat,label = as.character(q$city))
  }
}

server <- function(input,output,session){
  data <- reactive({
    x <- acs_fcc_shapes(input$State, input$Geography, input$R_U) %>% st_transform(4326)
  })
  
  output$mymap <- renderLeaflet({
    make_state_map(input$State, input$Geography, input$R_U)
  })
}

ui <- fluidPage(
  theme = "bootstrap.css",
  title = "Broadband DSPG 2019",
  
  fluidRow(width = 4, column(2.5,
                             img(src = 'ers_logo.png', class = 'topimage')
  ),
  column(7, 
         h1('Broadband Coverage: ACS and FCC'))
  ),
  hr(),
  fluidRow(width = 4,
           column(3,
                  selectInput("State", "Select State", choices = state.abb, selected = 'AL', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL)
           ),
           column(3, 
                  selectInput("Geography", "Select Geography", c("Census Tract", "Block Group"), selected = 'Census Tract', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL)
           ),
           column(3,
                  selectInput("R_U", "Rural/Urban", c("Rural", "Urban", "All"), selected = 'All', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL)
           )
  ),
  
  hr(),
  fluidRow(width = 4,leafletOutput("mymap",height = 580, width = 1200))
  
)

shinyApp(ui = ui, server = server)