# Read In Packages
for (pkg in c("tableHTML","usmap","shinydashboard", "shiny", "leaflet", "dplyr", "httr", "here", "maptools","gpclib","sp", 'sf', 'ggplot2', 'ggmap', 
              'tidyverse', 'tigris', 'acs', 'data.table', 'maditr', 'viridis', 'ggplot2', 'usmap', 'rapportools', 'RPostgreSQL', 'DBI', 'DT', 'shinythemes')) {
  library(pkg, character.only = TRUE)
}

# Create data for plotting
acs_fcc_shapes <- function(state, geography, r_u){
  state_fips = usmap::fips(state)
  if(geography == "Block Group"){
    acs_file <- here("data", "dashboard", "summary_acs.csv")
    acs <- fread(acs_file, colClasses=c(state = "character", county = "character", census_tract = "character", block_group = "character"))
    fcc_file <- here("data", "dashboard", "fcc_processed_25.csv")
    fcc <- fread(fcc_file, colClasses = c(state = "character", county = "character", tract = "character", block_group = "character")) 
  } else if(geography == "Census Tract"){
    acs_file <- here("data", "dashboard", "summary_acs_census_tract.csv")
    acs <- fread(acs_file, colClasses = c(state = "character", county = "character", census_tract = "character"))
    fcc_file <- here("data", "dashboard", "fcc_processed_tract_25.csv")
    fcc <- fread(fcc_file, colClasses=c(state_fips = "character", county_short = "character", county = "character", tract_short = "character", tract = "character"))   
  }
  
  # Merge FCC & ACS
  if(geography =='Block Group'){
    fcc_acs = merge(fcc, acs, by.x = c('state', 'county', 'tract', 'block_group'), by.y = c('state', 'county', 'census_tract', 'block_group')) %>% 
      dt_filter(state == state_fips)
  } else if(geography == "Census Tract") {
    fcc_acs = merge(fcc, acs, by.x = c('state_fips', 'county_short', 'tract_short'), by.y = c('state', 'county', 'census_tract')) %>%
      dt_filter(state_fips == state_fips)  
  }
  # Pull shapes for state
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   dbname = "gis",
                   host = "localhost",
                   port = "5434",
                   user = "bband_user",
                   password = "bband")
  # Pull shapes on tract or block level
  if(geography == 'Block Group'){
    geo = st_read(con, c("census_cb", sprintf("cb_2018_%s_bg_500k", state_fips)))
  } else if(geography == "Census Tract"){
    geo = st_read(con, c("census_cb", sprintf("cb_2018_%s_tract_500k", state_fips)))  
  }
  
  DBI::dbDisconnect(con)
  
  # Merge shapes and data
  if(geography == "Block Group") {
    full <- merge(fcc_acs, geo, by.x = c('state', 'county', 'tract', 'block_group'), by.y = c('STATEFP', 'COUNTYFP', 'TRACTCE', 'BLKGRPCE'))
    full_sf <- full %>% dt_select(state,county,tract, block_group, consumer_has, business_has,
                                  maxaddown, maxcirdown, stateabbr, num_ppl, availability_cons,
                                  availability_bus, pcat_all_pct_min, pcat_all_pct_max, pcat_all_10x1_min,
                                  pcat_all_10x1_max, State, County_Name, Population_2010, RUCC_2013.x, 
                                  B28002_004_per, B28002_007_per, ALAND, AWATER, geometry) %>% 
      dt_mutate(rural_urban = ifelse(RUCC_2013.x < 4, 'Urban', 'Rural')) %>% rename(replace = c("RUCC_2013.x" = "RUCC_2013"))
    
  } else if(geography == "Census Tract"){
    full <- merge(fcc_acs, geo, by.x = c('state_fips', 'county_short', 'tract_short'), by.y =c('STATEFP','COUNTYFP','TRACTCE'))
    full_sf <- full %>% dt_select(state,county,tract, tract_short,
                                  maxaddown,state_fips, availability_cont,
                                  availability_adv, pcat_all_pct_min, pcat_all_pct_max, pcat_all_10x1_min,
                                  pcat_all_10x1_max, State, County_Name, Population_2010, RUCC_2013, 
                                  B28002_004_per, B28002_007_per, ALAND, AWATER, geometry) %>% 
      dt_mutate(rural_urban = ifelse(RUCC_2013 < 4, 'Urban','Rural'))
  }
  
  # Allow to filter by rural/urban
  if(r_u == 'Rural') {
    full_sf %>% data.table() %>% dt_filter(rural_urban == 'Rural') %>% st_as_sf()
  } else if (r_u == 'Urban') {
    full_sf %>% data.table() %>% dt_filter(rural_urban == 'Urban') %>% st_as_sf()
  } else if (r_u == 'All') {
    full_sf = full_sf %>% st_as_sf()
  }
}

county_shapes <- function(state, r_u){
  state_fips = usmap::fips(state)
  mic <- here("data", "dashboard", 'microsoft.csv')
  microsoft <- read.csv(mic, colClasses = c(ST = "character", COUNTY.ID = "character", 
                                          BROADBAND.USAGE = "numeric", BROADBAND.AVAILABILITY.PER.FCC = "numeric"),
                        na.strings = "-")
  microsoft$county <- ifelse(nchar(microsoft$COUNTY.ID) != 5 , gsub(" ", "", paste("0", microsoft$COUNTY.ID), fixed = TRUE), microsoft$COUNTY.ID)
  
  # Merge with FCC
  fcc_data <- here("data", "dashboard", "fcc_processed_county_25.csv")
  fcc_county <- read.csv(fcc_data, colClasses = c(state = "character", county = "character"))
  fcc_mic = merge(fcc_county, microsoft, by = 'county')
  # Get shapes
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   dbname = "gis",
                   host = "localhost",
                   port = "5434",
                   user = "bband_user",
                   password = "bband")
  
  geo = st_read(con, c("census_cb", "cb_2016_us_county_500k"))
  DBI::dbDisconnect(con)
  
  # Merge with shapes
  full = merge(fcc_mic, geo, by.x = 'county', by.y = 'GEOID') 
  full_st = full[full$STATEFP == state_fips,] %>% data.table() %>%
    dt_mutate(rural_urban = ifelse(RUCC_2013 < 4, 'Urban','Rural')) %>% st_as_sf()
  
  # Allow to filter by rural/urban
  if(r_u == 'Rural') {
    full_st %>% data.table() %>% dt_filter(rural_urban == 'Rural') %>% st_as_sf()
  } else if (r_u == 'Urban') {
    full_st %>% data.table() %>% dt_filter(rural_urban == 'Urban') %>% st_as_sf()
  } else if (r_u == 'All') {
    full_st = full_st %>% st_as_sf()
  }
}

make_state_map <- function(state, geography, r_u){
  print("Building Map...")
  if(geography == 'Block Group'){
    data <- acs_fcc_shapes(state, geography, r_u) %>% st_transform(4326)
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
            formatC(data$ALAND, format = "f", big.mark = ",", digits = 0),
            "<br />",
            "<strong>Population (2010): </strong>",
            data$Population_2010,
            "<br />",
            "<strong>Rural-Urban Continuum Code: </strong>",
            data$RUCC_2013,
            "<br />",
            "<strong>Broadband Availability (FCC): </strong>",
            round(data$availability_cons*100, 1),"%",
            "<br />",
            "<strong>Broadband Subscription (ACS): </strong>",
            round(data$B28002_007_per, 1),"%",
            "<br />",
            "<strong>Broadband Coverage Discrepancy (FCC-ACS): </strong>",
            abs(round(data$availability_cons*100 - data$B28002_007_per, 1)),"%",
            # "<br />",
            # "<strong>Broadband Subscription (Any Type): </strong>",
            # round(data$B28002_004_per, 1),"%",
            # "<br />",
            # "<strong>FCC Subscription Coverage 10x1 (Max): </strong>",
            # round(data$pcat_all_10x1_max*100, 1),"%",
            # "<br />",
            # "<strong>FCC Subscription Coverage 10x1 (Min): </strong>",
            # round(data$pcat_all_10x1_min*100, 1),"%",
            # "<br />",
            # "<strong>ACS Internet in FCC Subs Bin: </strong>",
            # data$B28002_007_per < data$pcat_all_10x1_max*100 & data$B28002_007_per > data$pcat_all_10x1_min*100,
            "<br />"
      ),
      htmltools::HTML
    )
    
    bins <- c(0, 21, 41, 61, 81, 100)
    binpal<- colorBin("YlOrRd", abs(round(data$availability_adv*100 - data$B28002_007_per, 1)), bins = bins, pretty = FALSE)
    m = leaflet(data = data)
    
    m <- addPolygons(m,
                     stroke = TRUE,
                     weight = .8,
                     color = 'lightgray',
                     smoothFactor = 0.2,
                     label = labels,
                     highlight = highlightOptions(
                       weight = 5,
                       color = "black"),
                     
                     labelOptions = labelOptions(direction = "bottom",
                                                 style = list(
                                                   "font-size" = "12px",
                                                   "border-color" = "rgba(0,0,0,0.5)",
                                                   direction = "auto"
                                                 )),
                     fillColor = ~binpal(abs(round(data$availability_cons*100 - data$B28002_007_per, 1))),
                     fillOpacity = 0.7
    )
    
    cl <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
    
    leg <- c("0%-20%", "21%-40%", "41%-60%", "61%-80%", "81%-100%")  
    m <- addLegend(m,
                   position = "bottomleft", colors = cl, values = ~(abs(round(data$availability_cons*100 - data$B28002_007_per, 1))),
                   title = "Broadband Coverage Discrepancy<br>(% FCC Availability - % ACS Subscription)",
                   opacity = 1, labels = leg)
    m
    
  } else if (geography  == 'Census Tract') {
    data <- acs_fcc_shapes(state, geography, r_u) %>% st_transform(4326)
    labels <- lapply(
      paste("<strong>County: </strong>",
            data$County_Name,
            "<br />",
            "<strong>Tract: </strong>",
            data$tract_short,
            "<br />",
            "<strong>Land Area (square meters): </strong>",
            formatC(data$ALAND, format = "f", big.mark = ",", digits = 0),
            "<br />",
            "<strong>Population (2010): </strong>",
            data$Population_2010,
            "<br />",
            "<strong>Rural-Urban Continuum Code: </strong>",
            data$RUCC_2013,
            "<br />",
            "<strong>Broadband Availability (FCC): </strong>",
            round(data$availability_adv*100, 1),"%",
            "<br />",
            "<strong>Broadband Subscription (ACS): </strong>",
            round(data$B28002_007_per, 1), "%",
            "<br />",
            "<strong>Broadband Coverage Discrepancy (FCC-ACS): </strong>",
            abs(round(data$availability_adv*100 - data$B28002_007_per, 1)),"%",
            # "<br />",
            # "<strong>ACS Coverage: Broadband (Any Type) (004): </strong>",
            # round(data$B28002_004_per, 1),"%",
            # "<br />",
            # "<strong>FCC Subscription Coverage 10x1 (Max): </strong>",
            # round(data$pcat_all_10x1_max*100, 1),"%",
            # "<br />",
            # "<strong>FCC Subscription Coverage 10x1 (Min): </strong>",
            # round(data$pcat_all_10x1_min*100, 1),"%",
            # "<br />",
            # "<strong>ACS Internet in FCC Subs Bin: </strong>",
            # data$B28002_007_per < data$pcat_all_10x1_max*100 & data$B28002_007_per > data$pcat_all_10x1_min*100,
            "<br />"
      ),
      htmltools::HTML
    )
    
    bins <- c(0, 21, 41, 61, 81, 100)
    binpal<- colorBin("YlOrRd", abs(round(data$availability_adv*100 - data$B28002_007_per, 1)), bins = bins, pretty = FALSE)
    m = leaflet(data = data)
    m <- addPolygons(m,
                     stroke = TRUE,
                     weight = 1,
                     color = "lightgray",
                     smoothFactor = 0.2,
                     label = labels,
                     highlight = highlightOptions(
                       weight = 5,
                       color = "black"),
                     
                     labelOptions = labelOptions(direction = "bottom",
                                                 style = list(
                                                   "font-size" = "12px",
                                                   "border-color" = "rgba(0,0,0,0.5)",
                                                   direction = "auto",
                                                   offset = c(1, 5)
                                                 )),
                     fillColor = ~binpal(abs(round(data$availability_adv*100 - data$B28002_007_per, 1))),
                     fillOpacity = 0.7
    )
    
    cl <- c("#FFFFB2","#FECC5C","#FD8D3C","#F03B20","#BD0026")
    
    leg <- c("0%-20%", "21%-40%", "41%-60%", "61%-80%", "81%-100%")
    m <- addLegend(m,
                   position = "bottomleft", colors = cl, values = ~(abs(round(availability_adv*100 - B28002_007_per, 1))),
                   title = "Broadband Coverage Discrepancy<br>(% FCC Availability - % ACS Subscription)",
                   opacity = 1, labels = leg)
    
  } else if (geography == 'County') {
    data <- county_shapes(state, r_u) %>% st_transform(4326)
    labels <- lapply(
      paste("<strong>County: </strong>",
            data$COUNTY.NAME,
            "<br />",
            "<strong>Land Area (square meters): </strong>",
            formatC(data$ALAND, format = "f", big.mark = ",", digits = 0),
            "<br />",
            "<strong>Population (2010): </strong>",
            data$Population_2010,
            "<br />",
            "<strong>Rural-Urban Continuum Code: </strong>",
            data$RUCC_2013,
            "<br />",
            "<strong>Broadband Availability (FCC): </strong>",
            round(data$availability_adv*100, 1),"%",
            "<br />",
            "<strong>Broadband Usage (MS): </strong>",
            round(data$BROADBAND.USAGE*100, 1),"%",
            "<br />",
            "<strong>Broadband Coverage Discrepancy (FCC-MS): </strong>", 
            abs(round(data$availability_adv*100 - data$BROADBAND.USAGE*100, 1)),"%",
            # "<br />",
            # "<strong>FCC Subscription Coverage (Max): </strong>",
            # round(data$max_pcat_10x1_per*100, 1),"%",
            # "<br />",
            # "<strong>FCC Subscription Coverage (Min): </strong>",
            # round(data$min_pcat_10x1_per*100, 1),"%",
            # "<br />",
            # "<strong>Microsoft Usage In FCC Subs Bin: </strong>",
            # data$BROADBAND.USAGE*100 < data$max_pcat_10x1_per*100 & data$BROADBAND.USAGE*100 > data$min_pcat_10x1_per*100,
            "<br />"
      ),
      htmltools::HTML
    )

    bins <- c(0, 21, 41, 61, 81, 100)
    binpal<- colorBin("YlOrRd", abs(round(data$availability_adv*100 - data$BROADBAND.USAGE*100, 1)), bins = bins, pretty = FALSE)

    m = leaflet(data = data)
    m <- addPolygons(m,
                     stroke = TRUE,
                     weight = 1,
                     color = "lightgray",
                     smoothFactor = 0.2,
                     label = labels,
                     highlight = highlightOptions(
                       weight = 5,
                       color = "black"),
                     
                     labelOptions = labelOptions(direction = "bottom",
                                                 style = list(
                                                   "font-size" = "12px",
                                                   "border-color" = "rgba(0,0,0,0.5)",
                                                   direction = "auto",
                                                   offset = c(1, 5)
                                                 )),
                     fillColor = ~binpal(abs(round(data$availability_adv*100 - data$BROADBAND.USAGE*100, 1))),
                     fillOpacity = 0.7
    ) 
    
    labelOptions = labelOptions(direction = "bottom",
                                style = list(
                                  "font-size" = "12px",
                                  "border-color" = "rgba(0,0,0,0.5)",
                                  direction = "auto"
                                ))
    
    cl <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
    
    leg <- c("0%-20%", "21%-40%", "41%-60%", "61%-80%", "81%-100%")
    
    m <- addLegend(m,
                   position = "bottomleft", colors = cl, values = ~(abs(round(availability_adv*100 - BROADBAND.USAGE*100,  1))),
                   title = "Broadband Coverage Discrepancy<br>(% FCC Availability - % MS Usage)",
                   opacity = 0.7, labels = leg)
  }
}

server <- function(input, output, session){
  data <- reactive({
    x <- acs_fcc_shapes(input$State, input$Geography, input$R_U) %>% st_transform(4326)
  })
  
  output$mymap <- renderLeaflet({
    make_state_map(input$State, input$Geography, input$R_U)
  })
  
  definitions_file <- here("data", "dashboard", "definitions_key.csv")
  definitions <- read.csv(definitions_file)
  output$table <- DT::renderDataTable(definitions %>% dt_filter(Geography == input$Geography), 
                                  options = list(searching = FALSE,
                                                 paging = FALSE,
                                                 pageLength = 15,
                                                 lengthMenu = list(c(1, 2, 3), c('5', '15', 'All'))
                                  ))
}

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  # theme = "bootstrap.css",
  #shinythemes::themeSelector(),
  title = "Comparing US Broadband Availability, Subscription, and Usage",
  headerPanel(img(src = 'logo.png', class = 'topimage', width = '20%', style = 'display: block; margin-left: auto; margin-right: auto;')),
  fluidRow(width = 12, 
           column(12, align = 'center', h1(strong('US Broadband Coverage Discrepancy Map')))
  ),
  hr(),
  fluidRow(width = 12,
           column(1),
           column(10,
                  em('This dashboard was created as part of the Data Science for Public Good program in the Social and Decision Analytics Division of the Biocomplexity Institute and Initiative
                      at the University of Virginia, in partnership with the US Department of Agriculture Economic Research Service.'),
                  p(),
                  br('Internet at broadband speeds—defined as 25mbps download and 3mbps upload—is still not available to many Americans. However, estimates differ on how many individuals are 
                     without access to broadband and thus are limited in their ability to participate in today’s increasingly online world. To better understand US broadband coverage and where 
                     estimates disagree, our Data Science for Public Good team set out to examine three publicly available broadband data sources: the Federal Communications Commission (FCC)
                     data, American Community Survey (ACS) data, and Microsoft (MS) Airband Initiative data. Our aims are to understand the extent of coverage according to each dataset, to examine 
                     discrepancies between the ACS and Microsoft with FCC data as the source used for policy and funding decision-making, and to address these aims with particular attention to rural areas.'),
                  p(),
                  tags$b('This dashboard visualizes discrepancies between FCC-reported broadband availability and ACS-reported broadband subscription at census block group and census tract levels,
                     and the discrepancies between FCC-reported broadband availability and MS-reported broadband usage at the county level.'), ('Using the map selector allows filtering by state,
                     level of geography, and urban status. Hovering over each area on the resulting map displays information about the geography, land area, population, urban status, and broadband coverage.
                     Detailed data sources and measure descriptions are available below.'),
                  p(),
                  br('You can download the accompanying project poster',  a(href = 'https://github.com/uva-bi-sdad/dspg19broadband/files/4167827/BroadbandPoster.pdf', 'here.'), 'A policy-oriented brief is also forthcoming.')
            ),
           column(1)
  ),
  hr(),
  fluidRow(width = 12, style = "margin: 20px",
           h2('Data Sources and Key Measures')
  ),
  fluidRow(style = "margin: 6px",
    column(3, wellPanel(strong('Federal Communications Commission (FCC): Broadband Availability'), 
                        p(),
                        em('Description.'),
                        br('The FCC collects broadband availability and subscription data from providers using Form 477 every six months. Facilities-based providers of broadband connections to end-users 
                          are required to report information on broadband deployment and subscriptions, including whether fixed broadband connection is available in a census block. Data are available 
                          at multiple time points and can be aggregated from block group to census tract and county levels. '),
                        p(),
                        em('How We Measure Broadband Availability.'),
                        br(), tags$b('Our measure of broadband availability indicates the percent of the population in a given geographic unit (census block, census tract, or county) with access to at least one 
                           broadband provider offering at least a 25 advertised download speed.'), ('We use FCC provider and maximum advertised download speed information from December 2015 (at census block 
                           group and census tract level for comparisons with ACS, and at county level for comparisons with Microsoft), and ACS 2013-17 (5-year) population estimates to calculate the percent 
                           of the population in a given geography with broadband access.'),
                        p(),
                        em('Source and More Information.'),
                        tags$li('Data description: ', a(href = 'https://www.fcc.gov/general/explanation-broadband-deployment-data', 'Broadband deployment data explanation', target="_blank")),
                        tags$li('Data source: ', a(href = 'https://www.fcc.gov/form-477-broadband-deployment-data-december-2015-version-4', 'FCC Form 477 December 2015', target="_blank")))),
    
    column(3, wellPanel(strong('American Community Survey (ACS): Broadband Subscription'),
                        p(),
                        em('Description.'),
                        br('US Census Bureau’s ACS is an annual, nationally representative US household survey. It provides estimates on population sociodemographic characteristics and select topics, 
                          including internet access. Contrary to the FCC provider-reported data, the ACS relies on household head self-reports. Broadband estimates became available for the first time 
                          at the census block level in its 2013-17 (5-year) data, which we used in our analyses.'),
                        p(),
                        em('How We Measure Broadband Subscription.'),
                        br(), tags$b(' Our measure of broadband subscription indicates the percent of census block group or tract households that self-reports having access to any kind of broadband connection, 
                        excluding satellite and cellular.'), ('We obtain broadband subscription estimates from ACS 2013-17 (5-year) data table B28002, which contains information on the presence and 
                        types of internet subscriptions in households. We use estimates at census block group and census tract levels for comparisons with FCC. To calculate the metric, 
                        we divide the number of households reporting such access by the total population in a given geographic unit.'),
                        p(),
                        em('Source and More Information.'),
                        tags$li('Data description: ', a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target="_blank")),
                        tags$li('Data source: ', a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002 - Presence and types of internet subscriptions in household', target="_blank")))),
    column(3, wellPanel(strong('Microsoft (MS) Airband Initiative: Broadband Usage'),
                        p(),
                        em('Description.'),
                        br('Microsoft broadband data come from the company’s one-time initiative to collect data on broadband coverage using customer access as part of the Airband Initiative. 
                          Contrary to FCC and ACS data, Microsoft data are neither provider- nor consumer-reported; instead, the company analyzed its server logs when electronic devices downloaded 
                          Microsoft Windows and Office updates, accessed Microsoft’s Bing search, or used Xbox gaming consoles. Microsoft aggregated their 2018 data and made them available at the county level.'),
                        p(),
                        em('How We Measure Broadband Usage.'),
                        br(), tags$b('Our measure of broadband usage indicates the proportion of county population that uses Microsoft services (e.g., uses Bing.com, downloads Microsoft updates, or uses Xbox) at 
                        25 mbps download speeds, according to Microsoft server logs.'), ('We use Microsoft data from 2018 at county level for comparisons with FCC; Microsoft performed the calculation of 
                        population proportions and aggregation to county level.'),
                        p(),
                        em('Source and More Information.'),
                        tags$li('Data report: ', a(href = 'https://news.microsoft.com/rural-broadband/', 'Microsoft Airband: An update on connecting rural America', target="_blank")),
                        tags$li('Data request: ', a(href = 'https://forms.office.com/Pages/ResponsePage.aspx?id=v4j5cvGGr0GRqy180BHbRxi63BqtSQBHm4t78vwTzcZUNkFKV04xMEVDN0RMNkxTWkVERVMyNlZHViQlQCN0PWcu', 'Request Microsoft Airband data', target="_blank")))),
    column(3, wellPanel(strong('Economic Research Service (ERS) Urban-Rural Continuum Codes (RUCC): Urbanicity'),
                        p(),
                        em('Description.'),
                        br('US Department of Agriculture’s Economic Research Service provides RUCC codes to characterize urbanicity at county level. The agency updates RUCC codes at each decennial census, with 
                           the latest available data from 2013. The nine RUCC codes consist of three metro and six nonmetro categories. Metro categories are designated depending on metro area population size, 
                           and nonmetro codes are designated based on degree of urbanization and metro area adjecency.'),
                        p(),
                        em('How We Measure Urbanicity.'),
                        br('We use 2013 RUCC data to measure area urban status. We collapse RUCC codes 1 through 6 to indicate urban (metro) status, and codes 7 through 9 to indicate rural (nonmetro) status.'),
                        p(),
                        em('Source and More Information.'),
                        tags$li('Data description: ', a(href = 'https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx', 'Urban-Rural Continuum Codes', target="_blank")),
                        tags$li('Data file used: ', a(href = 'https://www.ers.usda.gov/webdocs/DataFiles/53251/ruralurbancodes2013.xls?v=0', 'Urban-Rural Continuum Codes 2013', target="_blank"))))
   ),
  hr(),
  fluidRow(width = 12, style = "margin: 20px",
           column(2,
                  h2('Map Selector'),
                  selectInput("State", "State", choices = state.abb, selected = 'AL', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL),
                  selectInput("Geography", "Geography", c("County", "Census Tract", "Block Group"), 
                              selected = 'County', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL),
                  selectInput("R_U", "Urban Status", c("Rural", "Urban", "All"), selected = 'All', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL),
                  p(),
                  em('County-level maps visualize a FCC broadband availability and MS broadband usage comparison. Census tract and census block group-level maps visualize a
                     FCC broadband availability and ACS broadband subscription comparison.')
           ),
           column(10, leafletOutput("mymap", height = 550, width = "100%")
           )
  ),
  hr(),
  fluidRow(width = 12, style = "margin: 20px",
           column(12, 
                  h2('Table of Measures'),
                  DT::dataTableOutput('table')
           )
  ),
  hr(),
  fluidRow(width = 12, style = "margin: 20px",
           column(6, align = 'center',
                  h2('Acknowledgments'),
                  br('We would like to thank our Data Science for Public Good program participants -- ', a(href = 'https://www.linkedin.com/in/kateryna-savchyn/', 'Kateryna Savchyn'), ',', a(href = 'https://www.linkedin.com/in/sarahmcdnld', 'Sarah McDonald'), ', and ', a(href = 'https://www.linkedin.com/in/raghavsawhney96/', 'Raghav Sawhney'), '-- for their valuable contributions to this project.')
           ),
           column(6, align = 'center',
                  h2('Contact'),
                  br(a(href = 'https://biocomplexity.virginia.edu/joshua-goldstein', 'Joshua Goldstein'), 'and', a(href = 'https://biocomplexity.virginia.edu/teja-pristavec', 'Teja Pristavec')),
                  br('University of Virginia, Biocomplexity Institute and Initiative'),
                  br(a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics', 'Social and Decision Analytics Division', target = '_blank'))
                  )
           ),
  hr(),
  fluidRow(width = 12, style = "margin: 20px",
           column(12, align = 'center',
                  em('Last updated: February 2020'))
           )
)

shinyApp(ui = ui, server = server)