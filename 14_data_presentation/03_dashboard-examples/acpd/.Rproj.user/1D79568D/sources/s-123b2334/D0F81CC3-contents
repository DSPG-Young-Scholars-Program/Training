#========Interactive Dashboard======================

if (!("pacman" %in% installed.packages()[,1])) {
  install.packages(pkgs = "pacman")
}
pacman::p_load(RPostgreSQL, tidyverse, maditr, sf, geosphere,
               semantic.dashboard, shiny.semantic,
               leaflet, plotly, DT, here, leaflet.mapboxgl)

ui <- dashboardPage(
  dashboardHeader(color = "blue", title = "Alcohol-Related Crime", inverted = TRUE),
  dashboardSidebar(
    color = "teal",
    sidebarMenu(
      menuItem(tabName = "overview", "ARI Overview", icon = icon("info circle")),
      menuItem(tabName = "map", "Map", icon = icon("map")),
      menuItem(tabName = "data", "Data", icon = icon("database")),
      menuItem(tabName = "heatmap", "Heatmap", icon = icon("fire"))
    )
  ),
  dashboardBody(
    tabItems(
      selected = 1,

      tabItem(
        tabName = "overview",
        fluidRow(
          box(width = 16,
              title = "ARI Overview",
              color = "red",
              h1("Evaluating the Impact of the Arlington Restaurant Initiative on Alcohol-Related Crimes in Clarendon"),
              h3("Objective"),
              p("Evaluate effectiveness (social and economic impact) of ARI in Clarendon to help ACPD sustain and support the program"),
              h3("Background"),
              p("Arlington County features some of the most unique restaurants and nightlife destinations in the Washington D.C. metro region. Areas such as Clarendon, however, with a large number of restaurants have become a difficult issue for police to manage due to alcohol-related crimes such as malicious wounding, sexual assault, public intoxication, assault on police, DUI, disorderly conduct, and rape."),
              p("Arlington County Police Department (ACPD) launched the Arlington Restaurant Initiative (ARI) that focuses on best practices for restaurants and nightlife to reduce the risk of alcohol-related disorder. The initiative grew out of the Clarendon Detail, the creation of a team of patrol officers using overtime to control pedestrian and road traffic, and to ensure that intoxicated patrons are protected from harm."),
              HTML("<figure>"),
              img(src="ARI_circle.png", style="display: block; margin-left: auto; margin-right: auto;"),
              HTML("<figcaption>
                   <br />Clarendon has over 40 restaurants (pinned in blue circles) with ABC (Alcohol and Beverage Control) licenses and an average of 5,500 patrons per weekend night. Each year, approximately 580,00 patrons visit Clarendon between 21:00 and 03:00, especially during holidays and special ”drinking” events. Alcohol-related crime counts in Clarendon on Friday, Saturday, and Sunday between 21:00 and 03:00 for 2015-2017 are given in yellow and green circles.
                   </figcaption>
                   </figure>"),
              h3("Research Team"),
              div("DSPG: Victoria Halewicz, and Kateryna Savchyn (DSPG Fellow)"),
              div("SDAL: Gizem Korkmaz, Aaron Schroeder, and José Bayoán Santiago Calderón"),
              div("Sponsors: Jim Mastoras, Arlington County Police Department")
              )
          )
        ),

      tabItem(
        tabName = "map",
        fluidRow(
          box(width = 4,
              title = "Crime Type",
              color = "red",
              dropdown("type",
                       choices = c("Aggravated Assault", "Disorderly Conduct", "Drunkenness", "DUI", "Sexual Assault/Rape"),
                       choices_value = c("Aggravated Assault", "Disorderly Conduct", "Drunkenness", "DUI", "Sexual Assault/Rape"),
                       default_text = "Select",
                       value = "Drunkenness")
          ),
          box(width = 16,
              title = "Arlington Crime Map",
              color = "red",
              leafletOutput("map", height = 900))
        )
      ),

      tabItem(
        tabName = "data",
        fluidRow(
          box(width = 4,
              title = "Crime Type",
              color = "red",
              dropdown("dd1",
                       choices = c("Aggravated Assault", "Disorderly Conduct", "Drunkenness", "DUI", "Sexual Assault/Rape"),
                       choices_value = c("Aggravated Assault", "Disorderly Conduct", "Drunkenness", "DUI", "Sexual Assault/Rape"),
                       default_text = "Select",
                       value = "DUI")
          ),
          box(width = 16,
              title = "Arlington Crime Data",
              color = "red",
              DT::DTOutput("crimesdata"))
        )
      ),

      tabItem(
        tabName = "heatmap",

        fluidRow(
          box(width = 4,
              title = "Heatmap Control",
              color = "red",
              dropdown("dd2",
                       choices = c("Aggravated Assault", "Disorderly Conduct", "Drunkenness", "DUI", "Sexual Assault/Rape"),
                       choices_value = c("Aggravated Assault", "Disorderly Conduct", "Drunkenness", "DUI", "Sexual Assault/Rape"),
                       default_text = "Select",
                       value = "Drunkenness")
          ),
          box(width = 9,
              title = "Arlington Crime Heatmap",
              color = "red",
              plotlyOutput("heatmap", height = 600))
        )
      )
    )
  ), theme = "cerulean"
  )
#=================Crime Type Drop Down==========================
server <- shinyServer(function(input, output, session) {
  source("db_connection.R")
  source("leaflet.R")
  source("heatmap.R")
  source("datatable.R")

  options(mapbox.accessToken = "pk.eyJ1IjoiYXNjaHJvZWQiLCJhIjoiY2prMnFtM2hsMDBnMDNwcGFkN2ZkczdsNCJ9.p3zPl9XlPc4BL5fwfMomTQ")

  output$map <- renderLeaflet(make_crime_map(input$type))

  observeEvent(input$dd1, {
    output$crimesdata <- renderDT(make_datatable(input$dd1, crimes_data))
  })

  observeEvent(input$dd2, {
    output$heatmap <- renderPlotly(make_heatmap(input$dd2, crime_hours))
  })
})

shinyApp(ui, server)
