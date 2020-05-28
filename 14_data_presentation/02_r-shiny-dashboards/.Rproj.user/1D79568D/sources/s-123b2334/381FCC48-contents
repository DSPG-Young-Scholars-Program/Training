library(shiny)
library(semantic.dashboard)
library(shiny.semantic)
library(ggplot2)
library(plotly)
library(leaflet)

ui <- dashboardPage(
    dashboardHeader(color = "blue", title = "Dashboard Demo", inverted = TRUE),
    dashboardSidebar(
        color = "teal",
        sidebarMenu(
            menuItem(tabName = "main", "Main", icon = icon("car")),
            menuItem(tabName = "data", "Data", icon = icon("table")),
            menuItem(tabName = "map", "Map", icon = icon("globe"))
        )
    ),
    dashboardBody(
        tabItems(
            selected = 1,
            tabItem(
                tabName = "main",
                fluidRow(
                    box(width = 8,
                        title = "Graph 1",
                        color = "green", ribbon = TRUE, title_side = "top right",
                        column(8,
                               plotOutput("boxplot1")
                        )
                    ),
                    box(width = 8,
                        title = "Graph 2",
                        color = "red", ribbon = TRUE, title_side = "top right",
                        column(width = 8,
                               plotlyOutput("dotplot1")
                        )
                    )
                )
            ),
            tabItem(
                tabName = "data",
                fluidRow(
                    dataTableOutput("carstable")
                )
            ),
            tabItem(
                tabName = "map",
                
                fluidRow(
                    box(width = 4, title = "Map Control",
                        dropdown("dd1", 
                                 choices = c("ALL", "DRUNK", "BURGLARY"), 
                                 choices_value = c("ALL", "DRUNK", "BURGLARY"), 
                                 default_text = "Select", 
                                 value = "ALL")
                    ),
                    box(width = 12,
                        title = "Arlington Crime",
                        color = "red", ribbon = TRUE,
                        leafletOutput("map")
                    )
                )
            )
        )
    ), theme = "cerulean"
)

server <- shinyServer(function(input, output, session) {
    library(data.table)
    library(sf)
    # ggplot
    data("mtcars")
    mtcars$am <- factor(mtcars$am,levels=c(0,1),
                        labels=c("Automatic","Manual"))
    output$boxplot1 <- renderPlot({
        ggplot(mtcars, aes(x = am, y = mpg)) +
            geom_boxplot(fill = semantic_palette[["green"]]) + 
            xlab("gearbox") + ylab("Miles per gallon")
    })
    
    # plotly
    colscale <- c(semantic_palette[["red"]], semantic_palette[["green"]], semantic_palette[["blue"]])
    output$dotplot1 <- renderPlotly({
        ggplotly(ggplot(mtcars, aes(wt, mpg))
                 + geom_point(aes(colour=factor(cyl), size = qsec))
                 + scale_colour_manual(values = colscale)
        )
    })
    
    # DT
    output$carstable <- renderDataTable(mtcars)
    
    # leaflet
    crime_data <- fread("crime data.csv")
    
    observeEvent(input$dd1, {
        data <- crime_data[description %like% input$dd1,]
        if (input$dd1 != "ALL") data <- data[description %like% input$dd1,]

        sf <- st_as_sf(data, coords = c("longitude", "latitude"))
        
        m <- leaflet(data = sf) %>% addTiles() %>%
            addCircleMarkers(
                popup = ~ as.character(location),
                label = ~ as.character(description),
                radius = 4,
                clusterOptions = markerClusterOptions()
            )
        
        output$map <- renderLeaflet(m)
    })
})

shinyApp(ui, server)
