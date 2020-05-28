library(shiny)
library(semantic.dashboard)
library(ggplot2)
library(plotly)
library(leaflet)

ui <- dashboardPage(
    dashboardHeader(color = "blue", title = "Dashboard Demo", inverted = TRUE),
    dashboardSidebar(
        size = "thin", color = "teal",
        sidebarMenu(
            menuItem(tabName = "thingy1", "Main thingy"),
            menuItem(tabName = "data", "Data")
        )
    ),
    dashboardBody(
        tabItems(
            selected = 1,
            tabItem(
                tabName = "thingy1",
                fluidRow(
                    h3("main")
                    )
            ),
            tabItem(
                tabName = "data",
                fluidRow(
                    h1("data")
                )
            )
        )
    )
)

server <- shinyServer(function(input, output, session) {
    
})

shinyApp(ui, server)
