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
            menuItem(tabName = "main", "Main", icon = icon("car")),
            menuItem(tabName = "data", "Data", icon = icon("table"))
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
                    leafletOutput("map")
                )
            )
        )
    ), theme = "cerulean"
)

server <- shinyServer(function(input, output, session) {
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
})

shinyApp(ui, server)
