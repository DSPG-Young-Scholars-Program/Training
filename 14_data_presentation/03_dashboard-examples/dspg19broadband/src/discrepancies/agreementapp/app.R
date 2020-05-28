library(shiny)

int_alaska <- readRDS("./data/alaska.Rds")
int_hawaii <- readRDS("./data/hawaii.Rds")
int_contig <- readRDS("./data/contig.Rds")

library(ggplot2)
library(ggthemes)
library(sf)

# User interface
ui <- fluidPage(
  tags$style(type  ="text/css",
             ".recalculating {opacity: 1.0;}"
  ),
  
  tags$head(tags$style(HTML(" .sidebar { font-size: 50%; } "))),
  
  titlePanel("ACS and FCC Broadband Subscription Estimate Congruence by Tract"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(width = 5,
      h3("Input", align = "left"),
      p(), 
      radioButtons("intervaltype", "Overlap Type", 
                   choices = list("ACS within [min FCC 200kbps, max FCC 200kbps]", 
                               "ACS within [min FCC 10mbps, max FCC 10mbps]",
                               "ACS within [min FCC 10mbps, max FCC 200kbps]"), 
                   selected = "ACS within [min FCC 200kbps, max FCC 200kbps]"),
      "Notes:",
      br(),
      "FCC = Federal Communications Commission, December 2015.",
      br(),
      "ACS = American Community Survey, 2013-17.",
      br(),
      "Alaska and Hawaii not to scale."
      ),
    mainPanel(
      h3("Map", align = "left"),
      textOutput("selected_vars"),
      p(),
      plotOutput("map", width = "800px")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$selected_vars <- renderText({ 
    paste("The map shows", input$intervaltype, "with urbanicity.")
  })

  output$map <- renderPlot({
  
  inttype_hawaii <- switch(input$intervaltype,
                    "ACS within [min FCC 200kbps, max FCC 200kbps]" = int_hawaii$urban_fcc200,
                    "ACS within [min FCC 10mbps, max FCC 10mbps]" = int_hawaii$urban_fcc10,
                    "ACS within [min FCC 10mbps, max FCC 200kbps]" = int_hawaii$urban_any)
  
  inttype_alaska <- switch(input$intervaltype,
                           "ACS within [min FCC 200kbps, max FCC 200kbps]" = int_alaska$urban_fcc200,
                           "ACS within [min FCC 10mbps, max FCC 10mbps]" = int_alaska$urban_fcc10,
                           "ACS within [min FCC 10mbps, max FCC 200kbps]" = int_alaska$urban_any)
  
  inttype_contig <- switch(input$intervaltype,
                           "ACS within [min FCC 200kbps, max FCC 200kbps]" = int_contig$urban_fcc200,
                           "ACS within [min FCC 10mbps, max FCC 10mbps]" = int_contig$urban_fcc10,
                           "ACS within [min FCC 10mbps, max FCC 200kbps]" = int_contig$urban_any)
  
  
    plot_main <- ggplot() +
      geom_sf(data = int_contig, aes(fill = inttype_contig), size = 0.001) +
      theme_map() +
      coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) +
      labs(title = "ACS and FCC Broadband Subscription Estimate Congruence by Tract",
           subtitle = "Tracts with incongruent estimate ranges shown in grey.") +
      scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
      theme(plot.title = element_text(size = 16, face = "bold"),
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 10),
            legend.position = "top")

    # Plot Hawaii
    plot_hawaii <- ggplot() +
      geom_sf(data = int_hawaii, aes(fill = inttype_hawaii), size = 0.001)  +
      theme_map() +
      coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
      scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
      theme(legend.position = "none")

    # Plot Alaska
    plot_alaska <- ggplot() +
      geom_sf(data = int_alaska, aes(fill = inttype_alaska), size = 0.001) +
      theme_map() +
      coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE) +
      scale_fill_manual(name = "Urbanicity", values = c("#fed98e", "#fe9929", "#d95f0e", "#993404"), na.value = "#f0f0f0") +
      theme(legend.position = "none")

    # Plot all
    plot_main +
      annotation_custom(grob = ggplotGrob(plot_alaska),
                        xmin = -3350000,
                        xmax = -3350000 + (1600000 - (-2400000))/1.8,
                        ymin = -2450000,
                        ymax = -2450000 + (2500000 - 200000)/1.8) +
      annotation_custom(grob = ggplotGrob(plot_hawaii),
                        xmin = -1700000,
                        xmax = -1700000 + (-154 - (-161))*230000,
                        ymin = -2450000,
                        ymax = -2450000 + (23 - 18)*230000)
  })
}


# App
shinyApp(ui = ui, server = server)