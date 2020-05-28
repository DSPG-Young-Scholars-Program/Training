library(shiny)
library(DT)
library(ggplot2)  # for the diamonds dataset
responses<-read.csv("Data Discovery NOV17.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")
ui <- fluidPage(
  
  #tags$head(
#    tags$style(HTML("

 #     label {

#      }

#    "))
 # ),
  
  
  
  title = "Data Discovery",
  titlePanel(
    fluidRow(
      column(3, img(height = 100, width = 150, src = "UVA centered.jpg")),
      column(9, "Data Discovery - Skilled Technical Workforce") 
      )
    ),
  
  
  sidebarLayout(
    sidebarPanel(
      

        checkboxGroupInput("show_vars", "Columns in Datasets to show:",choiceNames=gsub(names(responses),pattern="\\.",replacement=" "),choiceValues=names(responses),selected = names(responses))


    ,width=2),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("datasets", DT::dataTableOutput("mytable1"))
      )
    )
  )
)

server <- function(input, output) {

  responses<-read.csv("Data Discovery NOV17.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")
  # choose columns to display

  output$mytable1 <- DT::renderDataTable({
    DT::datatable(responses[, input$show_vars, drop = FALSE],  extensions = 'Buttons',
                  filter = "top",
                  options = list(buttons = list(list(extend='csv',
                                                     filename = 'STW-Data-Discovery'),
                                                list(extend='excel',
                                                     filename = 'STW-Data-Discovery')),dom="BlfrtipS",iDisplayLength=-1,fixedColumns = TRUE))
  })
  
  
}

shinyApp(ui, server)
