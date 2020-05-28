#https://shiny.rstudio.com/articles/layout-guide.html LAYOUT
#https://shiny.rstudio.com/articles/persistent-data-storage.html PERSISTENT STORAGE
#> https://stat.ethz.ch/pipermail/r-help/2017-June/447450.html This is for styling RMarkdown file

library(shiny)
library(DT) 
library(ggplot2)  
library(stringi)
library(gtools)

#this will select commas outside of parenthesis ,(?![^(]*\))
##### FORM VARIABLES #####
fields <- c("Name", "Affiliation", "Data Source Name", "Credentials", "Skills", "Jobs", "Employers", "STW Relevant", 
            "Dataset Name", "Dataset Link", "Subject", "Organization", "Data Type", "Purpose", "Audience", "Population Coverage", 
            "Unit of Analysis", "Geographic Unit", "Time Coverage", "Collection Frequency", "When does the data become available?", "Can this data be trended?", 
            "Methodology Report Link", "Data Dictionary Link", "Data Quality Assessments", "Cost/Price", "Funding amount to support R&D", "Licensing or Training Required?", 
            "Accessbility", "Data Format", "Individuals Identifiable", "Gender", "Race/Ethnicity", "Persons Who Live on Tribals Lands", "Veterans", "Active Military", "Persons Who Live on Tribal Lands", 
            "Fields of Study/Types of Training", "Types of Employment/Occupations", "Notes")

field_list <- c(fields, "submit_time")


##### FORM RESPONSES DIRECTORY #####
#dir.create("form")
outputDir <- "/home/sm9dv/shiny-stw/test-shiny/form"

##### FUNCTIONS FOR SAVING AND DISPLAYING FORM INPUTS #####
saveData <- function(input) {
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  
  
  
  
  
  
  
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data[[x]] <- list(var)
    } else {
      # all other data types
      data[[x]] <- var
    }
  }
  data$submit_time <- date()
  
  # Create a unique file name
  fileName <- sprintf(
    "%s_%s.rds", 
    as.integer(Sys.time()), 
    digest::digest(data)
  )
  
  # Write the file to the local system
  saveRDS(
    object = data,
    file = file.path(outputDir, fileName)
  )
}

loadData <- function() {
  # read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
     

    

    field_list <- c(fields, "submit_time")
    data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(data) <- field_list
  } else {
    data <- lapply(files, function(x) readRDS(x)) 
    
    # Concatenate all data together into one data.frame
    data <- do.call(smartbind, data)
  }
  
  data
}



##### UI #####
ui <- fluidPage(
  
  
  title = "Data Discovery",
  titlePanel(
    fluidRow(
      column(3, img(height = 51.65, width = 243.3, src = "BII.jpg")),
      column(9, h1("Data Discovery - Skilled Technical Workforce", style = "font-weight: bold; font-size: 30pt;"))
    )
  ),
  
  
  #this puts the side bar visible for all tabs
  #sidebarLayout(
  # sidebarPanel(
  
  
  #  checkboxGroupInput("show_vars", "Columns in Datasets to show:",choiceNames=gsub(names(responses),pattern="\\.",replacement=" "), choiceValues=names(responses),selected = names(responses)) 
  # ,width=2),
  
  mainPanel(
      
      tabPanel("Form", DT::dataTableOutput("form", width = 300), tags$hr(),
               fluidRow( column(4, textInput("Name", "Name", ""), 
                                textInput("Affiliation", "Affiliation", ""),
                                textInput("Data Source Name", "Data Source Name", ""),
                                radioButtons("Credentials", "Credentials", choices = list("Yes", "No")),
                                radioButtons("Skills", "Skills", choices = list("Yes", "No")),
                                radioButtons("Jobs", "Jobs", choices = list("Yes", "No")),
                                radioButtons("Employers", "Employers", choices = list("Yes", "No")),
                                textAreaInput("STW Relevant", "STW Relevant", ""),
                                textInput("Dataset Name", "Dataset Name", ""), 
                                textInput("Dataset Link", "Dataset Link", ""), 
                                checkboxGroupInput("Subject", 
                                                   "Subject", 
                                                   choices = list("Education/Training", "Licenses/Certifications", 
                                                                  "Jobs/Employment", "Industry"
                                                   )),
                                radioButtons("Organization", "Organization", choices = list("Non-Profit", "Federal", 
                                                                                            "For-Profit")),
                                checkboxGroupInput("Data Type", "Data Type", choices = list("Administrative", "Opportunity", 
                                                                                            "Procedural", "Designed (Survey)"))),
                         column(4,  textAreaInput("Purpose", "Purpose", ""), 
                                textInput("Audience", "Audience", ""), 
                                textInput("Population Coverage", "Population Coverage", ""),
                                textInput("Unit of Analysis", "Unit of Analysis", ""), 
                                checkboxGroupInput("Geographic Unit", "Geographic Unit", choices = list("National", "State", "City", "Zip Code", "Census Block", "Census Tract")), 
                                textInput("Time Coverage", "Time Coverage", ""),
                                radioButtons("Collection Frequency", "Collection Frequency", choices = list("Annual", "Monthly", "Biennial", "Daily", "Real-time", "Quarterly", "One-time")), 
                                textInput("When does the data become available?", "When does the data become available?", ""),
                                radioButtons("Can this data be trended", "Can this data be trended?", choices = list("Yes", "No")), 
                                textInput("Methodology Report Link", "Methodology Report Link", ""), 
                                textInput("Data Dictionary Link", "Data Dictionary Link", ""), 
                                textAreaInput("Data Quality Assessments", "Data Quality Assessments", ""), 
                                textInput("Cost/Price", "Cost/Price", ""), 
                                textInput("Funding amount to support R&D", "Funding amount to support R&D", ""), 
                                radioButtons("Licensing or Training Required?", "Licensing or Training Required?", choices = list("Yes", "No"))), 
                         column(4, 
                                checkboxGroupInput("Accessbility", "Accessibility", choices = list("API", "Download", "FTP", "Portal", "Webscraping")), 
                                checkboxGroupInput("Data Format", "Data Format", choices = list("CSV", "Excel", "TXT", "PDF", "JSON", "SAS", "R", "SPSS")), 
                                radioButtons("Individuals Identifiable", "Individuals Identifiable", choices = list("Yes", "No")), 
                                radioButtons("Gender", "Gender", choices = list("Yes", "No")), 
                                radioButtons("Race/Ethnicity", "Race/Ethnicity", choices = list("Yes", "No")), 
                                radioButtons("Persons with Disabilities", "Persons with Disabilities", choices = list("Yes", "No")), 
                                radioButtons("Veterans", "Veterans", choices = list("Yes", "No")), 
                                radioButtons("Active Military", "Active Military", choices = list("Yes", "No")), 
                                radioButtons("Persons Who Live on Tribals Lands", "Persons Who Live on Tribal Lands", choices = list("Yes", "No")), 
                                radioButtons("Fields of Study/Types of Training", "Fields of Study/Types of Training", choices = list("Yes", "No")), 
                                radioButtons("Types of Employment/Occupations", "Types of Employment/Occupations", choices = list("Yes", "No")))),
               textAreaInput("Notes", "Notes", ""),
               actionButton("submit", "Submit", style="border-color: #F17E1D; font-size: 20px; padding: 16px 16px;"))
  ))
    



##### SERVER #####
server <- function(input, output, session) {
  
  ##### FORM DATA #####

  
  
  
  
  formData <- reactive({
    data <- sapply(field_list, function(x) input[[x]])
    data
  })
  

  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  output$form <- renderDataTable({
    # update with current response when Submit or Delete are clicked
    input$submit 
    
    loadData()
  })
  
  
  
  
}

shinyApp(ui, server)

