##### These are helpful links I used to create the shiny app ##### 
#https://shiny.rstudio.com/articles/layout-guide.html LAYOUT
#https://shiny.rstudio.com/articles/persistent-data-storage.html PERSISTENT STORAGE
#https://stat.ethz.ch/pipermail/r-help/2017-June/447450.html This is for styling RMarkdown file
#https://gupsych.github.io/tquant/data-input.html This is the solution for the saving checkbox group issue
#https://cran.r-project.org/web/packages/vcd/vignettes/strucplot.pdf this is helpful for mosaic plots
#https://stackoverflow.com/questions/36132204/reactive-radiobuttons-with-tooltipbs-in-shiny  This is the solution to the checkbox individual id issues


# You should un-comment the following and create two folders using the code below to store the feedback and form data.
# I re-comment this after running so that I only create the folder once.

#dir.create("feedback")
#dir.create("form")

#if you do not have these packages, use install.packages("")
library(shiny)
library(DT) 
library(ggplot2)  
library(stringi)
library(stringr)
library(gtools)
library(vcd)
library(tidyr)
library(dplyr)
library(shinythemes)


##### THEME COLORS #####
theme_Palette<-c("#1B3766", "#02ABD6", "#6DD4DB", "#A9D5A5", "#F17E1D", "#FDDA24", "#EF3F6B","#25CAD3","#009FDF","#FDBC00", "#62BB46")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col=col, radius=radius)
wheel(theme_Palette)

##### DATA #####
#responses <- read.csv("https://raw.githubusercontent.com/uva-bi-sdad/Measuring-STW-App/sarah/data-discovery-feb-3.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")
responses <- read.csv("data-discovery-feb-3.csv", sep = ",",stringsAsFactors = FALSE, header=TRUE, encoding="UTF-8")
responses<-responses[!apply(responses == "", 1, all),] #remove empty rows
names(responses) <- stri_trim(gsub("..Yes.No.|i\\.e\\..+or\\.|i\\.e\\..+|\\.{53}.+|\\.+", " ", names(responses)), side = "right")

# Expand Responses for Visualization of Checkbox Group Items #
expand_responses <- responses
expand_responses[["Subject"]] <- trimws(expand_responses[["Subject"]])
expand_responses[["Subject"]] <- as.list(str_split(expand_responses[["Subject"]], ", "))
expand_responses <- expand_responses %>% unnest("Subject") %>% group_by(`Data Source Name`, `Dataset Name`)
expand_responses <- as.data.frame(expand_responses)

expand_responses[["Data Type"]] <- trimws(expand_responses[["Data Type"]])
expand_responses[["Data Type"]] <- as.list(str_split(expand_responses[["Data Type"]], ", "))
expand_responses <- expand_responses %>% unnest("Data Type") %>% group_by(`Data Source Name`, `Dataset Name`)
expand_responses <- as.data.frame(expand_responses)

##### FORM VARIABLES #####
outputDir <- "form"
fields <- c("Name", "Affiliation", "Data Source Name", "Credentials", "Skills", "Jobs", "Employers", "STW Relevant", 
                 "Dataset Name", "Dataset Link", "Subject", "Organization", "Data Type", "Purpose", "Audience", "Population Coverage", 
                 "Unit of Analysis", "Geographic Unit", "Time Coverage", "Collection Frequency", "When does the data become available?", "Can this data be trended?", 
                 "Methodology Report Link", "Data Dictionary Link", "Data Quality Assessments", "Cost/Price", "Funding amount to support R&D", "Licensing or Training Required?", 
                 "Accessibility", "Data Format", "Individuals Identifiable", "Gender", "Race/Ethnicity", "Veterans", "Active Military", "Persons Who Live on Tribal Lands", 
                 "Fields of Study/Types of Training", "Types of Employment/Occupations", "Notes")
field_list <- c(fields, "submit_time")

##### FEEDBACK VARIABLES #####
outputDir.feedback <- "feedback"
fields.feedback <-c("Name.feedback", "Email.feedback", "Comment.feedback")
field_feedback_list <- c(fields.feedback, "submit_time")


##### FUNCTIONS FOR SAVING AND DISPLAYING FORM INPUTS (see saving checkboxs link) #####
# This saves the form input data as and .rds object, because CSV does not read lists that are created with the checkbox items
saveData <- function(input) {
  data <- data.frame(matrix(nrow=1,ncol=0))
  
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      
      data[[x]] <- list(var)
    } else {
      data[[x]] <- var
    }
  }
  
  data$submit_time <- date()
  

  fileName <- sprintf(
    "%s_%s.rds", 
    as.integer(Sys.time()), 
    digest::digest(data)
  )
    saveRDS(
    object = data,
    file = file.path(outputDir, fileName)
  )
}

loadData <- function() {
  files <- list.files(outputDir, full.names = TRUE)
  
  if (length(files) == 0) {
   
    field_list <- c(fields, "submit_time")
    data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(data) <- field_list
  } else {
    data <- lapply(files, function(x) readRDS(x)) 
    
    data <- do.call(smartbind, data)
  }
  
  data
}


##### FUNCTIONS FOR SAVING FEEDBACK INPUTS #####
saveData.feedback <- function(data) {
  data <- t(data)
  names(data) <- fields.feedback
  data$submit_time <- date()
  
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  write.csv(
    x = data,
    file = file.path(outputDir.feedback, fileName), 
    row.names = FALSE, 
    quote = FALSE
  )
}

##### This function allows tooltips for checkbox items on the data sources tab (see stackoverflow link for further information)#####
radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
                                                 $(document).ready(function() {
                                                 setTimeout(function() {
                                                 $('input', $('#", id, "')).each(function(){
                                                 if(this.getAttribute('value') == '", choice, "') {
                                                 opts = $.extend(", options, ", {html: true});
                                                 $(this.parentElement).tooltip('destroy');
                                                 $(this.parentElement).tooltip(opts);
                                                 }
                                                 })
                                                 }, 500)
                                                 });
                                                 ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

##### UI #####
ui <- fluidPage(
 #theme = shinytheme("cosmo"),

  HTML('<script src="//use.typekit.net/tgy5tlj.js"></script>'),
    
    HTML('<script>try{Typekit.load();}catch(e){}</script>'),
  
  
  theme = "theme.css",
  #shinythemes::themeSelector(),
 
title = "Data Discovery",

# https://stackoverflow.com/questions/56407601/how-to-fix-the-position-of-main-panel-in-r-shiny this almost fixes the title Panel

titlePanel(fluidRow(
      column(3, img(width = '60%', height = '20%', src = "BII.jpg")),
      column(6, "Data Discovery: Skilled Technical Workforce", align = "center"),
      column(3, img(src='NCSES-full-color.pdf'))
    )),



hr(),
  #mainPanel(

    tabsetPanel(
      id = 'dataset',
      tabPanel( "About", includeMarkdown("https://raw.githubusercontent.com/uva-bi-sdad/Measuring-STW-App/sarah/welcome-page.Rmd")), # you will need to include the path to the "welcome-page.Rmd"
      tabPanel("Data Sources",
               sidebarPanel(
                 checkboxGroupInput("show_vars", "Columns to Show:", 
                                    choiceNames=names(responses), 
                                    choiceValues = names(responses),
                                    selected=names(responses)), 
                 width=2), 
               # the following lines create the hover tooltip for the checkboxes on the Data Sources tab.
                     radioTooltip(id = "show_vars", choice = "Data Source Name", title = " An organization collecting the data and link to the organization.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Credentials", title = "Indicates if the dataset includes information on credentials.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Skills", title = "Indicates if the dataset includes information on skills.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Jobs", title = "Indicates if the dataset includes information on jobs.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Employers", title = "Indicates if the dataset includes information on employers.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "STW Relevant", title = "A description of how the data source can be used to describe the STW.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Dataset Name", title = "An organization’s name for the dataset and link to the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Dataset Link", title = "A link, if available, to the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Subject", title = "The main topic(s) addressed in the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Organization Type", title = "Indicates if the organization providing the dataset is for-profit, non-profit, or federal.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Data Type", title = "Describes the data by broad method(s) used for collection.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Purpose", title = "A brief explanation of the purpose and history of the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Audience", title = "Lists the main users of the dataset, or for whom the dataset was compiled.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Population Coverage", title = "Description of the target population and observations in the sample or population.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Unit of Analysis", title = "The smallest unit in the dataset on which information is provided.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Geographic Unit", title = "A list of all geographic variables contained in the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Time Coverage", title = "The start and end date of the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Collection Frequency", title = "Indicates how often the data is collected.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "When does the data become available", title = "Indicates how frequently new data are made available.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Can the data be trended", title = "Indicates if the dataset can be trended.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Methodology Report Link", title = "A link, if available, to the methodology report for the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Data Dictionary Link", title = "A link, if available, to the data dictionary for the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Data Quality Assessments", title = "A description including biases and issues of the dataset and if the dataset can be benchmarked against an existing survey.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Cost Price", title = "Lists any fees for using the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Funding amount to support R D", title = "Lists any funding provided by the organization for uses of the dataset that support R&D.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Licensing or Training Required", title = "Indicates if licensing or training is required for use of the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Accessibility", title = "Method(s) of acquiring data.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Data Format", title = "The format(s) in which the data is available.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Individuals Identifiable", title = "Indicates if personally identifiable information is included in the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Gender", title = " Indicates if information on gender is included in the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Race Ethnicity", title = "Indicates if information on race or ethnicity is included in the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Persons with Disabilities", title = "Indicates if information on persons with disabilities is included in the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Veterans", title = "Indicates if information on veterans is included in the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Active military and their families", title = "Indicates if information on active military is included in the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Persons who live on tribal lands", title = "Indicates if information on tribal lands is included in the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Fields of Study", title = "Indicates if information on field of study or types of training is included in the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Types of Employment or Occupations", title = "Indicates if information on employment or occupations is included in the dataset.", placement = "right", trigger = "hover"),
                     radioTooltip(id = "show_vars", choice = "Notes", title = "Any additional information relevant to the data source.", placement = "right", trigger = "hover"),
                mainPanel( DT::dataTableOutput("mytable1"), width = 10)), 
      tabPanel( "Plot", 
             fluidRow(
               column(2, uiOutput("filter_vars"),
                      uiOutput("select_vars")
               ), column(10,  uiOutput("plot")
                 
               )
             )),  

      tabPanel("Dictionary", includeMarkdown("https://raw.githubusercontent.com/uva-bi-sdad/Measuring-STW-App/sarah/data-dictionary.Rmd")), # you will need to include the path to the "data-dictionary.Rmd"
      tabPanel("Form", DT::dataTableOutput("form"), tags$hr(),
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
                                checkboxGroupInput("Accessibility", "Accessibility", choices = list("API", "Download", "FTP", "Portal", "Webscraping")), 
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
               actionButton("submit", "Submit", style="border-color: #F17E1D; font-size: 20px; padding: 16px 16px;")), 
      tabPanel("Contact", 
               textInput("Name.feedback", "Name", ""), textInput("Email.feedback", "Email", ""), 
               textAreaInput("Comment.feedback", "Comment", ""),
               actionButton("submit.feedback", "Submit", style="border-color: #F17E1D; font-size: 20px; padding: 16px 16px;"))
    )
  )
#)



##### SERVER #####
server <- function(input, output, session) {
  # This outputs the reactive data table on the Data Sources tab
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(responses[, input$show_vars, drop = FALSE], extensions = 'Buttons', filter = "top",
                   options = list(
                     
                     buttons = list(list(extend='csv',
                                                     filename = 'STW-Data-Discovery'),
                                                list(extend='excel',
                                                     filename = 'STW-Data-Discovery')), dom="BlfrtipS", iDisplayLength=-1, fixedColumns = TRUE))
  })

  #This allows filtering between one, two and three variables on the Plots tab
  output$filter_vars <- renderUI({
    radioButtons("rd","Select Option", choices = c("One Variable","Two Variables", "Three Variables"),
                 selected = "One Variable")
  })
  
  #Designates the variable choices on the drop down menus on the Plots tab
  output$select_vars <- renderUI({
    req(input$rd)
    if (input$rd == "One Variable") {fluidRow(column(width = 12,
      selectInput("category1", "Variable", choices=c("Credentials","Jobs", "Employers", "Skills", "Subject", "Organization Type", "Data Type", "Gender", "Race Ethnicity",  "Persons with Disabilities", "Veterans", "Active military and their families", "Persons who live on tribal lands", "Fields of Study", "Types of Employment or Occupations"))))
    }
    else if(input$rd == "Two Variables") {fluidRow(column(width = 12, 
      selectInput("category2", "Variable 1", choices=c("Credentials","Jobs", "Employers", "Skills", "Subject", "Organization Type", "Data Type", "Gender", "Race Ethnicity",  "Persons with Disabilities", "Veterans", "Active military and their families", "Persons who live on tribal lands", "Fields of Study", "Types of Employment or Occupations")),
      selectInput("category3", "Variable 2", choices=c("Credentials","Jobs", "Employers", "Skills", "Subject", "Organization Type", "Data Type", "Gender", "Race Ethnicity",  "Persons with Disabilities", "Veterans", "Active military and their families", "Persons who live on tribal lands", "Fields of Study", "Types of Employment or Occupations" ))))
    }
    else if(input$rd == "Three Variables"){fluidRow(column(width = 12, 
      selectInput("category4", "Variable 1", choices=c("Credentials","Jobs", "Employers", "Skills", "Subject", "Organization Type", "Data Type", "Gender", "Race Ethnicity",  "Persons with Disabilities", "Veterans", "Active military and their families", "Persons who live on tribal lands", "Fields of Study", "Types of Employment or Occupations" )),
      selectInput("category5", "Variable 2", choices=c("Credentials","Jobs", "Employers", "Skills", "Subject", "Organization Type", "Data Type", "Gender", "Race Ethnicity",  "Persons with Disabilities", "Veterans", "Active military and their families", "Persons who live on tribal lands", "Fields of Study", "Types of Employment or Occupations")),
      selectInput("category6", "Variable 3", choices=c("Credentials","Jobs", "Employers", "Skills", "Subject", "Organization Type", "Data Type", "Gender", "Race Ethnicity",  "Persons with Disabilities", "Veterans", "Active military and their families", "Persons who live on tribal lands", "Fields of Study", "Types of Employment or Occupations"))))
    }
  })
  
  # This displays the plots
  output$plot <- renderUI({
    
    req(input$rd)

    
    if(input$rd=="One Variable") {
      
        req(input$category1)

        if (input$category1=="Subject"|input$category1=="Data Type"){
          
          output$plot2<-renderPlot({
            
            ggplot(expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category1)]), ], aes(x = expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category1)]), input$category1] , 
                              fill =expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category1)]), input$category1]))+
              geom_bar()+
              geom_bar(width = 0.66) +
              scale_discrete_manual(theme_Palette[1],theme_Palette[2], theme_Palette[3],theme_Palette[4],
                                    theme_Palette[5],theme_Palette[6],theme_Palette[7],theme_Palette[8])+
              theme_minimal() +
              labs(title = paste("Data Sources Containing", input$category1), y = "Number of Sources", x = "") +
              theme(
                legend.position = "none", 
                plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                axis.text.x = element_text(size = 18, angle = 20),
                axis.text.y = element_text(size = 18), 
                axis.title.x = element_text(size = 18), 
                axis.title.y = element_text(size = 18))
          },  height = 600, width = 1000)
          
          
              plotOutput("plot2")
      
        } else {
          
          
          
          output$plot1<-renderPlot({
            
            ggplot(responses, aes(x =responses[ , input$category1], fill =responses[ , input$category1]))+ 
              scale_fill_manual(values = c(theme_Palette[1], theme_Palette[5], theme_Palette[4], theme_Palette[2]))+
              geom_bar(width = 0.66) +
              theme_minimal() +
              labs(title = paste("Data Sources Containing", input$category1), y = "Number of Sources", x = "") +
              theme(
                legend.position = "none", 
                plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                axis.text.x = element_text(size = 18),
                axis.text.y = element_text(size = 18), 
                axis.title.x = element_text(size = 18), 
                axis.title.y = element_text(size = 18))
          }, height = 600, width = 800)
            
              plotOutput("plot1")
        }
    } else if(input$rd=="Two Variables"){
      
    if((input$category2 == "Subject" & input$category3 != "Data Type")|(input$category2 == "Data Type" & input$category3 != "Subject")){

            output$plotb<-renderPlot({ 
        
              ggplot(expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category2)]), ], 
                     aes(x = expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category2)]), input$category2], 
                         fill = expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category2)]), input$category3]))+
                geom_bar()+
                geom_bar(width = 0.66) +
                theme_minimal() +
                labs(title = paste("Data Sources Containing", input$category3, "by", input$category2), y = "Number of Sources", x = paste(input$category2), fill = paste(input$category3)) +
                theme(
                plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                axis.text.x = element_text(size = 18, angle = 30),
                axis.text.y = element_text(size = 18), 
                axis.title.x = element_text(size = 18), 
                axis.title.y = element_text(size = 18))
            }, height = 600, width = 800)
              plotOutput("plotb")
         
      } else if ((input$category2 != "Subject" & input$category3 == "Data Type")|(input$category2 != "Data Type" & input$category3 == "Subject")){
        
            output$plota<-renderPlot({ 
              
              ggplot(expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category3)]), ], 
                     aes(x = expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category3)]), input$category2], 
                     fill =expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category3)]), input$category3]))+
                geom_bar()+
                geom_bar(width = 0.66) +
                theme_minimal() +
                labs(title = paste("Data Sources Containing", input$category3, "by", input$category2), y = "Number of Sources", x = paste(input$category2), fill = paste(input$category3)) +
                theme(
                  plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                  axis.text.x = element_text(size = 18, angle = 30),
                  axis.text.y = element_text(size = 18), 
                  axis.title.x = element_text(size = 18), 
                  axis.title.y = element_text(size = 18))
              }, height = 600, width = 800)
                plotOutput("plota")
      } else if ((input$category2 == "Subject" & input$category3 == "Data Type")|(input$category2 == "Data Type" & input$category3 == "Subject")){
          
            
            output$plotc<-renderPlot({
              
              ggplot(expand_responses, aes(x =expand_responses[ , input$category2], fill =expand_responses[ , input$category3]))+ 
                geom_bar(width = .66) +
                theme_minimal() +
                labs(title = paste("Data Sources Containing", input$category3, "Data by", input$category2), y = "Number of Sources", x = paste(input$category2), fill = paste(input$category3) ) +
                theme(
                  plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                  axis.text.x = element_text(size = 18, angle = 30),
                  axis.text.y = element_text(size = 18), 
                  axis.title.x = element_text(size = 18), 
                  axis.title.y = element_text(size = 18))
            }, height = 600, width = 800)
              plotOutput("plotc")
      } else {
      
            output$plot3<-renderPlot({
              
              ggplot(responses, aes(x =responses[ , input$category2], fill =responses[ , input$category3]))+ 
                scale_fill_manual(values = c(theme_Palette[1], theme_Palette[5], theme_Palette[4], theme_Palette[2]))+
                geom_bar(width = .66) +
                theme_minimal() +
                labs(title = paste("Data Sources Containing", input$category3, "Data by", input$category2), y = "Number of Sources", x = paste(input$category2), fill = paste(input$category3) ) +
                theme(
                  plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                  axis.text.x = element_text(size = 18),
                  axis.text.y = element_text(size = 18), 
                  axis.title.x = element_text(size = 18), 
                  axis.title.y = element_text(size = 18))
            }, height = 600, width = 800)
              plotOutput("plot3")
        }
    } else if(input$rd=="Three Variables"){
          

          if((length(unique(responses[[input$category4]]))==2) &
             (length(unique(responses[[input$category5]]))==2) & (length(unique(responses[[input$category6]]))==2)){  
               
            output$plot4<-renderPlot({
  
                  var1  <- responses[ , input$category4]
                  var2 <- responses[ , input$category5]
                  var3 <- responses[ , input$category6]  
    
    
                  mosaic(xtabs(~ var1 + var2 + var3  ), data = responses, margin = c(3, 10, 2, 10),
                           shade = T,  gp = gpar(fill = c("#72dbc7", "#58a0a6", "#58a0a6", "#3c6a86", "#58a0a6", "#3c6a86", "#3c6a86", "#1b3766")), 
                           main = paste("Data Containing ", input$category4, ", ", input$category5, ", and ", input$category6, sep = ""),
                           labeling_args = list(set_varnames = c(var1 = input$category4, var2 = input$category5, var3 = input$category6), 
                                                rot_labels = c(0, 0, 90), offset_varnames = c(0,1,0, 1)
                           ))
          }, height = 600, width = 800)
            plotOutput("plot4")
          } else if((input$category4== "Data Type" & input$category5 != "Subject" & input$category6 != "Subject")|
                    (input$category4== "Subject" & input$category5 != "Data Type" & input$category6 != "Data Type")){
                      
            output$plotd<-renderPlot({ 
              
              ggplot(expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category4)]), ], 
                     aes(x = expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category4)]), input$category5], 
                         fill =expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category4)]), input$category6]))+
                geom_bar()+
                facet_grid(~expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category4)]), input$category4])+
                geom_bar(width = 0.66) +
                theme_minimal() +
                labs(title = paste("Data Sources Containing", input$category3, "by", input$category2), y = "Number of Sources", x = paste(input$category2), fill = paste(input$category3)) +
                theme(
                  plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                  axis.text.x = element_text(size = 18, angle = 30),
                  axis.text.y = element_text(size = 18), 
                  axis.title.x = element_text(size = 18), 
                  axis.title.y = element_text(size = 18))
            }, height = 600, width = 800)
            plotOutput("plotd")   
                      
                    
                      
          } else if((input$category5== "Data Type" & input$category4 != "Subject" & input$category6 != "Subject") |
                    (input$category5== "Subject"& input$category4 != "Data Type" & input$category6 != "Data Type")){
            
            output$plote<-renderPlot({ 
              
              ggplot(expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category5)]), ], 
                     aes(x = expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category5)]), input$category6], 
                         fill =expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category5)]), input$category4]))+
                geom_bar()+
                facet_grid(~expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category5)]), input$category5])+
                geom_bar(width = 0.66) +
                theme_minimal() +
                labs(title = paste("Data Sources Containing", input$category3, "by", input$category2), y = "Number of Sources", x = paste(input$category2), fill = paste(input$category3)) +
                theme(
                  plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                  axis.text.x = element_text(size = 18, angle = 30),
                  axis.text.y = element_text(size = 18), 
                  axis.title.x = element_text(size = 18), 
                  axis.title.y = element_text(size = 18))
            }, height = 600, width = 800)
            plotOutput("plote")  
            
            
            
            
          } else if( (input$category6== "Data Type" & input$category5 != "Subject" & input$category4 != "Subject") |
                     (input$category6== "Subject" & input$category5 != "Data Type" & input$category4 != "Data Type")){
            
            output$plote<-renderPlot({ 
              
              ggplot(expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category6)]), ], 
                     aes(x = expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category6)]), input$category4], 
                         fill =expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category6)]), input$category5]))+
                geom_bar()+
                facet_grid(~expand_responses[!duplicated(expand_responses[,c('Dataset Name', input$category6)]), input$category6])+
                geom_bar(width = 0.66) +
                theme_minimal() +
                labs(title = paste("Data Sources Containing", input$category3, "by", input$category2), y = "Number of Sources", x = paste(input$category2), fill = paste(input$category3)) +
                theme(
                  plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                  axis.text.x = element_text(size = 18, angle = 30),
                  axis.text.y = element_text(size = 18), 
                  axis.title.x = element_text(size = 18), 
                  axis.title.y = element_text(size = 18))
            }, height = 600, width = 800)
            plotOutput("plote") 
            
            
            
            
            
            
          }   else if ((input$category4== "Data Type" & input$category5 == "Subject")|
                       (input$category4== "Data Type" & input$category5 == "Data Type")|
                       (input$category4== "Subject" & input$category5 == "Data Type")|
                       (input$category4== "Subject" & input$category5 == "Subject")|
                       (input$category5== "Data Type" & input$category6 == "Subject")|
                       (input$category5== "Data Type" & input$category6 == "Data Type")|
                       (input$category5== "Subject" & input$category6 == "Data Type")|
                       (input$category5== "Subject" & input$category6 == "Subject")|
                       (input$category6== "Data Type" & input$category4 == "Subject")|
                       (input$category6== "Data Type" & input$category4 == "Data Type")|
                       (input$category6== "Subject" & input$category4 == "Data Type")|
                       (input$category6== "Subject" & input$category4 == "Subject")
          ){
            
            
            output$plotf<-renderPlot({ 
              
              ggplot(expand_responses, 
                     aes(x = expand_responses[ , input$category4], 
                         fill =expand_responses[, input$category5]))+
                geom_bar()+
                facet_grid(~expand_responses[, input$category6])+
                geom_bar(width = 0.66) +
                theme_minimal() +
                labs(title = paste("Data Sources Containing", input$category3, "by", input$category2), y = "Number of Sources", x = paste(input$category2), fill = paste(input$category3)) +
                theme(
                  plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                  axis.text.x = element_text(size = 16, angle = 30),
                  axis.text.y = element_text(size = 16), 
                  axis.title.x = element_text(size = 18), 
                  axis.title.y = element_text(size = 18),
                  legend.position = "bottom")
            }, height = 800, width = 1000)
            plotOutput("plotf") 
            
          } else{
            output$plotg<-renderPlot({ 
              
              ggplot(responses, 
                     aes(x = responses[ , input$category4], 
                         fill =responses[, input$category5]))+
                geom_bar()+
                facet_grid(~responses[, input$category6])+
                geom_bar(width = 0.66) +
                theme_minimal() +
                labs(title = paste("Data Sources Containing", input$category3, "by", input$category2), y = "Number of Sources", x = paste(input$category2), fill = paste(input$category3)) +
                theme(
                  plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                  axis.text.x = element_text(size = 18, angle = 30),
                  axis.text.y = element_text(size = 18), 
                  axis.title.x = element_text(size = 18), 
                  axis.title.y = element_text(size = 18))
            }, height = 600, width = 800)
            plotOutput("plotg")
     }
 
  }
})
  
  
  ##### FORM DATA #####
  #This displays the form entries
  
  formData <- reactive({
    data <- sapply(field_list, function(x) input[[x]])
    data
  })
  
  
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  output$form <- renderDataTable({
    input$submit 
    
    loadData()
  })
  
  
  ##### FEEDBACK DATA #####
  formData.feedback <- reactive({
    data <- sapply(fields.feedback, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit.feedback, {
    saveData.feedback(formData.feedback())
  })

  
  observeEvent(input$submit.feedback, {
    showModal(modalDialog(
      title = "",
      "Thank you for contacting us. We will return your email shortly."
    ))
  })
  
}

shinyApp(ui, server)
