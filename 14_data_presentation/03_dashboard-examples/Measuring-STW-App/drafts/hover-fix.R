library(shiny)
library(DT) 
library(ggplot2)  
library(stringi)
library(gtools)
library(shinyBS)

#https://stackoverflow.com/questions/36132204/reactive-radiobuttons-with-tooltipbs-in-shiny  This is the solution to the issues


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



ui <- shinyUI(
  fluidPage(
    fluidRow(
      column(3,
             checkboxGroupInput("show_vars", "Columns in Data Sources to show:",
                                choices = names(responses),
                                selected=names(responses))
      ),
      radioTooltip(id = "show_vars", choice = "Data Source Name", title = "Button 1 Explanation", placement = "right", trigger = "hover"),
      radioTooltip(id = "show_vars", choice = "Skills", title = "Button 2 Explanation", placement = "right", trigger = "hover"),
      radioTooltip(id = "show_vars", choice = "Jobs", title = "Button 3 Explanation", placement = "right", trigger = "hover"),
      column(9, mainPanel( DT::dataTableOutput("mytable1")))
    ) 
  )
)

server <- function(input, output, session) {
  

    
    output$mytable1 <- DT::renderDataTable({
      DT::datatable(responses[, input$show_vars, drop = FALSE], extensions = 'Buttons', filter = "top",
                    options = list(buttons = list(list(extend='csv',
                                                       filename = 'STW-Data-Discovery'),
                                                  list(extend='excel',
                                                       filename = 'STW-Data-Discovery')), dom="BlfrtipS",iDisplayLength=-1,fixedColumns = TRUE))
    })
    
  
}
shinyApp(ui = ui, server = server)