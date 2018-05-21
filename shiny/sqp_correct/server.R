#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


all_variables <- paste0("V", 1:9)
toy_df <- as.data.frame(replicate(10, rnorm(100)))

server <- function(input, output, session) {
  
  observeEvent(input$ins_sscore, {
    # When user clicks insert, add
    # the sscore name and variables to the ui
    whole_html <-
      splitLayout(
          textInput(paste0("ssname", input$insertBtn), "Name of sum score"),
          selectInput(paste0("sscore", input$insertBtn),
                      'Variables that compose the sum score',
                      choices = all_variables, multiple = TRUE),
        cellWidths = c("17%", "83%")
      )
    # Interactively add a sumscore to the UI
    insertUI(selector = '#placeholder', ui = whole_html)
  })
  
  
  # When the sumscores are ready, the user clicks
  # define model and we switch to the define model
  # tab to select dependent and independent variables
  observeEvent(input$def_model, {
    updateNavlistPanel(session,
                       inputId = "menu",
                       selected = "def_model")
    
    
  })
  
  # observe({
  #   print(input$sscore1)
  #   print(input$sscore2)
  # })
}


# runApp("./shiny/sqp_correct/")