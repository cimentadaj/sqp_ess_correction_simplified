#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


all_variables <- paste0("var", 1:9)

server <- function(input, output) {
  
  observeEvent(input$insertBtn, {
    whole_html <-
      splitLayout(
          textInput(paste0("name", input$insertBtn), "Name of sum score"),
          selectInput(paste0("sscore", input$insertBtn),
                      'Variables that compose the sum score',
                      choices = all_variables, multiple = TRUE),
        cellWidths = c("17%", "83%")
      )
    insertUI(selector = '#placeholder', ui = whole_html)
  })
  
  observe({
    print(input$sscore1)
    print(input$sscore2)
  })
}


# runApp("./shiny/sqp_correct/")