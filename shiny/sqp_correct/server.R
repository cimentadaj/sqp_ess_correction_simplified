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
pos <- div("display: inline-block;vertical-align:top; width: 150px;")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  observeEvent(input$insertBtn, {
    
    whole_html <-
        splitLayout(
          textInput("txt1", "Name of sum score"),
          selectInput('selvar1', 'Create sum score', choices = all_variables, multiple = TRUE),
          cellWidths = c("20%", "80%")
        )

    # whole_html <-
    #   tagList(textInput("txt1", "Name of sum score"),
    #           selectInput('selvar1', 'Create sum score', choices = all_variables, multiple = TRUE))

    insertUI(selector = '#placeholder',
             ui = whole_html)
  })
})
