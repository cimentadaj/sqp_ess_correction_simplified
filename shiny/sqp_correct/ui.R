#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

toy_df <- as.data.frame(replicate(10, rnorm(100)))

# Define UI for application that draws a histogram
ui <- 
  # mainPanel(
  #   tabsetPanel(
  #     tabPanel("Define sum scores",
  #         actionButton('insertBtn', 'Insert new sum score'),
  #         tags$div(id = 'placeholder'),
  #         actionButton('whatever', 'Define model')
  #       )
  #     )
  #   )
  
  fluidPage(
    navlistPanel(id = "menu",
      tabPanel("Create sum scores",
               actionButton('ins_sscore', 'Insert new sum score'),
               tags$div(id = 'placeholder'),
               actionButton('def_model', "I'm done, I want to define my model")
      ),
      tabPanel("Define the model", value = "def_model")
    )
  )
