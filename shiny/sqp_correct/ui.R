#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    navlistPanel(id = "menu", widths = c(2, 8),
      tabPanel("Create sum scores",
               actionButton('ins_sscore', 'Insert new sum score'),
               tags$div(id = 'placeholder'),
               actionButton('def_model', "I'm done, I want to define my model")
      ),
      tabPanel("Define the model", value = "def_model",
               selectInput("slid_cnt", "Pick a country", choices = all_countries),
               fluidRow(column(3, uiOutput("dv")),
                        column(3, uiOutput("iv")),
                        column(5, uiOutput("cmv"))),
               actionButton("calc_model", "Create model")),
      tabPanel("Create model", value = "cre_model")
    )
  )
