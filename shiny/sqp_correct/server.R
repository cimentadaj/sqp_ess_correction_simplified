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
toy_df <- as.data.frame(replicate(15, rpois(100, 10)))

server <- function(input, output, session) {
  
  # Because the toy_df (ESS data) must be fresh to all users
  # we leave it outside the session and create a subset that will
  # be unique per user here. This way the user can have a
  # non-conflicting copy of the data.
  cp_toydf <- toy_df[all_variables]
  
  observeEvent(input$ins_sscore, {
    # When user clicks insert, add
    # the sscore name and variables to the ui
    whole_html <-
      splitLayout(
        textInput(paste0("ssname", input$ins_sscore), "Name of sum score"),
        selectInput(paste0("sscore", input$ins_sscore),
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
    
    # If they clicked, jump to the model tab
    updateNavlistPanel(session,
                       inputId = "menu",
                       selected = "def_model")
  })
  
  new_df <-
    eventReactive(input$def_model, {
      
      # If no sscore was defined, jump the above
      if (input$ins_sscore == 0) return(cp_toydf)
      
      # We need to add new sscores to the origin df.
      # Calculate them here
      sscore <-
        lapply(1:input$ins_sscore, function(x) {
          all_sscore <- paste0("sscore", x)
          rowSums(cp_toydf[input[[all_sscore]]], na.rm = TRUE)
        })
      
      # Create sscore names
      ssnames <- vapply(1:input$ins_sscore,
                        function(x) input[[paste0("ssname", x)]],
                        FUN.VALUE = character(1))
      
      cbind(cp_toydf, as.data.frame(sscore, col.names = ssnames))
    })
  
  output$dv <-
    renderUI(
      radioButtons("dv_ch",
                   "Dependent variable",
                   choices = names(new_df()),
                   selected = names(new_df())[1])
    )
  
  output$iv <-
    renderUI(
      checkboxGroupInput("iv_ch",
                         "Independent variables",
                         choices = setdiff(names(new_df()), input$dv_ch))
    )
  output$cmv <-
    renderUI(
      checkboxGroupInput("cmv_ch",
                         "Which variables have Common Method Variance?",
                         choices = names(new_df()))
    )
}
