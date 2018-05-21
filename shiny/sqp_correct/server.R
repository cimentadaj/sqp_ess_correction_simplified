#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(survey)

# Replace w/ all ESS id's.
all_ids <- c("id")
# Replace w/ ess variables
all_variables <- paste0("V", 1:9)

ess_df = cbind(id = 1:100, as.data.frame(replicate(15, rpois(100, 10))))
sddf_data = data.frame(id = 1:100,
                       stratify = sample(20, replace = TRUE),
                       dweight = rnorm(100, mean = 1))

server <- function(input, output, session) {
  
  # Because the ess_df (ESS data) must be fresh to all users
  # we call it from `globals.R` in order for it to be available
  # through out sessions. All countries are downloaded when
  # the app is launched.
  
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
  
  ssnames <-
    eventReactive(input$def_model, {
      if (input$ins_sscore == 0) return(character())
      
      # Create sscore names
      vapply(1:input$ins_sscore,
             function(x) input[[paste0("ssname", x)]],
             FUN.VALUE = character(1))
    })
  
  ## Define the three model parts
  # Pick the dependent variable
  output$dv <-
    renderUI(
      radioButtons("dv_ch",
                   "Dependent variable",
                   choices = c(all_variables, ssnames()),
                   selected = all_variables[1])
    )
  
  # Pick the independent variables
  output$iv <-
    renderUI(
      checkboxGroupInput("iv_ch",
                         "Independent variables",
                         choices = setdiff(c(all_variables, ssnames()),
                                           input$dv_ch))
    )
  
  # Pick the variables that share CMV
  output$cmv <-
    renderUI(
      checkboxGroupInput("cmv_ch",
                         "Which variables have Common Method Variance?",
                         choices = c(input$dv_ch, input$iv_ch))
    )
  
  # YOU LEFT OFF HERE!
  # rowSums below is generating an error when defining the model
  # Maybe due to two things: tibble not being a matrix
  # or I'm thinking it's a tibble but I've just subsetting something random
  # Check it manually
  
  var_df <-
    eventReactive(input$calc_model, {
      # Choose country when user calculates model
      upd_ess <- ess_df[[input$slid_cnt]][all_variables]
      
      # If no sscore was defined, return the same df the above
      if (input$ins_sscore == 0) return(upd_ess)
      
      # We need to add new sscores to the origin df.
      # Calculate them here
      sscore <-
        lapply(1:input$ins_sscore, function(x) {
          all_sscore <- paste0("sscore", x)
          rowSums(upd_ess[input[[all_sscore]]], na.rm = TRUE)
        })
      
      cbind(upd_ess, as.data.frame(sscore, col.names = ssnames()))
    })
  
  # If calculate model is clicked, switch panel
  observeEvent(input$calc_model, {
    updateNavlistPanel(session,
                       inputId = "menu",
                       selected = "cre_model")
  })
  
  id_df <- reactive(
    id_df <- ess_df[[input$slid_cnt]][all_ids]
  )
  
  
  ## Replace all of this w/ the sddf script
  # Define svydesign object
  weighted_data <-
    reactive(
    svydesign(
      id = ~ id,
      strata = ~ stratify,
      weights = ~ dweight,
      data = dplyr::left_join(cbind(id_df(), var_df()), sddf_data, by = all_ids)
    )
  )
  
  observe(
    print(weighted_data())
  )
}
