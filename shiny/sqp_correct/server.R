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
library(ggplot2)
library(lavaan)
library(jtools)
library(dplyr)
library(purrr)
library(kableExtra)

set.seed(2311)
# Replace w/ all ESS id's.
all_ids <- c("id")
# Replace w/ ess variables
all_variables <- paste0("V", 1:9)
all_variables_label <- paste0("V", 1:9)
var_n_labels <- paste0(all_variables, ": ", all_variables_label)

ess_df <- cbind(id = 1:100, as.data.frame(replicate(15, rpois(100, 10))))
sddf_data <- data.frame(id = 1:100,
                       stratify = sample(20, replace = TRUE),
                       dweight = runif(100))

sqp_df <-
  data.frame(question = paste0("V", 1:5),
         quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
         reliability = c(0.1, 0.4, 0.5, 0.5, 0.7),
         validity = c(0.3, 0.2, 0.6, 0.7, 0.8))

sqp_df <- structure(sqp_df, class = c(class(sqp_df), "sqp"))


#option to deal with lonegly PSUs
options(survey.lonely.psu="adjust")


valid_email_error <- tags$span(style="color:red; font-size: 15px;", "Invalid email, please register at http://www.europeansocialsurvey.org/user/new")
minimum_var_error <- tags$span(style="color:red; font-size: 15px;", "Please select at least two variables for modeling.")

main_page <- function(...) {
  div(id = "fluidp",
      fluidPage(
        img(id = "ess_logo",
            src = "http://www.europeansocialsurvey.org/common/ess_eric/img/ess-logo-top.png",
            height = 45),
        br(),
        br(),
        ...
      ),
      tags$style(type="text/css",
              "#fluidp {
               background-color: red;
               height: 70px;
               }
               #ess_logo {
               margin-top: 13px;
               }")
  )
}

ui1 <- tagList(
    div(id = "login",
        textInput("essemail", "Registered ESS email"),
                  # passwordInput("passwd", "Password"),
                  br(),
                  uiOutput("emailValid"),
                  actionButton("Login", "Log in")),
    tags$style(type="text/css",
               "#login {
               font-size: 14px;
               text-align: left;
               position: absolute;
               top: 50%;
               left: 50%;
               margin-top: -100px;
               margin-left: -150px; 
               width: 25%;
               }")
  )


#Define UI for application that draws a histogram
ui2 <- navlistPanel(id = "menu", widths = c(2, 8),
                   tabPanel("Select variables and country",
                            selectInput("slid_cnt", "Pick a country", choices = all_countries),
                            uiOutput('chosen_vars'),
                            uiOutput('length_vars'),
                            actionButton("def_sscore", "I'm done, let me define my sum scores")
                   ),
                   tabPanel("Create sum scores", value = "def_sscore",
                            p("Would you like to create additional sumscore variables?
                              Sum scores are the addition fo several variables into one single
                              variable. click on 'Insert new sum score' to create your sum score."),
                            actionButton('ins_sscore', 'Insert new sum score'),
                            br(),
                            div(id = 'placeholder'),
                            actionButton('def_model', "I'm done, I want to define my model")
                   ),
                   tabPanel("Define the model", value = "def_model",
                            fluidRow(column(3, uiOutput("dv")),
                                     column(3, uiOutput("iv")),
                                     column(5, uiOutput("cmv"))),
                            actionButton("calc_model", "Create model")),
                   tabPanel("Create model", value = "cre_model",
                            tabsetPanel(
                            tabPanel("Plot of results", plotOutput("model_plot")),
                            tabPanel("Table of results", tableOutput("model_table"))
                              )
                            )
      )

# For checking when the ess email is valid or not
is_error <- function(x) {
  if (is(try(x, silent = TRUE), "try-error")) {
    return(TRUE)
  }
  FALSE
}

server <- function(input, output, session) {
  
  # Record whether user logged in
  USER <- reactiveValues(Logged = FALSE)
  
  # Update the login state if email is valid
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          email <- isolate(input$essemail)
          # Password <- isolate(input$passwd)
          # Id.username <- which(my_username == Username)
          # Id.password <- which(my_password == Password)
            auth <- is_error(essurvey:::authenticate(email))
            if (auth || email == "") {
              output$emailValid <- renderUI(p(valid_email_error))
            } else {
              USER$Logged <- TRUE
            }
          }
        }
      }
  })

  # Change UI based on whether the user is logged in or not.
  observe({
    if (USER$Logged == FALSE) {
      output$page <- renderUI({
        main_page(ui1)
      })
    }
    
    if (USER$Logged == TRUE) {
      output$page <- renderUI({
        div(main_page(ui2))
      })
    }
  })
  
  output$chosen_vars <-
    renderUI({
      selectInput('vars_ch',
                  'Choose variables to use in the modeling',
                  var_n_labels,
                  multiple = TRUE,
                  selectize = FALSE)
      
    })
  
  # This is a turning point. We want to ensure that there
  # are at least two variables chosen for the modeling.
  # If they are, we jumpt to sscore tabs
  observeEvent(input$def_sscore, {
    if (length(input$vars_ch) < 2) {
      output$length_vars <- renderUI(p(minimum_var_error))
    } else {
      updateTabsetPanel(session,
                        inputId = "menu",
                        selected = "def_sscore")
    }
  })
  
  # Remove labels from the chosen variables
  chosen_vars <- reactive({
    gsub(":.*$", "", input$vars_ch)
  })

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
                    choices = chosen_vars(), multiple = TRUE),
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
  # Delete any sumscores that are empty!
  clean_ssnames <- reactive({
    ssnames()[ssnames() != ""]
  })
  
  ## Define the three model parts
  # Pick the dependent variable
  output$dv <-
    renderUI(
      radioButtons("dv_ch",
                   "Dependent variable",
                   choices = c(chosen_vars(), clean_ssnames()),
                   selected = chosen_vars()[1])
    )
  
  # Pick the independent variables
  output$iv <-
    renderUI(
      checkboxGroupInput("iv_ch",
                         "Independent variables",
                         choices = setdiff(c(chosen_vars(), clean_ssnames()),
                                           input$dv_ch))
    )
  
  # Pick the variables that share CMV
  output$cmv <-
    renderUI(
      checkboxGroupInput("cmv_ch",
                         "Which variables have Common Method Variance?",
                         choices = c(input$dv_ch, input$iv_ch))
    )
  
  var_df <-
    eventReactive(input$calc_model, {
      # Choose country when user calculates model
      # This is for when the ess data is available
      # upd_ess <- ess_df[[input$slid_cnt]][chosen_vars()]
      upd_ess <- ess_df[chosen_vars()]
      
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
  
  models_coef <- eventReactive(input$calc_model, {
    
    # For when the ess data is in
    # id_df <- ess_df[[input$slid_cnt]][all_ids]
    id_df <- ess_df[all_ids]
    
    
    ## Replace all of this w/ the sddf script
    # Define svydesign object
    weighted_data <-
        svydesign(
          id = ~ id,
          strata = ~ stratify,
          weights = ~ dweight,
          data = dplyr::left_join(cbind(id_df, var_df()), sddf_data, by = all_ids)
        )
    
    ch_vars <- c(input$dv_ch, input$iv_ch)
    
    ### Calculations begin ####
    
    # Correlation matrix without weights:
    original <- cor(var_df()[ch_vars],
                    use = "complete.obs",
                    method = "pearson")
    
    selected_vars_formula <- 
      eval(
        parse(text = paste0("~", paste0(ch_vars,collapse = "+"))
        )
      )
    
    
    # We get the covariance of the variables instead of the
    # correlation
    corrected <- 
      cov2cor(
        as.matrix(
          svyvar(selected_vars_formula, design = weighted_data, na.rm = TRUE)
        )
      )

    attr(corrected, "statistic") <- "covariance"
    
    # Subset chosen variables in sqp_df
    filtered_sqp <- sqp_df[sqp_df[[1]] %in% ch_vars, ]
    
    # Replace diagonal by multiplying it with the quality of each variable
    diag(corrected) <- diag(corrected) * filtered_sqp$quality
    
    #subtract the cmv from the observed correlation
    # Calculate the common method variance of some variables
    # and subtract that common method variance from the correlation
    # coefficients of the variables.
    
    # The sqpr::sqp_cmv is a bit strange for programming
    # because it uses non standard evaluation. I defined
    # sqp_cmv_str in `globals.R` to be the same
    # but it accepts a string in cmv_vars instead of `...`
    # in sqpr::sqp_cmv
    
    if (length(input$cmv_ch) > 1) {
      corrected <-
        cov2cor(
          as.matrix(
            sqp_cmv_str(x = corrected,
                        sqp_data = filtered_sqp,
                        cmv_vars = input$cmv_ch)[-1]
          )
        )
    }
    
    # Create model formula
    model <- paste0(ch_vars[1], 
                    " ~ ", 
                    paste0(ch_vars[-1], collapse = " + "))
    
    sample_size <- nrow(var_df())
    
    # Model based on original correlation matrix
    fit <-
      sem(model,
          sample.cov = original,
          sample.nobs = sample_size)
    
    # Model based on corrected covariance matrix 
    fit.corrected <-
      sem(model,
          sample.cov=corrected,
          sample.nobs= sample_size)
    

    # Why do I leave it incomplete and not bind everything into a data frame ready to plot?
    # Because the table and the plot do different computations so I leave it up to the
    # point that both operations have common ground.
    coef_table <-
      list(fit, fit.corrected) %>%
      map(parameterestimates) %>%
      map(~ filter(.x, lhs == ch_vars[1])) %>%
      map(~ select(.x, rhs, pvalue, est, ci.lower, ci.upper))
    
    coef_table
  })

  output$model_table <-
    reactive({
      models_coef() %>% 
        reduce(left_join, by = "rhs") %>% 
        mutate_if(is.numeric, function(x) round(x, 3)) %>% 
        set_names(c("Covariates", rep(c("Estimate", "P-val", "Lower CI", "Upper CI"), times = 2))) %>% 
        kable() %>% 
        kable_styling("striped", full_width = F) %>% 
        add_header_above(c(" ", "Original" = 4, "Corrected" = 4))
    })

  output$model_plot <-
    renderPlot({
      models_coef() %>% 
        bind_rows() %>%
        mutate(model = rep(c("original", "corrected"), each = length(unique(.$rhs)))) %>% 
        ggplot(aes(rhs, est, colour = model)) +
        geom_linerange(aes(ymin = ci.lower, ymax = ci.upper),
                       position = position_dodge(width = 0.5)) +
        geom_point(position = position_dodge(width = 0.5)) +
        labs(x = "Predictors", y = "Estimated coefficients") +
        theme_bw()
  })
}
