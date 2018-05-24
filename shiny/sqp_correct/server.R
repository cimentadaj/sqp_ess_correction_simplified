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

set.seed(2311)
# Replace w/ all ESS id's.
all_ids <- c("id")
# Replace w/ ess variables
all_variables <- paste0("V", 1:9)

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
                   choices = c(all_variables, clean_ssnames()),
                   selected = all_variables[1])
    )
  
  # Pick the independent variables
  output$iv <-
    renderUI(
      checkboxGroupInput("iv_ch",
                         "Independent variables",
                         choices = setdiff(c(all_variables, clean_ssnames()),
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
      # upd_ess <- ess_df[[input$slid_cnt]][all_variables]
      upd_ess <- ess_df[all_variables]
      
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
    
    ## ---- fig.width = 7, fig.with = 9----------------------------------------
    # Difference in coefficients between models
    
    coef_table <-
      list(fit, fit.corrected) %>%
      map(parameterestimates) %>%
      map(~ filter(.x, lhs == ch_vars[1])) %>%
      map(~ select(.x, rhs, est, ci.lower, ci.upper)) %>%
      bind_rows() %>%
      mutate(model = rep(c("original", "corrected"), each = ncol(original)))
  })
  
  output$model_plot <-
    renderPlot({
      ggplot(models_coef(), aes(rhs, est, colour = model)) +
      geom_linerange(aes(ymin = ci.lower, ymax = ci.upper),
                     position = position_dodge(width = 0.5)) +
      geom_point(position = position_dodge(width = 0.5)) +
      labs(x = "Predictors", y = "Estimated coefficients") +
      theme_bw()
    
  })
}
