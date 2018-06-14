library(shiny)
library(shinycssloaders)
library(kableExtra)
library(survey)
library(ggplot2)
library(lavaan)
library(jtools)
library(dplyr)


all_variables <-
  c("polintr",
    "ppltrst",
    "stfeco",
    "stfedu",
    "stfhlth",
    "stflife", 
    "trstplt",
    "trstprl",
    "trstprt")

all_variables_label <- 
  c("How interested in politics",
    "Most people can be trusted or you can't be too careful",
    "How satisfied with present state of economy in country",
    "State of education in country nowadays",
    "State of health services in country nowadays",
    "How satisfied with life as a whole", 
    "Trust in politicians",
    "Trust in country's parliament",
    "Trust in political parties")

# Variables pasted together with labels
var_n_labels <- paste0(all_variables, ": ", all_variables_label)

ess_website <- "http://www.europeansocialsurvey.org"
path_login <- "/user/login"

#### NOTE #######
# Only thing left is to replace the sqp data w/ the actual SQP
# and delete the the toy dataset blow
######

sqp_df <-
  data.frame(question = all_variables,
             quality = c(0.2, 0.3, 0.5, 0.6, 0.9, 0.5, 0.6, 0.8, 0.1),
             reliability = c(0.1, 0.4, 0.5, 0.5, 0.7, 0.2, 0.5, 0.6, 0.9),
             validity = c(0.3, 0.2, 0.6, 0.7, 0.8, 0.4, 0.3, 0.7, 0.8))

sqp_df <- structure(sqp_df, class = c(class(sqp_df), "sqp"))


#option to deal with lonegly PSUs
options(survey.lonely.psu = "adjust")


# Text for errors when email is wrong or when 1 variable is selected as model
valid_email_error <- tags$span(style="color:red; font-size: 15px;", "Invalid email, please register at http://www.europeansocialsurvey.org/user/new")
minimum_var_error <- tags$span(style="color:red; font-size: 15px;", "Please select at least two variables for modeling.")
minimum_iv_error <- tags$span(style="color:red; font-size: 15px;", "Please select at least one independent variable.")

# Main wrapper of the page that contains the red banner on top
main_page <- function(...) {
  div(id = "fluidp",
      fluidPage(
        img(id = "ess_logo",
            src = "http://www.europeansocialsurvey.org/common/ess_eric/img/ess-logo-top.png"),
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

# First tab for logging in
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


# Second tab for defining model and selecting vars/countries
ui2 <- navlistPanel(id = "menu", widths = c(2, 8),
                    tabPanel("Select country and model", value = "country_n_vars",
                             selectInput("slid_cnt", "Pick a country", choices = all_countries),
                             uiOutput('chosen_vars'),
                             uiOutput('length_vars'),
                             actionButton("def_sscore", "I'm done, let me define my sum scores")
                    ),
                    tabPanel("Create sum scores", value = "def_sscore",
                             p("Would you like to create additional sum score variables?
                              Sum scores are the addition to several variables into one single
                              variable. click on 'Create sum score' to create your sum score."),
                             actionButton('ins_sscore', 'Create sum score'),
                             br(),
                             div(id = 'placeholder'),
                             actionButton('def_model', "I'm done, I want to define my model")
                    ),
                    tabPanel("Define the model", value = "def_model",
                             fluidRow(column(3, uiOutput("dv")),
                                      column(3, uiOutput("iv")),
                                      column(5, uiOutput("cmv"))),
                             fluidRow(column(3, ""),
                                      column(3, uiOutput('length_iv')), # three rows just to raise an error
                                      column(3, "")),
                             actionButton("calc_model", "Create model")
                    ),
                    tabPanel("Create model", value = "cre_model",
                             tabsetPanel(
                               tabPanel("Plot of results",
                                        withSpinner(tagList(plotOutput("model_plot")),
                                                    color = "#ff0000")
                                        ),
                               tabPanel("Table of results",
                                        withSpinner(tagList(tableOutput("model_table")),
                                                    color = "#ff0000")
                                        )
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

# For calculating sscore of quality scores and only returnig the row of
# the newly created qualtiy score
iterative_sscore <- function(x, y, sqp_df, data_df) {
  sqp_sscore_str(sqp_data = sqp_df,
                 df = data_df,
                 new_name = x,
                 vars_names = y) %>% 
    filter(question == x)
}


server <- function(input, output, session) {
  
  # ### Loging in ####
  # # Record whether user logged in
  # USER <- reactiveValues(Logged = FALSE)
  # 
  # # Update the login state if email is valid
  # observe({ 
  #   if (USER$Logged == FALSE) {
  #     if (!is.null(input$Login)) {
  #       if (input$Login > 0) {
  #         email <- isolate(input$essemail)
  #         # Password <- isolate(input$passwd)
  #         # Id.username <- which(my_username == Username)
  #         # Id.password <- which(my_password == Password)
  #         
  #         # I was using essurvey:::authenticate here but because the .global_vars
  #         # are not in the .Globalenv, the handle of the website was not shared
  #         # across requests. I have input the website variables manually
  #         auth <- is_error(authenticate_ess(email, ess_website, path_login))
  #         if (auth || email == "") {
  #           output$emailValid <- renderUI(p(valid_email_error))
  #         } else {
  #           USER$Logged <- TRUE
  #         }
  #       }
  #     }
  #   }
  # })
  # 
  # # Change UI based on whether the user is logged in or not.
  # observe({
  #   if (USER$Logged == FALSE) {
  #     output$page <- renderUI({
  #       main_page(ui1)
  #     })
  #   }
  #   
  #   if (USER$Logged == TRUE) {
  #     output$page <- renderUI({
  #       div(main_page(ui2))
  #     })
  #   }
  # })
  
  output$page <- renderUI({
  div(main_page(ui2))
  })

  output$chosen_vars <-
    renderUI({
      checkboxGroupInput(
        'vars_ch',
        'Choose variables to use in the modeling',
        var_n_labels,
        width = '500px')
      
    })
  
  # Checks whether you are in `tab`
  is_tab <- function(tab) {
    is_specific_tab <- reactive({
      if (is.null(input$menu) || input$menu != tab) FALSE else TRUE
    })
    
    is_specific_tab()
  }
  
  # observe(print(is_tab("def_sscore")))
  
  # This is a turning point. We want to ensure that there
  # are at least two variables chosen for the modeling.
  # If they are, we jumpt to sscore tabs.
  observeEvent(input$def_sscore, {
    if (length(input$vars_ch) < 2) {
      output$length_vars <- renderUI(p(minimum_var_error))
    } else {
      updateTabsetPanel(session,
                        inputId = "menu",
                        selected = "def_sscore")
    }
  })
  
  
  # But if the user changes the tab manually, then check that we have the number
  # of variables
  observeEvent(is_tab("def_sscore"), {
    # input$df_sscore at the beginning is null, that's why I check that the length
    # is different from zero.
    if (length(input$vars_ch) < 2 && (length(input$def_sscore) != 0 && input$def_sscore != 0)) {
      output$length_vars <- renderUI(p(minimum_var_error))
    } else {
      output$length_vars <- renderUI(p(" "))
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
    if (input$ins_sscore == 0) return(character())
    
    # When user clicks insert, add
    # the sscore name and variables to the ui
    whole_html <-
      splitLayout(
        textInput(paste0("ssname", input$ins_sscore), "Name of sum score"),
        selectInput(paste0("sscore", input$ins_sscore),
                    'Variables that compose the sum score',
                    choices = input$vars_ch,
                    multiple = TRUE,
                    selectize = FALSE,
                    width = '500px',
                    size = length(input$vars_ch)),
        cellWidths = c("17%", "83%")
      )
    
    # Interactively add a sumscore to the UI
    insertUI(selector = '#placeholder', ui = whole_html)
  })
  
  # When the sumscores are ready, the user clicks
  # define model and we switch to the define model
  # tab to select dependent and independent variables
  observeEvent(input$def_model, {
      updateTabsetPanel(session,
                        inputId = "menu",
                        selected = "def_model")
  })

  clean_ssnames <-
    eventReactive(input$def_model, {
      if (input$ins_sscore == 0) return(character())
      
      # Create sscore names
      all_names <- 
        vapply(1:input$ins_sscore,
               function(x) input[[paste0("ssname", x)]],
               FUN.VALUE = character(1))
      all_names[all_names != ""]
    })
  
  # Get a list where each slot contains
  # the variables that compose each sscore.
  # The counterpart of this is clean_ssnames()
  # which contains the variable names of each sscore
  sscore_list <-
    eventReactive(input$def_model, {
      lapply(1:input$ins_sscore, function(x) {
        all_sscore <- paste0("sscore", x)
        gsub(":.*$", "", input[[all_sscore]])
      })
    })
  
  # When clean_ssnames() and sscore_list()
  # are first run when the shiny app starts
  # they contain a silent error because the
  # eventReactive counter is at 0.
  # With is_error I test whether it has been ran or not
  # and based on that return the same variable or an empty characther.
  # this is because these two functions are passed to the list
  # of independent and dependent variables and an error
  # will crash it.
  exists_cleanssnames <- reactive({
    if (!is_error(clean_ssnames())) {
      clean_ssnames()
    } else {
      character()
    }
  })
  
  exists_sscorelist <- reactive({
    if (!is_error(sscore_list())) {
      unlist(sscore_list())
    } else {
      character()
    }
  })
  

  # observe({
  #   print(exists_cleanssnames())
  #   print(sscore_list)
  #   # print("Does it exists?:", exists("clean_ssnames"))
  #   # print(chosen_vars(), exists_cleanssnames())
  #   # print(exists_cleanssnames())
  #   # print(exists_sscorelist())
  # })
  
  # This preservers the order of variables picked if the user already
  # choose a dependent variable
  preserve_order_dv <- function(equation_side) {
    selected <- reactive({
      # If no variable has been chosen, choose first
      if (is.null(input[[equation_side]]))  {
        chosen_vars()[1]
        # If a variable has been chosen but it was later deleted from the chosen variables
        # return to the first variable
      } else if (!is.null(input[[equation_side]]) & !input[[equation_side]] %in% chosen_vars()) {
        chosen_vars()[1] 
        # Otherwise return the already picked dependent variable
      } else { 
        input[[equation_side]]
      }
    })
    selected()
  }
  
  preserve_order <- function(equation_side) {
    selected <- reactive({
      # If no variable has been chosen, don't select anything
      if (is.null(input[[equation_side]]))  {
        NULL
        # If any of the variables selected are in the chosen variables, the bring only these ones
      } else if (any(index <- input[[equation_side]] %in% chosen_vars())) {
        input[[equation_side]][index]
      }
    })
    selected()
  }
  
  # Define the three model parts
  # Pick the dependent variable
  output$dv <-
    renderUI(
      radioButtons("dv_ch",
                   "Dependent variable",
                   choices = setdiff(c(chosen_vars(), exists_cleanssnames()), exists_sscorelist()),
                   selected = preserve_order_dv("dv_ch")
      )
    )
  
  observe({
    print(input$dv_ch)
  })
  
  # Pick the independent variables
  output$iv <-
    renderUI(
      checkboxGroupInput("iv_ch",
                         "Independent variables",
                         choices = setdiff(c(chosen_vars(), exists_cleanssnames()),
                                           c(input$dv_ch, exists_sscorelist())),
                         selected = preserve_order("iv_ch"))
    )
  
  
  # Pick the variables that share CMV
  output$cmv <-
    renderUI(
      checkboxGroupInput("cmv_ch",
                         "Which variables are measured with the same method?",
                         choices = c(input$dv_ch, input$iv_ch),
                         selected = preserve_order("cmv_ch"))
    )
  
  observeEvent(input$calc_model, {
    if (length(input$iv_ch) < 1) {
      output$length_iv <- renderUI(p(minimum_iv_error))
    } else {
      updateTabsetPanel(session,
                        inputId = "menu",
                        selected = "cre_model")
    }
  })
  
  
  # This is the ess data w/ only the selected variables
  var_df <-
    eventReactive(input$calc_model, {
      # Choose country when user calculates model
      # This is for when the ess data is available
      upd_ess <- ess_df[[input$slid_cnt]]
      # upd_ess <- ess_df[chosen_vars()]
      
      # If no sscore was defined, return the same df the above
      if (input$ins_sscore == 0) return(upd_ess)
      
      # We need to add new sscores to the origin df.
      # Calculate them here
      sscore <-
        lapply(sscore_list(), function(x) {
          rowSums(upd_ess[x], na.rm = TRUE)
        })
      
      cbind(upd_ess, as.data.frame(sscore, col.names = exists_cleanssnames()))
    })
  
  upd_sqpdf <-
    eventReactive(input$calc_model, {
      # Calculate the quality of sumscore of each name-variables pairs
      # and then bind them together with the sqp_df. The final output
      # is the sqp_df with the quality of the N sum scores created.
      q_sscore <- lapply(seq_along(exists_cleanssnames()), function(index) {
        iterative_sscore(exists_cleanssnames()[index], sscore_list()[[index]], sqp_df, var_df())
      })
      
      bind_rows(sqp_df, q_sscore)
    })
  
  weighted_data <- eventReactive(input$calc_model, {
    # Define svydesign object thas has ALL columns of ess_data + sscore columns
    mk_ess_svy(svyinfo = svyinfo[[input$slid_cnt]], # svyinfo comes from globals.R
               ess_data = var_df(),
               round = 6,
               email = Sys.getenv("ess_email"),
               id_vars = all_ids,
               ess_website = ess_website)
    
  })
  
  ### Calculations begin ####
  models_coef <- eventReactive(input$calc_model, {
    
    ch_vars <- c(input$dv_ch, input$iv_ch)
    
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
          svyvar(selected_vars_formula, design = weighted_data(), na.rm = TRUE)
        )
      )
    
    attr(corrected, "statistic") <- "covariance"
    
    # Subset chosen variables in sqp_df and
    # create quality estimate for the
    filtered_sqp <- upd_sqpdf()[upd_sqpdf()[[1]] %in% ch_vars, ]
    
    # Replace all NA's so that there's no error.
    filtered_sqp <- map_dfc(filtered_sqp, ~ {.x[is.na(.x)] <- 0; .x})
    
    # Replace diagonal by multiplying it with the quality of each variable
    diag(corrected) <- diag(corrected) * filtered_sqp$quality
    
    #subtract the cmv from the observed correlation
    # Calculate the common method variance of some variables
    # and subtract that common method variance from the correlation
    # coefficients of the variables.
    
    # sqpr::sqp_cmv is a bit strange for programming
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
      map(~ select(.x, rhs, est, pvalue, ci.lower, ci.upper))
    
    coef_table
  })
  
  observeEvent(input$calc_model, {
    if (length(input$iv_ch) < 1) {
      output$length_iv <- renderUI(p(minimum_iv_error))
    } else {
      output$length_iv <- renderUI(p(""))
      updateTabsetPanel(session,
                        inputId = "menu",
                        selected = "cre_model")
    }
  })
  
  # Final table
  output$model_table <-
    reactive({
      models_coef() %>% 
        reduce(left_join, by = "rhs") %>% 
        mutate_if(is.numeric, function(x) as.character(round(x, 3))) %>% 
        set_names(c("Covariates", rep(c("Estimate", "P-val", "Lower CI", "Upper CI"), times = 2))) %>% 
        kable() %>% 
        kable_styling("striped", full_width = F) %>% 
        add_header_above(c(" ", "Original" = 4, "Corrected" = 4))
    })
  
  # Final plot
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
        theme_bw(base_size = 16)
    })
}
