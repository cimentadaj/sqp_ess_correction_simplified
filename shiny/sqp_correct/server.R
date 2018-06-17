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
  read_table(
  "polintr        0.624    0.964   0.601
  ppltrst        0.737    0.954   0.703
  stfeco         0.797    0.912   0.727
  stfedu         0.757    0.838   0.635
  stfhlth        0.760    0.798   0.607
  stflife        0.721    0.911   0.657
  trstplt        0.852    0.965   0.822
  trstprl        0.812    0.959   0.779
  trstprt        0.858    0.956   0.821",
  col_names = FALSE
) %>% setNames(c("question", "reliability", "validity", "quality"))

# sqp_df <-
#   data.frame(question = all_variables,
#              quality = c(0.2, 0.3, 0.5, 0.6, 0.9, 0.5, 0.6, 0.8, 0.1),
#              reliability = c(0.1, 0.4, 0.5, 0.5, 0.7, 0.2, 0.5, 0.6, 0.9),
#              validity = c(0.3, 0.2, 0.6, 0.7, 0.8, 0.4, 0.3, 0.7, 0.8))

sqp_df <- structure(sqp_df, class = c(class(sqp_df), "sqp"))


#option to deal with lonegly PSUs
options(survey.lonely.psu = "adjust")


# Text for errors when email is wrong or when 1 variable is selected as model
valid_email_error <- tags$span(style="color:red; font-size: 15px;", "Invalid email, please register at http://www.europeansocialsurvey.org/user/new")
minimum_var_error <- tags$span(style="color:red; font-size: 15px;", "Please select at least two variables for modeling.")
minimum_iv_error <- tags$span(style="color:red; font-size: 15px;", "Please select at least one independent variable.")

color_website <- "#AD1400"

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
                 paste0(
                   "#fluidp {
                 background-color: ", color_website, ";
                 height: 70px;
                 }
                 #ess_logo {
                 margin-top: 15px;
                 position: relative
                 }"
                 ))
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

ess_button <- function(id, label, color = color_website) {
  actionButton(id,
               label,
               style = paste0("color: #fff;
                              background-color:", color, ";
                              border-color:", color))
}


# Second tab for defining model and selecting vars/countries
ui2 <- tabsetPanel(id = "menu",
                    tabPanel("Select country and model", value = "country_n_vars",
                             br(),
                             fluidRow(
                               column(3, selectInput("slid_cnt", "Pick a country", choices = all_countries)),
                               column(7, style = "margin-top: 25px;", actionButton("select_all", "Select all variables"))
                               ),
                             uiOutput('chosen_vars'),
                             uiOutput('length_vars'),
                             ess_button("def_sscore", "I'm done, let me define my sum scores")
                    ),
                    tabPanel("Create sum scores", value = "def_sscore",
                             br(),
                             p("Would you like to create additional sum score variables?
                              Sum scores are the addition of several variables into one single
                              variable. click on 'Create sum score' to create your sum score."),
                             br(),
                             br(),
                             sidebarLayout(
                               sidebarPanel(
                                 actionButton('ins_sscore', 'Create sum score'),
                                 h6("Create more than one sum score by clicking again."),
                                 br(),
                                 br(),
                                 uiOutput("list_sscore", style = "margin-top: -25px;"),
                                 uiOutput("del_sscore"),
                                 br(),
                                 ess_button('def_model', "I'm done, I want to define my model"),
                                 width = 3
                                 ),
                               mainPanel(
                                 br(),
                                 div(id = 'placeholder'),
                                 tags$script("
                                 Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                                    Shiny.onInputChange(variableName, null);
                                });
                                ")
                               )
                    )),
                    tabPanel("Define the model", value = "def_model",
                             br(),
                             fluidRow(column(3, uiOutput("dv")),
                                      column(3, uiOutput("iv")),
                                      column(5, uiOutput("cmv"))),
                             fluidRow(column(3, ""),
                                      column(3, uiOutput('length_iv')), # three rows just to raise an error
                                      column(3, "")),
                             ess_button("calc_model", "Create model")
                    ),
                    tabPanel("Create model", value = "cre_model",
                             br(),
                             tabsetPanel(
                               tabPanel("Plot of results",
                                        withSpinner(tagList(plotOutput("model_plot")),
                                                    color = color_website)
                               ),
                               tabPanel("Table of results",
                                        withSpinner(tagList(tableOutput("model_table")),
                                                    color = color_website)
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

# for excluding a named slot from a list
pick_list <- function(exclude, the_list) {
  the_list[!names(the_list) == exclude]
}


server <- function(input, output, session) {
  
  ### Loging in ####
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

          # I was using essurvey:::authenticate here but because the .global_vars
          # are not in the .Globalenv, the handle of the website was not shared
          # across requests. I have input the website variables manually
          auth <- is_error(authenticate_ess(email, ess_website, path_login))
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
      checkboxGroupInput(
        'vars_ch',
        'Choose variables to use in the modeling',
        var_n_labels,
        width = '500px')
      
    })
  
  observeEvent(input$select_all, {
        updateCheckboxGroupInput(
          session,
          'vars_ch',
          'Choose variables to use in the modeling',
          var_n_labels,
          choices = var_n_labels)
  })
  
  # Checks whether you are in `tab`
  is_tab <- function(tab) {
    is_specific_tab <- reactive({
      if (is.null(input$menu) || input$menu != tab) FALSE else TRUE
    })
    
    is_specific_tab()
  }
  
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
    
    # so that the user knows he/she can click there to add more sum scores after the first one
    if (input$ins_sscore == 1)  {
      updateActionButton(session, 'ins_sscore', 'Create another sum score') 
    }
    
    
    output$del_sscore <- renderUI({actionButton('del_sscore', 'Delete sum score')})
    
    # And create a list of those names to delete
    output$list_sscore <- renderUI({selectInput('list_sscore',
                                                label = "If you'd like to delete a sum score you've created, which one?",
                                                choices = possible_ssnames())
    })
    
    
    # When user clicks insert, add
    # the sscore name and variables to the ui
    whole_html <-
      splitLayout(
        tags$div(id = paste0("splitlayout", input$ins_sscore),
                fluidRow(
                  column(6, textInput(paste0("ssname", input$ins_sscore), "Name of sum score")),
                  column(6, checkboxGroupInput(inputId = paste0("sscore", input$ins_sscore),
                                               label = 'Variables that compose the sum score',
                                               choices = input$vars_ch))
                ),
                # To remove the horizontall scroll bar
                tags$style(type = "text/css",
                           paste0(paste0("#splitlayout", input$ins_sscore),
                                  " {overflow-x: hidden}"))
                
        ),
        cellWidths = c("100%")
      )
    # Interactively add a sumscore to the UI
    insertUI(selector = '#placeholder', ui = whole_html)
  })
  
  # Grab the names of the sscore reactively, that is, whenever they are being written
  possible_ssnames <- reactive({
    unname(unlist(lapply(grep("^ssname", names(input), value = TRUE),
           function(x) if (!is_empty(x)) input[[x]] else "")))
  })
  
  ## Deleting sum scores
  observeEvent(input$del_sscore, {
    if (input$del_sscore == 0 | is_empty(input$list_sscore)) return(character())

    # print(possible_ssnames())
    
    # These are all the inputs from the app temporarily stored without the input
    # list_sscore. Why do I exclude list_sscore? See the paragraph below.
    # Why do I save this into a temp list? Because I acess this same data below
    # a few times.
    temp_input <- pick_list("list_sscore", reactiveValuesToList(input))

    # list_sscore stores the name of the sscore whenever it was picked
    # so when I compare which variable was picked agaisnt all available
    # inputs, I exclude list_sscore from the list because, for example,
    # var1 will match both ssname1 and list_sscore. By removing it, it will
    # only match sscore1 and be able to delete only that score
    semi_index <- which(input$list_sscore == temp_input)
    
    # Because I interactively add/delete sscores, only keeping the index is unreliable
    # because it will change as soon as I add a new sscore. So I grab the name of the index
    index_names <- gsub("ssname", "", names(temp_input)[semi_index])

    # explore_index <-
    #       as.data.frame(
    #         lapply(input$list_sscore, function(x) x == pick_list("list_sscore", reactiveValuesToList(input)))
    #       ) %>% mutate(inside = substr(pick_list("list_sscore", reactiveValuesToList(input)), 1, 10),
    #                    name = names(pick_list("list_sscore", reactiveValuesToList(input))))
    # print(explore_index)
    
    # print(index_names)
    
    # To avoid deleting the sscore and ssname separately, I just delete the whole layout
    # that has both things together.
    splits_del <- paste0("#splitlayout", index_names)
    removeUI(
      selector = paste0("div:has(> ", splits_del, ")"),
      multiple = TRUE,
      immediate = TRUE
    )
    
    # Regardless of whether I deleted the splitlayout or not, the
    # input names ssname* and sscore* where created. Even if I delete
    # the splitlayout, there's still there. This is a loop that
    # sets both ssname* and sscore* for the chosen sscore to
    # NULL using a custom javascript message. The counterparty
    # of this is in the UI where I define the function.
    session$sendCustomMessage(type = "resetValue", message = names(temp_input)[semi_index])
    
    # Everything above was to delete the ssname. To delete sscore we have the index_names
    # of ssname which is, for example, 1. We just have to delete sscore1
    session$sendCustomMessage(type = "resetValue", message = paste0("sscore", index_names))
    
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
      
      # I want to grab the value in all ssname* in which
      # the value is neither "" nor NULL (because if it's NULL)
      # it means it was deleted previously.
      
      temp_input <- reactiveValuesToList(input)
      non_null_name <- grepl("^ssname", names(temp_input[!is.null(temp_input)]))
      all_ssnames <- non_null_name[non_null_name != ""]
      unlist(temp_input[all_ssnames])
    })

  # Get a list where each slot contains
  # the variables names as strings that
  # compose each sscore.
  # The counterpart of this is clean_ssnames()
  # which contains the variable names of each sscore
  sscore_list <-
    eventReactive(input$def_model, {

      temp_input <- reactiveValuesToList(input)
      
      non_null_sscore <- grepl("^sscore", names(temp_input[!is.null(temp_input) & !is_empty(temp_input)]))
      final_sscore <- temp_input[non_null_sscore]
      
      list_sscore <- lapply(final_sscore, function(x) {
        gsub(":.*$", "", x)
      })

      list_sscore
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
  
  # This preservers the order of variables picked if the user already
  # choose a dependent variable
  preserve_order_dv <- function(equation_side, which_vars) {
    selected <- reactive({
      # If no variable has been chosen, choose first
      if (is.null(input[[equation_side]]))  {
        which_vars[1]
        # If a variable has been chosen but it was later deleted from the chosen variables
        # return to the first variable
      } else if (!input[[equation_side]] %in% which_vars) {
        which_vars[1]
        # Otherwise return the already picked dependent variable
      } else {
        input[[equation_side]]
      }
    })
    
    selected()
  }
  
  # This one is generic to independent and cmvs. See above for an explanation of the function
  preserve_order <- function(equation_side) {
    selected <- reactive({
      # If no variable has been chosen, don't select anything
      if (is.null(input[[equation_side]]))  {
        NULL
        # If any of the variables selected are in the chosen variables, the bring only these ones
      } else if (any(index <- input[[equation_side]] %in% c(chosen_vars(), exists_cleanssnames()))) {
        input[[equation_side]][index]
      }
    })
    
    selected()
  }
  
  # Define the reactive number of variables available. This will change a lot when
  # choosing/unchoosing sumscore variables and dv and iv's, so we save it on it's own.
  available_vars <- reactive(setdiff(c(chosen_vars(), exists_cleanssnames()), exists_sscorelist()))
  
  # Define the three model parts
  # Pick the dependent variable
  output$dv <-
    renderUI(
      radioButtons("dv_ch",
                   "Dependent variable",
                   choices = available_vars(),
                   selected = preserve_order_dv("dv_ch", available_vars()) 
                   # function to preserver order if some were chosen before
      )
    )
  
  
  # Pick the independent variables
  output$iv <-
    renderUI(
      checkboxGroupInput("iv_ch",
                         "Independent variables",
                         choices = setdiff(c(chosen_vars(), exists_cleanssnames()),
                                           c(input$dv_ch, exists_sscorelist())),
                         selected = preserve_order("iv_ch")) # function to preserver order if some were chosen before
    )
  
  
  # Pick the variables that share CMV
  output$cmv <-
    renderUI(
      checkboxGroupInput("cmv_ch",
                         "Which variables are measured with the same method?",
                         choices = c(input$dv_ch, input$iv_ch),
                         selected = preserve_order("cmv_ch")) # function to preserver order if some were chosen before
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
  
  # observe({
  #   print(head(var_df()))
  #   print(dim(var_df()))
  # })
  
  observe({
    print(exists_cleanssnames())
    print(exists_sscorelist())
  })

  
  upd_sqpdf <-
    eventReactive(input$calc_model, {
      # Calculate the quality of sumscore of each name-variables pairs
      # and then bind them together with the sqp_df. The final output
      # is the sqp_df with the quality of the N sum scores created.
      q_sscore <- lapply(seq_along(exists_cleanssnames()), function(index) {
        iterative_sscore(unname(exists_cleanssnames()[index]), sscore_list()[[index]], sqp_df, var_df())
      })
      
      bind_rows(sqp_df, q_sscore)
    })
  
  observe({
    print(upd_sqpdf())
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
