##### PKGS AND SETTING OPTIONS #####
library(shiny)
library(shinycssloaders)
library(knitr)
library(kableExtra)
library(survey)
library(ggplot2)
library(lavaan)
library(sqpr)
library(countrycode)
library(rhandsontable)
library(jtools)
library(dplyr)

all_variables <-
  c("polintr",
    "ppltrst",
    "stfeco",
    "stfedu")
# "stfhlth",
# "stflife", 
# "trstplt",
# "trstprl")

all_variables_label <- 
  c("How interested in politics",
    "Most people can be trusted or you can't be too careful",
    "How satisfied with present state of economy in country",
    "State of education in country nowadays")
# "State of health services in country nowadays",
# "How satisfied with life as a whole", 
# "Trust in politicians",
# "Trust in country's parliament")

# Variables pasted together with labels
var_n_labels <- paste0(all_variables, ": ", all_variables_label)

ess_website <- "http://www.europeansocialsurvey.org"
path_login <- "/user/login"

# SQP credentials
sqp_login("asqme", "asqme")

#option to deal with lonegly PSUs
options(shiny.sanitize.errors = TRUE,
        survey.lonely.psu = "adjust")

# Text for errors when email is wrong or when 1 variable is selected as model
valid_email_error <- tags$span(style="color:red; font-size: 15px;", "Invalid email, please register at http://www.europeansocialsurvey.org/user/new")
minimum_var_error <- tags$span(style="color:red; font-size: 15px;", "Please select at least two variables for modeling.")
minimum_iv_error <- tags$span(style="color:red; font-size: 15px;", "Please select at least one independent variable.")

color_website <- "#AD1400"
#####

##### MAIN WRAPPER FOR THE PAGE WITH RED BANNER #####
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
#####

##### UI TAB TO LOG IN #####
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

#####

##### UI for all tabs #####
ui2 <- tabsetPanel(id = "menu",
                   tabPanel("Select country and model", value = "country_n_vars",
                            br(),
                            fluidRow(
                              column(3, selectInput("slid_cnt", "Pick a country", choices = all_countries)),
                              column(3, uiOutput("chosen_rounds")),
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
                            fluidRow(
                              column(3,
                                     sidebarPanel(
                                       actionButton('ins_sscore', 'Create sum score'),
                                       h6("Create more than one sum score by clicking again."),
                                       br(),
                                       br(),
                                       width = 10
                                     )
                              ),
                              column(6,
                                     mainPanel(
                                       div(id = 'placeholder'),
                                       tags$script("
                                     Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                                        Shiny.onInputChange(variableName, null);
                                     });"
                                       ),
                                       width = 12
                                     ),
                                     uiOutput('length_vars2')
                              ),
                              column(3,
                                     sidebarPanel(
                                       br(),
                                       uiOutput("new_sscore"),
                                       uiOutput("list_sscore", style = "margin-top: -25px;"),
                                       uiOutput("del_sscore"),
                                       br(),
                                       ess_button('def_model', "I'm done, I want to define my model"),
                                       width = 10
                                     )
                              )
                            )
                   ),
                   tabPanel("Define the model", value = "def_model",
                            br(),
                            fluidRow(column(3, uiOutput("dv")),
                                     column(3, uiOutput("iv")),
                                     column(5, uiOutput("cmv"))),
                            fluidRow(column(3, ""),
                                     column(3, uiOutput('length_vars3')), # three rows just to raise an error
                                     column(3, "")),
                            mainPanel(rHandsontableOutput("hot")),
                            ess_button("calc_model", "Create model")
                   ),
                   tabPanel("Create model", value = "cre_model",
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
#####

##### HELPER FUNS #####
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
  the_list[!names(the_list) %in% exclude]
}

#####


server <- function(input, output, session) {
  
  ##### Helper functions #####
  # Checks whether you are in `tab`
  is_tab <- function(tab) {
    is_specific_tab <- reactive({
      if (is.null(input$menu) || input$menu != tab) FALSE else TRUE
    })
    
    is_specific_tab()
  }
  #####
  
  ##### Logging in #####
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
  #####

  ##### First tab - choosing vars, rounds, cnts #####
  output$chosen_vars <-
    renderUI({
      checkboxGroupInput(
        'vars_ch',
        'Choose variables to use in the modeling',
        var_n_labels,
        width = '500px')
      
    })
  
  output$chosen_rounds <-
    renderUI({
      selectInput("slid_rounds",
                  "Pick a round",
                  choices = show_country_rounds(input$slid_cnt))
    })
  
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(
      session,
      'vars_ch',
      'Choose variables to use in the modeling',
      var_n_labels,
      choices = var_n_labels)
  })
  
  #####
  
  # This is a turning point. If the user clicks to define
  # sumscores, we want to be sure that there
  # are at least two variables chosen for the modeling.
  # If they are, we jumpt to sscore tabs. Otherwise,
  # raise a error in the UI.
  observeEvent(input$def_sscore, {
    if (length(input$vars_ch) < 2) {
      output$length_vars <- renderUI(p(minimum_var_error))
    } else {
      output$length_vars <- renderUI(p(""))
      updateTabsetPanel(session,
                        inputId = "menu",
                        selected = "def_sscore")
    }
  })
  
  # At this point, the user must've chosen some variabes so 
  # we remove the labels labels from the chosen variables
  chosen_vars <- reactive({
    gsub(":.*$", "", input$vars_ch)
  })
  
  # However, if the user changes the tab manually to def_sscore,
  # then check that we have the number of variables
  observeEvent(is_tab("def_sscore"), {
    # input$df_sscore at the beginning is null, that's why I check that the length
    # is different from zero.
    if (length(input$vars_ch) < 2 && (length(input$def_sscore) != 0 && input$def_sscore != 0)) {
      output$length_vars <- renderUI(p(minimum_var_error))
      output$length_vars2 <- renderUI(p(minimum_var_error))
    } else {
      output$length_vars <- renderUI(p(" "))
      output$length_vars2 <- renderUI(p(" "))
    }
  })
  
  
  ##### Second tab - define sscore #####
  output$del_sscore <- renderUI({actionButton('del_sscore', 'Delete sum score')})
  
  # And create a list of those names to delete
  output$list_sscore <- renderUI({selectInput('list_sscore',
                                              label = "If you'd like to delete a sum score you've created, which one?",
                                              choices = possible_ssnames())})
  
  # If the user clicks on insert sscore, then the chunk below manually
  # adds as many sscores as clicks.
  observeEvent(input$ins_sscore, {
    if (input$ins_sscore == 0) return(character())
    
    # If clicked once, update the button label so that the user knows he/she can
    # click there to add more sum scores after the first one
    if (input$ins_sscore == 1)  {
      updateActionButton(session, 'ins_sscore', 'Create another sum score') 
    }
    
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
    
    # Why `[1]`? Because once you use the sum score as a dependent variable
    # the line below matches both `ssanme*` and `dv_ch` so in any case
    # we only keep the index of the variable in `ssname*`
    semi_index <- which(input$list_sscore == temp_input)[1]
    
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
    # the splitlayout, they're still there. This is a loop that
    # sets both ssname* and sscore* for the chosen sscore to
    # NULL using a custom javascript message. The counterparty
    # of this is in the UI where I define the function to set the
    # names to NULL.
    
    session$sendCustomMessage(type = "resetValue", message = paste0("ssname", index_names))
    
    # Everything above was to delete the ssname. To delete sscore we have the index_names
    # of ssname which is, for example, 1. We just have to delete sscore1
    session$sendCustomMessage(type = "resetValue", message = paste0("sscore", index_names))
  })
  #####
  
  observeEvent(input$def_model, {
    if (length(input$vars_ch) < 1) {
      output$length_vars2 <- renderUI(p(minimum_var_error))
    } else {
      output$length_vars2 <- renderUI(p(""))
      # When the sumscores are ready, the user clicks
      # define model and we switch to the define model
      # tab to select dependent and independent variables
        updateTabsetPanel(session,
                          inputId = "menu",
                          selected = "def_model")
    }
  })

  ##### Extract all scores manually after defined #####
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
      
      # Return only the sscores which are not empty
      list_sscore[vapply(list_sscore, function(x) !is_empty(x), FUN.VALUE = logical(1))]
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
      vapply(clean_ssnames(), make.names, FUN.VALUE = character(1))
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
  #####
  
  ##### Third tab - Pick your dv/iv and cmv #####
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
  
  #####
  
  # If you still haven't added a independent variable, it doesn't
  # allow you to pass to the create model tab.
  observeEvent(input$calc_model, {
    if (length(input$iv_ch) < 1) {
      output$length_vars3 <- renderUI(p(minimum_iv_error))
    } else {
      output$length_vars3 <- renderUI(p(""))
      updateTabsetPanel(session,
                        inputId = "menu",
                        selected = "cre_model")
    }
  })
  
  
  ##### Download ESS data #####
  # Download data and create sumscores.
  # This is possible because we already have them
  # form the previous steps.
  upd_ess <-
    eventReactive(input$def_model, {
      # Choose country when user calculates model
      downloaded_rnd <- import_country(input$slid_cnt, as.numeric(input$slid_rounds))
      tmp_vars_weights <- c(all_variables, "pspwght")
      upd_ess <- recode_missings(downloaded_rnd[tmp_vars_weights]) %>% filter(complete.cases(.))
      upd_ess
    })
  
  var_df <- reactive({
    # If no sscore was defined, return the same df the above
    if (input$ins_sscore == 0) return(upd_ess())
    
    # We need to add new sscores to the origin df.
    # Calculate them here
    sscore <-
      lapply(sscore_list(), function(x) {
        rowSums(upd_ess()[x], na.rm = TRUE)
      })
    
    bind_cols(upd_ess(), as.data.frame(sscore, col.names = exists_cleanssnames()))
  })
  
  #####
  
  observe({
    # print(head(var_df()))
  })
  
  observe({
    print(exists_cleanssnames())
    print(sscore_list())
  })
  
  ##### Download SQP data #####
  
  # Download the question ids from the SQP API.
  sqp_df <- reactive({
    if (is.null(input$slid_cnt)) return(character())
    
    paste0("ESS Round ", input$slid_rounds) %>% 
      find_studies() %>% 
      .[['id']] %>% 
      find_questions(all_variables) %>% 
      filter(country_iso == countrycode(input$slid_cnt, origin = "country.name", destination = "iso2c"),
             tolower(short_name) %in% all_variables) %>% # in case some other questions come up
      slice(seq_along(all_variables)) # In case new languages by country come up in the future
  })

  upd_sqpdf <-
    eventReactive(input$calc_model, {
    
    sqp_id <- sqp_df() %>% pull(id)
    
    if (length(sqp_id) != length(all_variables)) {
      sqp_cols <- c("reliability", "validity", "quality")
      
      vars_missing <- setdiff(all_variables, tolower(sqp_df()$short_name))
      
      df_to_complete <- 
        runif(length(vars_missing) * length(sqp_cols)) %>% 
        matrix(length(vars_missing), ncol = length(sqp_cols)) %>%
        as.data.frame() %>%
        set_names(sqp_cols) %>% 
        mutate(question = vars_missing) %>% 
        select(question, sqp_cols)
      
      # output$hot <- renderRHandsontable(rhandsontable(df_to_complete, readOnly = FALSE, selectCallback = TRUE))
      # sqp_df <- input$hot_select
      sqp_df <- df_to_complete
    } else {
      sqp_df <- sqp_id() %>% get_estimates()
    }
    
    # Calculate the quality of sumscore of each name-variables pairs
    # and then bind them together with the sqp_df. The final output
    # is the sqp_df with the quality of the N sum scores created.
    q_sscore <- lapply(seq_along(exists_cleanssnames()), function(index) {
      iterative_sscore(unname(exists_cleanssnames()[index]),
                       sscore_list()[[index]],
                       sqp_df,
                       var_df())
    })
    
    # Because q_sscore only returns the new quality of each sum score,
    # we need to remove the variables that compose the sumscore manually
    # from the sqp data.
    bind_rows(filter(sqp_df, !question %in% exists_sscorelist()), q_sscore)
  })
  #####
  
  observe({
    print("This is sqp_df")
    # print(sqp_df())
  })
  

  ##### Define cor and cov with weights/adjustments measurement erro #####
  models_coef <- eventReactive(input$calc_model, {
    
    ch_vars <- c(input$dv_ch, input$iv_ch)
    
    # Unweighted correlation and covariance
    original_cor <- cor(var_df()[ch_vars])
    original_cov <- cov(var_df()[ch_vars])
    
    cor_cov <- cov.wt(var_df()[ch_vars], wt = var_df()$pspwght, cor = TRUE)
    
    # Weighted correlation and covariance
    corrected_cor <- cor_cov$cor
    corrected_cov <- cor_cov$cov
    
    
    # Subset chosen variables in sqp_df and
    # create quality estimate for the
    filtered_sqp <- upd_sqpdf()[upd_sqpdf()[[1]] %in% ch_vars, ]
    
    # Replace all NA's so that there's no error.
    filtered_sqp <- map_dfc(filtered_sqp, ~ {.x[is.na(.x)] <- 0; .x})
    
    print(corrected_cor)
    print(filtered_sqp)
    
    # Replace diagonal by multiplying it with the quality of each variable
    diag(corrected_cor) <- diag(corrected_cor) * filtered_sqp$quality
    
    # subtract the cmv from the observed correlation
    # Calculate the common method variance of some variables
    # and subtract that common method variance from the correlation
    # coefficients of the variables.
    
    # sqpr::sqp_cmv is a bit strange for programming
    # because it uses non standard evaluation. I defined
    # sqp_cmv_str in `globals.R` to be the same
    # but it accepts a string in cmv_vars instead of `...`
    # in sqpr::sqp_cmv
    
    if (length(input$cmv_ch) > 1) {
      corrected_cor <-
        cov2cor(
          as.matrix(
            sqp_cmv_str(x = corrected_cor,
                        sqp_data = filtered_sqp,
                        cmv_vars = input$cmv_ch)[-1]
          )
        )
    }
    
    list(original_cov = original_cov,
         original_cor = original_cor,
         # corrected_cov = corrected_cov,
         corrected_cor = corrected_cor)
  })
  #####
  
  ##### Prepare final table/plots #####
  # Final table
  output$model_table <-
    reactive({
      models_coef()$original_cor %>%
        as.data.frame() %>% 
        tibble::rownames_to_column() %>% 
        as_tibble() %>% 
        mutate_if(is.numeric, function(x) as.character(round(x, 3))) %>% 
        knitr::kable(format = "html") %>% 
        kableExtra::kable_styling("striped", full_width = FALSE)
    })
  
  # Final plot
  output$model_plot <-
    renderPlot({
      models_coef()$original_cor %>%
        corrr::as_cordf() %>% 
        corrr::network_plot()
    })
  #####
}
