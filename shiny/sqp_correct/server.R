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
library(tidyr)
library(dplyr)
library(glue)

ess_website <- "http://www.europeansocialsurvey.org"
path_login <- "/user/login"

# SQP credentials
sqp_login("asqme", "asqme")

# option to deal with lonegly PSUs
options(
  shiny.sanitize.errors = TRUE,
  survey.lonely.psu = "adjust"
)


# Because countrycode has different long names for different iso2c UK names
custom_match <- c(`GB` = "United Kingdom", `UK` = "United Kingdom")

# Text for errors when email is wrong or when 1 variable is selected as model
valid_email_error <- tags$span(style = "color:red; font-size: 15px;", "Invalid email, please register at http://www.europeansocialsurvey.org/user/new")
minimum_var_error <- tags$span(style = "color:red; font-size: 15px;", "Please select at least two variables for modeling.")
minimum_iv_error <- tags$span(style = "color:red; font-size: 15px;", "Please select at least two variables.")

missing_est_error <- tags$span(style = "color:red; font-size: 15px;", "Please complete all cells with values between 0 and 1. If no information is available, add a 99 and note that if no reliability and/or validity are provided, the common method variance cannot be taken into account. If you set quality to 99, then correction for measurement error is not possible.")

minimum_cmv_vars <- tags$span(style = "color:red; font-size: 15px;", "You must choose no variables or either 2 or more variables that are measured with the same method")

missing_quality <- tags$span(style = "color:red; font-size: 15px;", "The quality column must be non-missing and should have a value between 0 and 1. Otherwise, correction for measurement error is not possible.")

ess_signup_url <- a("https://www.europeansocialsurvey.org/user/new",
  href = "https://www.europeansocialsurvey.org/user/new",
  target = "_blank"
)

ess_weight_url <- a("European Social Survey Round 8 Weighting Strategy",
  href = "https://www.europeansocialsurvey.org/docs/round8/methods/ESS8_weighting_strategy.pdf",
  target = "_blank"
)

sqp_url <- a("sqp.upf.edu",
  href = "http://sqp.upf.edu/",
  target = "_blank"
)

color_website <- "#AD1400"

empty_sqp <- tibble(
  question = character(),
  reliability = numeric(),
  validity = numeric(),
  quality = numeric()
)
#####

##### MAIN WRAPPER FOR THE PAGE WITH RED BANNER #####
main_page <- function(...) {
  div(
    id = "fluidp",
    fluidPage(
      img(
        id = "ess_logo",
        src = "http://www.europeansocialsurvey.org/common/ess_eric/img/ess-logo-top.png"
      ),
      img(
        id = "sqp_logo",
        src = "SQP_logo_big_white.png",
        width = "4.7%",
        height = "4.7%"
      ),
      h3(id = "header_title", "Correction for measurement and sampling error app"),
      br(),
      br(),
      ...
    ),
    tags$style(
      type = "text/css",
      paste0(
        "#fluidp {
                 background-color: ", color_website, ";
                 height: 70px;
                 }
                 #ess_logo {
                 margin-top: 15px;
                 position: relative
                 }
                 #sqp_logo {
                 margin-top: 8px;
                 float: right;
                 position: relative
                 }
                 #header_title {
                 margin-top: -35px;
                 color: white;
                 text-align: center;
                 position: relative
                 }"
      )
    )
  )
}
#####

##### UI TAB TO LOG IN #####
ui1 <- tagList(
  div(
    id = "welcome_msg",
    h3("Welcome to the correction for measurement and sampling app of the European Social Survey!", align = "center")
  ),
  div(
    id = "login",
    textInput("essemail", "Sign in with your registered ESS email address:"),
    # passwordInput("passwd", "Password"),
    uiOutput("emailValid"),
    actionButton("Login", "Log in"),
    br(),
    br(),
    "If you haven’t registered yet with the ESS, please do that first: ",
    ess_signup_url
  ),
  tags$style(
    type = "text/css",
    "
     #welcome_msg {
        position: absolute;
        top: 40%;
        margin-top: -120px;
        margin-left: 450px;
     }
     #login {
        font-size: 14px;
        text-align: left;
        position: absolute;
        top: 50%;
        left: 50%;
        margin-top: -100px;
        margin-left: -170px;
        width: 25%;
     }"
  )
)

ess_button <- function(id, label, color = color_website) {
  actionButton(id,
    label,
    style = paste0("color: #fff;
                              background-color:", color, ";
                              border-color:", color)
  )
}

#####

##### UI for all tabs #####
ui2 <- tabsetPanel(
  id = "menu",
  tabPanel("Select country and model",
    value = "country_n_vars",
    br(),
    fluidRow(
      column(3, selectInput("slid_cnt", "Pick a country or several",
        choices = all_countries,
        multiple = TRUE
      )),
      column(3, uiOutput("chosen_rounds"))
    ),
    "After having chosen a country and round, this application downloads the data from the ESS. If
several countries are chosen, the application will only provide those variables
that are available across countries for that specific round. The available variables can then be chosen
from a box that will appear next:",
    br(),
    br(),
    uiOutput("chosen_vars"),
    uiOutput("length_vars"),
    ess_button("def_model", "Define quality of variables"),
    br(),
    br(),
    "Note that your data is already corrected for sampling error at this point. The application
follows the", ess_weight_url, "(page 2):
“The use of pspwght is recommended in any analyses that aim to draw inferences related to
the general population of a country or when comparisons between countries are performed.
Instead, (pspwght*pweight) should be used for analyses in which countries are combined to
represent a larger geographical region (e.g. comparing “Nordic countries” with “Mediterranean
countries”, or producing estimates for “all ESS countries”)."
  ),
  # tabPanel("Create sum scores", value = "def_sscore",
  #          br(),
  #          p("Would you like to create additional sum score variables?
  #            Sum scores are the addition of several variables into one single
  #            variable. click on 'Create sum score' to create your sum score."),
  #          br(),
  #          br(),
  #          fluidRow(
  #            column(3,
  #                   sidebarPanel(
  #                     actionButton('ins_sscore', 'Create sum score'),
  #                     h6("Create more than one sum score by clicking again."),
  #                     br(),
  #                     br(),
  #                     width = 10
  #                   )
  #            ),
  #            column(6,
  #                   mainPanel(
  #                     div(id = 'placeholder'),
  #                     tags$script("
  #                   Shiny.addCustomMessageHandler('resetValue', function(variableName) {
  #                      Shiny.onInputChange(variableName, null);
  #                   });"
  #                     ),
  #                     width = 12
  #                   ),
  #                   uiOutput('length_vars2')
  #            ),
  #            column(3,
  #                   sidebarPanel(
  #                     br(),
  #                     uiOutput("new_sscore"),
  #                     uiOutput("list_sscore", style = "margin-top: -25px;"),
  #                     uiOutput("del_sscore"),
  #                     br(),
  #                     ess_button('def_model', "I'm done, I want to define my model"),
  #                     width = 10
  #                   )
  #            )
  #          )
  # ),
  tabPanel("Define quality of variables",
    value = "def_model",
    br(),
    p("This tab downloads quality information from the Survey Quality Predictor (SQP2.1,", sqp_url, ", Saris 2015) for all selected variables. When the download is ready, a table will pop up with all available SQP authorized quality predictions. If any of the quality information is missing, you need to add it before the correlation/covariance matrices can be corrected for measurement error. You can go to SQP2.1:", sqp_url, "and create or use a user quality prediction, or alternatively, you can introduce measurement quality from any other source manually in the input table. These values have to be between 0 and 1 or 99, in case you don't have any information. In order to correct for any measurement error you need at least information on the quality of the question. In order to account for the common method variance for those variables which are measured with the same method, you need information about the reliability and validity, as defined by Saris and Andrews (1991)."),
    br(),
    br(),
    uiOutput("sqp_table_output"),
    br(),
    br(),
    uiOutput("cmv"),
    br(),
    ess_button("calc_model", "Output"),
    uiOutput("length_vars3"), # three rows just to raise an error
    uiOutput("missing_est_error"),
    br(),
    p("Reference:"),
    p("Saris, W. E., and F. M. Andrews. 1991. “Evaluation of Measurement Instruments Using a
Structural Modeling Approach.” In Measurement Errors in Surveys, eds. P. P. Biemer, R.M.
Groves, N.A. Lyberg, L. E. Mathiowetz, and S. Sudman. New York: JohnWiley &amp; Sons, Inc.,
575–97."),
    p("Saris, W.E. 2015. “Survey Quality Predictor 2 [Online Software]. Version 2.1.” Retrieved from
sqp.upf.edu")
  ),
  tabPanel("Output", value = "cre_model", uiOutput("cre_model"))
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

between_0_1 <- function(df) {
  res <- keep(df, is.numeric) %>% reduce(`c`)
  valid_numbers <- (between(res, 0, 1) | res == 99) & !is.na(res)

  all(valid_numbers)
}

# For calculating sscore of quality scores and only returnig the row of
# the newly created qualtiy score
iterative_sscore <- function(x, y, sqp_df, data_df) {
  sqp_sscore_str(
    sqp_data = sqp_df,
    df = data_df,
    new_name = x,
    vars_names = y
  ) %>%
    filter(question == x)
}

# for excluding a named slot from a list
pick_list <- function(exclude, the_list) {
  the_list[!names(the_list) %in% exclude]
}

#####

server <- function(input, output, session) {

  ##### Helper functions in Shiny #####
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

  # # Change UI based on whether the user is logged in or not.
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

  non_na_columns <- function(x) select_if(x, ~ !all(is.na(.x)))

  observe({
    print(input$slid_rounds)
  })

  tmp_ess <-
    eventReactive(input$slid_rounds, {
      # Choose country when user calculates model
      downloaded_rnd <- map(input$slid_cnt, ~ non_na_columns(import_country(.x, as.numeric(req(input$slid_rounds)))))
      downloaded_rnd
    })

  keep_common_columns <- function(x) map(x, ~ select_if(.x, is.numeric) %>% names()) %>% reduce(intersect)

  ##### First tab - choosing vars, rounds, cnts #####
  output$chosen_vars <-
    renderUI({
      selectInput(
        "vars_ch",
        "Choose variables",
        choices = setdiff(keep_common_columns(tmp_ess()), c("essround", "idno")),
        multiple = TRUE,
        width = "500px"
      )
    })

  output$chosen_rounds <-
    renderUI({
      selectizeInput(
        "slid_rounds", "Pick a round",
        choices = reduce(map(req(input$slid_cnt), show_country_rounds), intersect),
        options = list(
          placeholder = "Please select an option below",
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    })
  #####

  upd_ess <- reactive({
    downloaded_rnd <- tmp_ess()
    tmp_vars_weights <- c(input$vars_ch, "pspwght", "pweight")
    upd_ess <- map(downloaded_rnd, ~ recode_missings(.x[tmp_vars_weights]) %>% drop_na())
    upd_ess
  })

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
        selected = "def_model"
      )
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
  output$del_sscore <- renderUI({
    actionButton("del_sscore", "Delete sum score")
  })

  # And create a list of those names to delete
  output$list_sscore <- renderUI({
    selectInput("list_sscore",
      label = "If you'd like to delete a sum score you've created, which one?",
      choices = possible_ssnames()
    )
  })

  # If the user clicks on insert sscore, then the chunk below manually
  # adds as many sscores as clicks.
  observeEvent(input$ins_sscore, {
    if (input$ins_sscore == 0) {
      return(character())
    }

    # If clicked once, update the button label so that the user knows he/she can
    # click there to add more sum scores after the first one
    if (input$ins_sscore == 1) {
      updateActionButton(session, "ins_sscore", "Create another sum score")
    }

    # When user clicks insert, add
    # the sscore name and variables to the ui
    whole_html <-
      splitLayout(
        tags$div(
          id = paste0("splitlayout", input$ins_sscore),
          fluidRow(
            column(6, textInput(paste0("ssname", input$ins_sscore), "Name of sum score")),
            column(6, checkboxGroupInput(
              inputId = paste0("sscore", input$ins_sscore),
              label = "Variables that compose the sum score",
              choices = input$vars_ch
            ))
          ),
          # To remove the horizontall scroll bar
          tags$style(
            type = "text/css",
            paste0(
              paste0("#splitlayout", input$ins_sscore),
              " {overflow-x: hidden}"
            )
          )
        ),
        cellWidths = c("100%")
      )
    # Interactively add a sumscore to the UI
    insertUI(selector = "#placeholder", ui = whole_html)
  })

  # Grab the names of the sscore reactively, that is, whenever they are being written
  possible_ssnames <- reactive({
    unname(unlist(lapply(
      grep("^ssname", names(input), value = TRUE),
      function(x) if (!is_empty(x)) input[[x]] else ""
    )))
  })

  ## Deleting sum scores
  observeEvent(input$del_sscore, {
    if (input$del_sscore == 0 | is_empty(input$list_sscore)) {
      return(character())
    }

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
    if (length(input$vars_ch) < 2) {
      output$length_vars <- renderUI(p(minimum_iv_error))
    } else {
      output$length_vars <- renderUI(p(""))

      # Because each user can go back to choose a new country/round
      # whenever the user clicks to define the model, the
      # hot table is thus resetted so that new values
      # can be added for these new country/rounds/variables
      session$sendCustomMessage(type = "resetValue", message = "hot")
      output$hot <- NULL
      # When the sumscores are ready, the user clicks
      # define model and we switch to the define model
      # tab to select dependent and independent variables
      updateTabsetPanel(session,
        inputId = "menu",
        selected = "def_model"
      )
    }
  })

  ##### Extract all scores manually after defined #####
  clean_ssnames <-
    eventReactive(input$def_model, {
      print((is.null(input$ins_sscore) || input$ins_sscore == 0))
      if (is.null(input$ins_sscore) || input$ins_sscore == 0) {
        return(character())
      }

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
      if (is.null(input[[equation_side]])) {
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
      if (is.null(input[[equation_side]])) {
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
        choices = setdiff(
          c(chosen_vars(), exists_cleanssnames()),
          c(input$dv_ch, exists_sscorelist())
        ),
        selected = chosen_vars()
      ) # to preserver order if some were chosen before
    )


  # Pick the variables that share CMV
  output$cmv <-
    renderUI(
      checkboxGroupInput("cmv_ch",
        "Which variables are measured with the same method?",
        choices = chosen_vars()
      )
    )

  #####

  ##### Download ESS data #####
  # Download data and create sumscores.
  # This is possible because we already have them
  # form the previous steps.

  var_df <- reactive({
    print("Temporary ess")
    print(tmp_ess())

    print("Updated ESS")
    print(upd_ess())
    # If no sscore was defined, return the same df the above
    if (is.null(input$ins_sscore) || input$ins_sscore == 0) {
      return(upd_ess())
    }

    # We need to add new sscores to the origin df.
    # Calculate them here
    map(upd_ess(), ~ {
      print(.x)

      sscore <-
        map(sscore_list(), function(sscore_cols) rowSums(.x[sscore_cols], na.rm = TRUE))

      bind_cols(.x, as.data.frame(sscore, col.names = exists_cleanssnames()))
    })
  })

  #####

  observe({
    print(exists_cleanssnames())
    print(sscore_list())
    print("Value of input$hot is")
    print(req(input$hot))
  })

  eventReactive(input$def_model, {
    output$cre_model <- renderUI(HTML(""))
  })

  ## Download SQP data #####
  ## input <- list(slid_cnt = c("Austria", "Spain", "France"),
  ##               slid_rounds = 1,
  ##               chosen_vars = c("rlgdgr", "pplfair", "uempli"),
  ##               cmv_ch = c("rlgdgr", "pplfair", "uempli"))

  # Download the question ids from the SQP API.
  sqp_df <- reactive({
    print("Downloading sqp")
    on.exit(print("Downloaded"))
    if (is.null(input$slid_cnt)) {
      return(character())
    }

    main_cnt_lang <- filter(language_iso3, country %in% input$slid_cnt)
    country_abbrv <- countrycode(main_cnt_lang$country, origin = "country.name", destination = "iso2c")
    country_lang <- paste0(country_abbrv, "_", main_cnt_lang %>% pull(iso3))

    intm <-
      paste0("ESS Round ", input$slid_rounds) %>%
      find_studies() %>%
      .[["id"]] %>%
      find_questions(paste0("^", chosen_vars(), "$")) %>%
      mutate(
        country_q = countrycode(country_iso, origin = "iso2c", destination = "country.name", custom_match = custom_match),
        country_q = paste0(country_q, "_", tolower(short_name))
      )

    res_sqp <-
      intm %>%
      filter(paste0(country_iso, "_", language_iso) %in% country_lang)

    estimates_sqp <- tryCatch(get_estimates(res_sqp$id), error = function(e) empty_sqp)

    list(res_sqp = res_sqp, estimates_sqp = estimates_sqp)
  })

  # If you still haven't added a independent variable, it doesn't
  # allow you to pass to the create model tab.
  observeEvent(input$def_model, {
    print(paste0("^", input$cmv_ch, "$"))

    output$hot <- renderRHandsontable({
      cnt_df <-
        sqp_df()$res_sqp %>%
        transmute(country = countrycode(country_iso, origin = "iso2c", destination = "country.name", custom_match = custom_match))

      sqp_id <- sqp_df()$res_sqp %>% pull(id)
      res <- bind_cols(cnt_df, sqp_df()$estimates_sqp)
      print(res)

      output$length_vars3 <- renderUI(p(""))
      difference_in_vars <- length(sqp_id) != (length(chosen_vars()) * length(input$slid_cnt))

      sqp_cols <- c("reliability", "validity", "quality")
      country_q <- unlist(map(input$slid_cnt, paste0, "_", chosen_vars()))

      vars_missing <- setdiff(country_q, sqp_df()$res_sqp$country_q)

      df_to_complete <-
        NA_real_ %>%
        matrix(length(vars_missing), ncol = length(sqp_cols)) %>%
        as.data.frame() %>%
        set_names(sqp_cols) %>%
        mutate(question = vars_missing) %>%
        separate(question, c("country", "question"), sep = "_") %>%
        select(country, question, sqp_cols)

      df_to_complete <-
        bind_rows(res, df_to_complete)

      df_to_complete <-
        df_to_complete %>%
        group_split(country) %>%
        map_dfr(~ slice(., na.omit(match(chosen_vars(), .$question))))
      
      rhandsontable(df_to_complete, selectCallback = TRUE) %>%
        hot_col("question", readOnly = TRUE) %>%
        hot_col("country", readOnly = TRUE)
    })

    output$sqp_table_output <-
      renderUI(
        withSpinner(
          tagList(
            rHandsontableOutput("hot")
          ),
          color = color_website
        )
      )
  })

  observeEvent(input$calc_model, {
    print(hot_to_r(input$hot))
    print(class(hot_to_r(input$hot)))
    hot_available <- !is.null(input$hot)
    hot_any_missing <- anyNA(hot_to_r(input$hot))
    length_cmv <- length(input$cmv_ch)
    valid_numbers <- between_0_1(hot_to_r(input$hot))
    quality_input <- hot_to_r(input$hot)$quality
    any_99_quality <- any(quality_input == 99)
    cmv_hot_df <- hot_to_r(input$hot) %>% filter(question %in% input$cmv_ch)
    missing_info_cmv <- any(unlist(map(cmv_hot_df, ~ .x == 99)))

    if (length_cmv == 1) {
      output$missing_est_error <- renderUI(p(minimum_cmv_vars))
    } else if (hot_any_missing || !valid_numbers) {
      output$missing_est_error <- renderUI(p(missing_est_error))
    } else if (anyNA(quality_input) || any_99_quality) {
      output$missing_est_error <- renderUI(p(missing_quality))
    } else if (length_cmv == 0 || (length_cmv > 1 && missing_info_cmv)) {
      print("Passed through replace quality only")
      output$missing_est_error <- NULL
      apply_cmv <<- FALSE


      upd_sqpdf <<- {
        sqp_df <-
          hot_to_r(input$hot) %>%
          split(.$country) %>%
          map(~ .[, -1])

        map2(var_df(), sqp_df, ~ {

          # Calculate the quality of sumscore of each name-variables pairs
          # and then bind them together with the sqp_df. The final output
          # is the sqp_df with the quality of the N sum scores created.
          q_sscore <- lapply(seq_along(exists_cleanssnames()), function(index) {
            iterative_sscore(
              unname(exists_cleanssnames()[index]),
              sscore_list()[[index]],
              .y,
              .x
            )
          })

          # Because q_sscore only returns the new quality of each sum score,
          # we need to remove the variables that compose the sumscore manually
          # from the sqp data.
          bind_rows(filter(.y, !question %in% exists_sscorelist()), q_sscore)
        })
      }

      spinner_wrapper <- function(plot_title) {
        withSpinner(tagList(tableOutput(plot_title)), color = color_website)
      }

      output$cre_model <-
        renderUI(
          mainPanel(
            br(),
            uiOutput('top_title'),
            br(),
            fluidRow(
              column(width = 5, tableOutput("original_cor")),
              column(width = 5, tableOutput("corrected_cor"))
            ),
            fluidRow(
              column(width = 5, tableOutput("original_cov")),
              column(width = 5, tableOutput("corrected_cov"))
            ),
            downloadButton("downloadData", "Download as csv")
          )
        )

      output$downloadData <-
        downloadHandler(
          filename = function() "corrected_matrices.zip",
          content = function(file) {
            write_csv_zip <- function(.x, .y) {
              .x <- as_tibble(.x, rownames = "rowname")
              path <- file.path(tempdir(), paste0(.y, ".csv"))
              write_csv(.x, path)
              path
            }

            where <- imap_chr(models_coef(), write_csv_zip)
            utils::zip(file, where, flags = "-j")
          },
          contentType = "application/zip"
        )


      updateTabsetPanel(session,
        inputId = "menu",
        selected = "cre_model"
      )
    } else if (length_cmv > 1 && !missing_info_cmv) {
      print("Passed through replace quality and apply CMV")
      apply_cmv <<- TRUE
      output$missing_est_error <- NULL

      upd_sqpdf <<- {
        sqp_df <-
          hot_to_r(input$hot) %>%
          split(.$country) %>%
          map(~ .[, -1])

        map2(var_df(), sqp_df, ~ {

          # Calculate the quality of sumscore of each name-variables pairs
          # and then bind them together with the sqp_df. The final output
          # is the sqp_df with the quality of the N sum scores created.
          q_sscore <- lapply(seq_along(exists_cleanssnames()), function(index) {
            iterative_sscore(
              unname(exists_cleanssnames()[index]),
              sscore_list()[[index]],
              .y,
              .x
            )
          })

          # Because q_sscore only returns the new quality of each sum score,
          # we need to remove the variables that compose the sumscore manually
          # from the sqp data.
          bind_rows(filter(.y, !question %in% exists_sscorelist()), q_sscore)
        })
      }

      spinner_wrapper <- function(plot_title) {
        withSpinner(tagList(tableOutput(plot_title)), color = color_website)
      }

      output$cre_model <-
        renderUI(
          mainPanel(
            br(),
            uiOutput('top_title'),
            br(),
            fluidRow(
              column(width = 5, tableOutput("original_cor")),
              column(width = 5, tableOutput("corrected_cor"))
            ),
            fluidRow(
              column(width = 5, tableOutput("original_cov")),
              column(width = 5, tableOutput("corrected_cov"))
            ),
            downloadButton("downloadData", "Download as csv")
          )
        )

      output$downloadData <-
        downloadHandler(
          filename = function() "corrected_matrices.zip",
          content = function(file) {
            write_csv_zip <- function(.x, .y) {
              .x <- as_tibble(.x, rownames = "rowname")
              path <- file.path(tempdir(), paste0(.y, ".csv"))
              write_csv(.x, path)
              path
            }

            where <- imap_chr(models_coef(), write_csv_zip)
            utils::zip(file, where, flags = "-j")
          },
          contentType = "application/zip"
        )

      updateTabsetPanel(session,
        inputId = "menu",
        selected = "cre_model"
      )
    }
  })

  #####


  ##### Define cor and cov with weights/adjustments measurement erro #####
  models_coef <- reactive({
    input$cmv_ch
    ch_vars <- chosen_vars()

    # Unweighted correlation and covariance
    original_cor <- map(var_df(), ~ cor(.x[ch_vars]))
    original_cov <- map(var_df(), ~ cov(.x[ch_vars]))

    print("original cor")
    print(original_cor)
    print("original cov")
    print(original_cov)

    cor_cov <- map(var_df(), ~ {
      print("var_df data frames")
      print(.x)
      cov.wt(.x[ch_vars], wt = .x$pspwght, cor = TRUE)
    })

    # Weighted correlation and covariance
    corrected_cor <- map(cor_cov, "cor")
    corrected_cov <- map(cor_cov, "cov")

    print("corrected cor")
    print(corrected_cor)
    print("corrected cov")
    print(corrected_cov)

    print("filtered sqp")
    print(upd_sqpdf)

    # Subset chosen variables in sqp_df and
    # create quality estimate for the
    filtered_sqp <- map(upd_sqpdf, ~ {
      filter(.x, question %in% ch_vars) %>%
        slice(match(question, ch_vars))
    })

    print(filtered_sqp)

    # Replace all NA's so that there's no error.
    filtered_sqp <- map(filtered_sqp, ~ map_dfc(., ~ {
      .x[is.na(.x)] <- 0
      .x
    }))

    print(filtered_sqp)

    # Replace diagonal by multiplying it with the quality of each variable
    corrected_cor <-
      map2(corrected_cor, filtered_sqp, ~ {
        diag(.x) <- diag(.x) * .y$quality
        .x
      })

    # Replace diagonal by multiplying it with the quality of each variable
    corrected_cov <-
      map2(corrected_cov, filtered_sqp, ~ {
        diag(.x) <- diag(.x) * .y$quality
        .x
      })

    # subtract the cmv from the observed correlation
    # Calculate the common method variance of some variables
    # and subtract that common method variance from the correlation
    # coefficients of the variables.

    # sqpr::sqp_cmv is a bit strange for programming
    # because it uses non standard evaluation. I defined
    # sqp_cmv_str in `globals.R` to be the same
    # but it accepts a string in cmv_vars instead of `...`
    # in sqpr::sqp_cmv

    print("This is the filtered sqp with all values")
    print(filtered_sqp)
    print("This is the weighted correlation")
    print(corrected_cor)
    print("This is the head of the original data")
    print(var_df())

    
    if (apply_cmv) {
      corrected_cor <-
        map2(corrected_cor, filtered_sqp, ~ {
          .x %>%
            sqp_cmv_cor_str(.y, cmv_vars = input$cmv_ch) %>%
            .[-1] %>%
            as.matrix()
        })

      corrected_cov <-
        pmap(list(corrected_cov, filtered_sqp, var_df()), ~ {
          ..1 %>%
            sqp_cmv_cov_str(..2, cmv_vars = input$cmv_ch, original_data = ..3) %>%
            .[-1] %>%
            as.matrix()
        })
    }

    print("This is the corrected cor previous to cov2cor")
    print(corrected_cor)
    
    corrected_cor <- map(corrected_cor, cov2cor)
    print("This is the corrected cor post cov2cor")
    print(corrected_cor)

    denom_adj <- length(original_cov)
    if (denom_adj > 1) {
      country_pweights <- map_dbl(var_df(), ~ .x[["pweight"]] %>% unique())
      corrected_cor <- map2(corrected_cor, country_pweights, `*`)
      corrected_cov <- map2(corrected_cov, country_pweights, `*`)
      denom_adj <- sum(country_pweights)

      corrected_cov <- reduce(corrected_cov, `+`) / denom_adj
      corrected_cor <- reduce(corrected_cor, `+`) / denom_adj

      diag(corrected_cor) <- 1
    } else {
      corrected_cov <- reduce(corrected_cov, `+`) / length(corrected_cov)
      corrected_cor <- reduce(corrected_cor, `+`) / length(corrected_cor)
    }

    rownames(corrected_cov) <- rownames(original_cov[[1]])
    rownames(corrected_cor) <- rownames(original_cor[[1]])

    list(
      original_cov = reduce(original_cov, `+`) / length(original_cov),
      original_cor = reduce(original_cor, `+`) / length(original_cor),
      corrected_cov = corrected_cov,
      corrected_cor = corrected_cor
    )
  })
  #####


  # Titles

  correction_text <-
    reactive({
      length_cmv <- length(input$cmv_ch)
      cmv_hot_df <- hot_to_r(input$hot) %>% filter(question %in% input$cmv_ch)
      missing_info_cmv <- any(unlist(map(cmv_hot_df, ~ .x == 99)))

      only_quality <- length_cmv == 0 || (length_cmv > 1 && missing_info_cmv)
      both_quality_cmv <- length_cmv > 1 && !missing_info_cmv

      if (only_quality) {
        res <- "quality"
        res_two <- "only for quality. This is due to the missing values in the reliability and validity columns or because you specified no shared common method among the variables."
      } else if (both_quality_cmv) {
        res <- "quality and CMV"
        res_two <- "for quality and CMV successfully"
      }

      list(res = res, res_two = res_two)
    })

  output$top_title <-
    renderUI({
        h4(glue("Your adjusted correlation and covariance matrices have been weighted and adjusted {correction_text()$res_two}"))
    })

  all_titles <-
    reactive({
      list(
        cap_original_cor = glue("Original correlation matrix unweighted and without correction for measurement error for selected countr{if (length(input$slid_cnt) > 1) 'ies' else 'y'}"),
        cap_original_cov = glue("Original covariance matrix unweighted and without correction for measurement error for selected countr{if (length(input$slid_cnt) > 1) 'ies' else 'y'}"),
        cap_corrected_cor = glue("Adjusted correlation matrix weighted and corrected for {correction_text()$res} for selected countr{if (length(input$slid_cnt) > 1) 'ies' else 'y'}"),
        cap_corrected_cov = glue("Adjusted covariance matrix weighted and corrected for {correction_text()$res} for selected countr{if (length(input$slid_cnt) > 1) 'ies' else 'y'}")
      )
    })


  ##### Prepare final table/plots #####
  # Final table
  output$original_cor <-
    reactive({
      models_coef()$original_cor %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        as_tibble() %>%
        mutate_if(is.numeric, function(x) as.character(round(x, 3))) %>%
        knitr::kable(
          format = "html",
          caption = all_titles()$cap_original_cor
        ) %>%
        kableExtra::kable_styling("striped", full_width = TRUE)
    })

  output$corrected_cor <-
    reactive({
      models_coef()$corrected_cor %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        as_tibble() %>%
        mutate_if(is.numeric, function(x) as.character(round(x, 3))) %>%
        knitr::kable(
          format = "html",
          caption = all_titles()$cap_corrected_cor
        ) %>%
        kableExtra::kable_styling("striped", full_width = TRUE)
    })

  output$original_cov <-
    reactive({
      models_coef()$original_cov %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        as_tibble() %>%
        mutate_if(is.numeric, function(x) as.character(round(x, 3))) %>%
        knitr::kable(
          format = "html",
          caption = all_titles()$cap_original_cov
        ) %>%
        kableExtra::kable_styling("striped", full_width = TRUE)
    })

  output$corrected_cov <-
    reactive({
      models_coef()$corrected_cov %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        as_tibble() %>%
        mutate_if(is.numeric, function(x) as.character(round(x, 3))) %>%
        knitr::kable(
          format = "html",
          caption = all_titles()$cap_corrected_cov
        ) %>%
        kableExtra::kable_styling("striped", full_width = TRUE)
    })
}
