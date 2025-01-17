library(essurvey)
library(readr)
library(purrr)
set_email("cimentadaj@gmail.com")

filter <- dplyr::filter
select <- dplyr::select

# Replace w/ available countries
all_countries <- c("Austria",
                   "Belgium",
                   "Bulgaria",
                   "Cyprus",
                   "Denmark",
                   "Estonia",
                   "Finland",
                   "France",
                   "Germany",
                   "Hungary",
                   "Ireland",
                   "Latvia",
                   "Netherlands",
                   "Norway",
                   "Poland",
                   "Portugal",
                   "Romania",
                   "Russian Federation",
                   "Slovakia",
                   "Slovenia",
                   "Spain",
                   "Sweden",
                   "Switzerland",
                   "Ukraine",
                   "United Kingdom")

countries_abbrv <- c("ES",
                     "DE",
                     "NL",
                     "FR",
                     "GB",
                     "IE")

all_ids <- c("cntry", "idno")

language_iso3 <- 
  read_csv("language_country_table.csv")

# ess_df <-
#   setNames(
#     map(all_countries, ~ recode_missings(import_country(.x, rounds = 6))),
#     all_countries
#   )

sqp_cmv_cor_str <- function (x, sqp_data, cmv_vars, cmv = NULL) {

  if (!(is.data.frame(x) | is.matrix(x))) {
    stop("`x` must be a correlation data frame or matrix")
  }
  if (length(cmv_vars) < 2) {
    stop("You need to supply at least two variables to calculate the Common Method Variance", 
         call. = FALSE)
  }

  sqp_data <- sqpr:::sqp_reconstruct(sqp_data[sqp_data[[1]] %in% cmv_vars, ], c("reliability", "validity"))
  x <- sqpr:::matrix2tibble(x)
  sqpr:::columns_present(x, sqp_data, cmv_vars)
  selected_rows <- sqp_data[[1]] %in% cmv_vars
  if (is.null(cmv)) cmv <- sqpr:::estimate_cmv(sqp_data[selected_rows, ])
  cmv <- prod(cmv)
  corrected_corr <- tibble::as_tibble(sqpr:::replace_matrix_cmv(x, cmv, cmv_vars))
  corrected_corr
}

# This function authenticates in the ess website to be able
# to download all other functions.
authenticate_ess <- function (ess_email, ess_website, path_login) {
  if (missing(ess_email)) {
    stop("`ess_email` parameter must be specified. Create an account at https://www.europeansocialsurvey.org/user/new")
  }
  values <- list(u = ess_email)
  url_login <- paste0(ess_website, path_login)
  authen <- httr::POST(url_login, body = values)
  check_authen <- safe_GET(url_login, query = values)
  authen_xml <- xml2::read_html(check_authen)
  error_node <- xml2::xml_find_all(authen_xml, "//p [@class=\"error\"]")
  if (length(error_node) != 0) {
    stop(xml2::xml_text(error_node), " Create an account at https://www.europeansocialsurvey.org/user/new")
  }
}



# Safe getter
safe_GET <- function(url, config = list(), ...) {
  httr::stop_for_status(httr::GET(url = url, config = config, ...))
}

sqp_cmv_cov_str <- function (x, sqp_data, cmv_vars, original_data, cmv = NULL) {
  
  if (!(is.data.frame(x) | is.matrix(x))) {
    stop("`x` must be a covariance data frame or matrix")
  }
  if (length(cmv_vars) < 2) {
    stop("You need to supply at least two variables to calculate the Common Method Variance", 
         call. = FALSE)
  }

  sqp_data <- sqpr:::sqp_reconstruct(sqp_data[sqp_data[[1]] %in% cmv_vars, ], c("reliability", "validity"))  
  x <- sqpr:::matrix2tibble(x)
  sqpr:::columns_present(x, sqp_data, cmv_vars)
  selected_rows <- sqp_data[[1]] %in% cmv_vars
  if (is.null(cmv)) 
    cmv <- sqpr:::estimate_cmv(sqp_data[selected_rows, ])
  if (!all(cmv_vars %in% names(original_data))) {
    stop("Variables ", paste(cmv_vars[!cmv_vars %in% names(original_data)], 
                             collapse = ", "), " are not preset in `original_data`")
  }
  cmv <- prod(cmv, vapply(original_data[cmv_vars], stats::sd,  na.rm = TRUE, FUN.VALUE = numeric(1)))
  corrected_cov <- tibble::as_tibble(sqpr:::replace_matrix_cmv(x, cmv, cmv_vars))
  corrected_cov
}




# Same as sqp_sscore but good for programming with. vars_names accepts a vector rather than ...
sqp_sscore_str <- function (sqp_data, df, new_name, vars_names, wt = NULL, drop = TRUE) {
  sqp_data <- sqpr:::sqp_reconstruct(sqp_data)
  summary_name <- new_name
  vars_not_matched <- !vars_names %in% names(df)
  if (any(vars_not_matched)) {
    stop("One or more variables are not present in `df`: ", 
         paste0(vars_names[vars_not_matched], collapse = ", "), 
         call. = FALSE)
  }
  vars_not_matched <- !vars_names %in% sqp_data[[1]]
  if (any(vars_not_matched)) {
    stop("One or more variables are not present in `sqp_data`: ", 
         paste0(vars_names[vars_not_matched], collapse = ", "), 
         call. = FALSE)
  }
  the_vars <- df[vars_names]
  if (!all(purrr::map_lgl(the_vars, is.numeric))) {
    stop(paste0(vars_names, collapse = ", "), " must be numeric variables in `df`")
  }
  if (ncol(the_vars) < 2) 
    stop("`df` must have at least two columns")
  rows_to_pick <- sqp_data[[1]] %in% vars_names
  sqp_scores <- sqp_data[rows_to_pick, sqpr:::sqp_env$sqp_columns]
  if (anyNA(sqp_scores)) {
    stop("`sqp_data` must have non-missing values at variable/s: ", 
         paste0(sqpr:::sqp_env$sqp_columns, collapse = ", "))
  }
  new_estimate <- sqpr:::columns_sqp("quality", sqpr:::estimate_sscore(sqp_scores, 
                                                                       the_vars, wt = wt))
  additional_rows <- sqpr:::generic_sqp(summary_name, new_estimate)
  if (!drop) {
    rows_to_pick <- rep(TRUE, length(rows_to_pick))
  }
  else {
    rows_to_pick <- !rows_to_pick
  }
  combined_matrix <- dplyr::bind_rows(sqp_data[rows_to_pick, ], additional_rows)
  correct_order <- c("question", sqpr:::sqp_env$sqp_columns)
  new_order <- combined_matrix[c(correct_order, setdiff(names(combined_matrix), 
                                                        correct_order))]
  final_df <- sqpr:::sqp_reconstruct(new_order)
  final_df
}
