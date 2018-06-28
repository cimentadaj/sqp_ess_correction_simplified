library(essurvey)
library(readr)
library(purrr)

set_email("cimentadaj@gmail.com")

filter <- dplyr::filter
select <- dplyr::select

# Replace w/ available countries
all_countries <- c("Spain",
                   "Germany",
                   "Netherlands",
                   "France",
                   "United Kingdom",
                   "Ireland")

countries_abbrv <- c("ES",
                     "DE",
                     "NL",
                     "FR",
                     "GB",
                     "IE")

all_ids <- c("cntry", "idno")

# To bring the function to extract sddf data.
source("mk_svyConfig.R")

# Country-specific data on spficif sampling strata, which variables
# haave the weights, etcc...
read_csv2("svydesign_info_ESS6.csv") %>%
  filter(domains %% 1 == 0) %>%
  split(., .$country) ->
  svyinfo

ess_df <-
  setNames(
    map(all_countries, ~ recode_missings(import_country(.x, rounds = 6))),
    all_countries
  )


# Same as sqp_cmv but good for programming with. cmv_vars accepts a vector rather than ...
sqp_cmv_str <- function(x, sqp_data, cmv_vars, standardized = TRUE, original_data, 
                        cmv = NULL) {
  if (!(is.data.frame(x) | is.matrix(x))) {
    stop("`x` must be a correlation data frame or matrix")
  }
  if (length(cmv_vars) < 2) {
    stop("You need to supply at least two variables to calculate the Common Method Variance", 
         call. = FALSE)
  }
  sqp_data <- sqpr:::sqp_reconstruct(sqp_data, c("reliability", "validity"))
  x <- sqpr:::matrix2tibble(x)
  sqpr:::columns_present(x, sqp_data, cmv_vars)
  selected_rows <- sqp_data[[1]] %in% cmv_vars
  if (is.null(cmv)) 
    cmv <- sqpr:::estimate_cmv(sqp_data[selected_rows, ])
  if (!standardized && missing(original_data)) {
    stop("Argument `standardized` was set to `FALSE` but the `original_data` argument was not supplied")
  }
  else if (!standardized && !missing(original_data)) {
    stopifnot(is.data.frame(original_data))
    if (!all(cmv_vars %in% names(original_data))) {
      stop("Variables ",
           paste(cmv_vars[!cmv_vars %in% names(original_data)], collapse = ", "),
           " are not preset in `original_data`")
    }
    cmv <- prod(cmv, vapply(original_data[cmv_vars], stats::sd, 
                            FUN.VALUE = numeric(1)))
  }
  corrected_corr <- tibble::as_tibble(sqpr:::corr2cmv(x, cmv, cmv_vars))
  corrected_corr
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
