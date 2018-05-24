library(essurvey)


Sys.setenv("ess_email" = "cimentadaj@gmail.com")
# Replace w/ available countries
all_countries <- c("Spain",
                   "Germany",
                   "Netherlands",
                   "Portugal",
                   "France",
                   "United Kingdom",
                   "Ireland")
# ess_df <-
#   setNames(
#     lapply(all_countries,
#            function(x) recode_missings(import_country(x, rounds = 6, ess_email = Sys.getenv("ess_email")))),
#     all_countries)

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
