
# Check sqp data and throw erros if it's not in correct format.
# If it is in the correct format attach 'sqp' class if it doesn't
# have it.

# Why? Because using any of the sqp_ funs with tidyverse verbs
# (or any other function whatsoever), drops the 'sqp' class. These
# functions will check whether the data is in right format and assign
# the class accordingly. It basically makes sure the data is good for
# later processing.
sqp_reconstruct <- function(sqp_data, variables_check = sqp_env$sqp_columns) {
  
  # If sqp_data is not in the correct format, throw an error
  check_sqp_data(sqp_data, variables_check)
  
  # If it has a correct format, then simply add the sqp class if
  # it doesn't have it
  if (!inherits(sqp_data, "sqp")) class(sqp_data) <- c(class(sqp_data), "sqp")
  sqp_data
}


# This should ONLY be used when you want to check an existing sqp
# df
check_sqp_data <- function(sqp_data, available_vars) {
  # Check sqp_env$sqp_columns variables exists
  
  metrics_available <- all(available_vars %in% names(sqp_data))
  
  if (!metrics_available) {
    stop("Variables ", paste0(available_vars, collapse = ", "),
         " must be available in `sqp_data`",
         call. = FALSE)
  }
  
  purrr::walk(sqp_data[available_vars], col_checker)
}

col_checker <- function(x) {
  if (all(is.na(x))) return(TRUE)
  
  is_numeric <- is.numeric(x)
  is_perc <- all(x >= 0 & x <= 1, na.rm = TRUE)
  if (!is_numeric | !is_perc) {
    stop(paste0(sqp_env$sqp_columns, collapse = ", "),
         " must be numeric columns with values between/including 0 and 1 in `sqp_data`",
         call. = FALSE)
  }
  invisible(TRUE)
}

columns_present <- function(corr_data, sqp_data, var_names) {
  sum_corr <- corr_data[[1]] %in% var_names
  sum_sqp <- sqp_data[[1]] %in% var_names
  
  vars_corr <- var_names %in% corr_data[[1]]
  vars_sqp <- var_names %in% sqp_data[[1]]
  
  if (sum(sum_corr) != length(var_names)) {
    stop("At least one variable not present in `x`: ",
         paste0(var_names[!vars_corr], collapse = ", "),
         call. = FALSE)
  }
  
  if ((sum(sum_sqp) != length(var_names))) {
    stop("At least one variable not present in `sqp_data`: ",
         paste0(var_names[!vars_sqp], collapse = ", "),
         call. = FALSE)
  }
  
}


# These functions are aimed at making requests to the SQP API.

# This function allows to retrieve the same requests if
# no parameters have changed. I have to keep it outside the functions
# because if defined inside a fun, then it is deleted after
# the env of the fun is deleted.
memo_GET <- memoise::memoise(httr::GET)

# Make a general request with the login information
auth_GET <- function(path, ...) {
  check_login()
  
  auth <- httr::add_headers('Authorization' = paste("Bearer", sqp_env$token))
  
  res <-
    memo_GET(url = sqp_env$hostname,
             path = path,
             config = auth,
             ...)
  res
}

# Wrapper of the previous fun to raise
# any errors early and clearly
safe_GET <- function(path, ...) {
  res <- auth_GET(path, ...)
  catch_error(res)
  res
}

# Wrapper to grab the data from the requests
# If estimates is TRUE returns a list, otherwise
# a tibble
object_request <- function(path, estimates = FALSE) {
  requested <- safe_GET(path)
  get_content <- httr::content(requested, as = 'text')
  
  if (estimates) {
    json_data <- jsonlite::fromJSON(get_content, flatten = TRUE)$data
    return(json_data)
  } else {
    json_data <- jsonlite::fromJSON(get_content)$data
  }
  
  final_df <- tibble::as_tibble(json_data)
  final_df
}
################################################################################
sqp_data_ <- sqp_data[selected_rows, ]
estimate_cmv <- function(sqp_data) {
  sqp_cols <- c("reliability", "validity")
  sqp_reconstruct(sqp_data_, sqp_cols)
  
  if (anyNA(sqp_data[sqp_cols])) {
    stop("`sqp_data` must have non-missing values at columns reliability and validity for all variables")
  }
  
  #reliability
  first_part  <- sqrt(sqp_data_[[sqp_cols[1]]])
  #method effect
  second_part <- sqrt(1 - sqp_data_[[sqp_cols[2]]])
  
  cmv <- prod(c(first_part, second_part))
  cmv
}

# This function is the one doing the replacement of the upper
# and lower of the correlation matrix.
corr2cmv <- function(x, cmv, cmv_vars) {
  x_row_low <- sort(match(cmv_vars, x[[1]]))
  x_col_low <- sort(match(cmv_vars, names(x)))
  
  x <- as.data.frame(x)
  
  p <- x[x_row_low, x_col_low] # subset only the select variables
  p[lower.tri(p)] <- p[lower.tri(p)] - cmv # adjust the lower.tri
  p[upper.tri(p)] <- p[upper.tri(p)] - cmv # adjust the upper.tri
  x[x_row_low, x_col_low] <- p # replace in the original data.frame
  
  x
}


matrix2tibble <- function(x) {
  has_rowname_col <- "rowname" %in% names(x)
  
  # It has a column rowname and is a tibble
  # then it's porbbaly from sqp_correlate
  if (tibble::has_name(x, "rowname") && tibble::is_tibble(x)) {
    return(x)
  } else if (tibble::has_rownames(x) & !has_rowname_col) {
    # If it has rownames and doesn't have a row name column
    # turn into tibble with row name column
    return(tibble::as_tibble(x, rownames = "rowname"))
  }
  
  x <- tibble::as_tibble(x)
  
  if (!tibble::has_rownames(x) & !has_rowname_col) {
    x <- tibble::add_column(x,
                            rowname = names(x),
                            .before = 1)
  }
  x
}
################################################################################


x        <- cor_q2
sqp_data <- Quality
cmv_vars <- c("imbgeco", "imueclt")
  
  if (!(is.data.frame(x) | is.matrix(x))) {
    stop("`x` must be a correlation data frame or matrix")
  }
  if (length(cmv_vars) < 2) {
    stop("You need to supply at least two variables to calculate the Common Method Variance", 
         call. = FALSE)
  }
  
  sqp_data <- sqp_reconstruct(sqp_data, c("reliability", "validity"))
  x <- matrix2tibble(x)
  columns_present(x, sqp_data, cmv_vars)
  selected_rows <- sqp_data[[1]] %in% cmv_vars
  if (is.null(cmv)) 
    cmv <- estimate_cmv(sqp_data[selected_rows, ])
  
  if (!standardized && missing(original_data)) {
    stop("Argument `standardized` was set to `FALSE` but the `original_data` argument was not supplied")
  } else if (!standardized && !missing(original_data)) {
    #if standardized==FALSE
    #cmv zur korretur der covarianz?
    stopifnot(is.data.frame(original_data))
    if (!all(cmv_vars %in% names(original_data))) {
      stop("Variables ", paste(cmv_vars[!cmv_vars %in% 
                                          names(original_data)], collapse = ", "),
           " are not preset in `original_data`")
    }
    cmv <- prod(cmv, vapply(original_data[cmv_vars], stats::sd, 
                            FUN.VALUE = numeric(1)) )
  }
  corrected_corr <- tibble::as_tibble(corr2cmv(x, cmv, cmv_vars))
  corrected_corr
}





x_row_low <- sort(match(cmv_vars, x[[1]]))
x_col_low <- sort(match(cmv_vars, names(x)))

x <- as.data.frame(x)

p <- x[x_row_low, x_col_low] # subset only the select variables
p[lower.tri(p)] <- p[lower.tri(p)] - cmv # adjust the lower.tri
p[upper.tri(p)] <- p[upper.tri(p)] - cmv # adjust the upper.tri
x[x_row_low, x_col_low] <- p # replace in the original data.frame

x