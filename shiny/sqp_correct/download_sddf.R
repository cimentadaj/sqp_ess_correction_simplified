library(xml2)
library(httr)
library(haven)

#### Functions

# Safe getter
safe_GET <- function(url, config = list(), ...) {
  httr::stop_for_status(httr::GET(url = url, config = config, ...))
}

# This function returns the stata or spss link of a given sddf
# file.
grab_link <- function(link, ess_website, format = "stata") {
  download_page <- safe_GET(paste0(ess_website, link))
  html_ess <- xml2::read_html(download_page)
  z <- xml2::xml_text(xml2::xml_find_all(html_ess, "//a/@href"))
  z[grep(format, z)]
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

# This function takes a dir and reads all files in the argument
# format and then deletes the files.
read_format_data <- function (urls, format, rounds) {
  # Added this in different from essurvey
  on.exit(unlink(format_dirs, recursive = TRUE, force = TRUE))
  
  format_read <- switch(format,
                        spss = haven::read_spss,
                        stata = haven::read_dta)
  
  # Added this different from essurvey
  format_ext <- if (format == 'stata') ".dta" else c(".sav", ".por")
  
  format_path <- paste0(format_ext, "$", collapse = "|")
  format_dirs <- list.files(urls, pattern = format_path,
                            full.names = TRUE)
  
  dataset <- lapply(format_dirs, format_read)
  
  print(head(dataset))

  if (length(rounds) == 1) dataset <- dataset[[1]]
  
  dataset
}

round_downloader <- function(each_url, which_round, which_folder) {
  message(paste("Downloading", which_round))
  temp_download <- file.path(which_folder, paste0(which_round, ".zip"))
  print(temp_download)
  current_file <- safe_GET(each_url,
                           write_disk(temp_download, overwrite = TRUE),
                           httr::progress())
  print(current_file)
  print(list.files(dirname(temp_download)))
  unzip(temp_download, exdir = which_folder)
}
