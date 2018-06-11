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

grab_sddf <- function(rounds, country, email, ess_website) {
  #### Checking all main arguments are valid
  if (!rounds %in% show_rounds()) {
    stop(rounds, " is not a valid round.")
  }
  
  if (!country %in% show_countries()) {
    stop(country, " is not a available in ESS round ", rounds)
  }
  
  ## Define all important links
  ess_url <- paste0(ess_website, "/data/download.html?r=")
  final_url <- paste0(ess_url, rounds)
  
  # Grab all country names
  cnts <-
    xml_text(xml_find_all(read_html(final_url), xpath = "//h4"))
  
  
  # Grab all urls that have an sddf file
  web_try <-
    as_list(xml_find_all(read_html(final_url), xpath = "//div[@class='round_files']//ul"))
  
  # Because some countries don't have an sddf urls, they're missing
  # a slot in the list that returns all the country details.
  # Below I loop over the index of each country-info and grab
  # the sddf url to donwload are returned an empty string
  all_sddf <-
    vapply(seq_along(web_try), function(index) {
      
      cnt_slot <- web_try[[index]]
      
      index_sample <-
        which(vapply(transpose(cnt_slot),
               function(.x) grepl("SDDF", .x),
               FUN.VALUE = logical(length(cnt_slot))))
      
      # If not SDDF file found then return empty chr, otherwise
      # grab the href attr
      if (length(index_sample) == 0) {
        grabs_link <- ""
      } else {
        grabs_link <- attr(cnt_slot[[index_sample]][[1]], "href")
      }
      
      grabs_link
    }, FUN.VALUE = character(1))
  
  # Which countries are available in the sddf website?
  country_available <- country %in% cnts
  
  # GRab the index of the countries that are available
  index_country <- country == cnts
  
  # Grab the sddf link of the chosen country
  available_link <- web_try[index_country]
  country_link <- all_sddf[index_country]
  
  # Raise an error if chose country doesn't have an sddf url
  if (country_available &&  country_link == "") {
    stop(country, " doesn't have weight data for round ", rounds)
  }
  
  # Grab the specific 'stata' (or 'spss') zip url from 
  # each country link and construct the full download path
  format_urls <-
    paste0(ess_website,
           map_chr(country_link, grab_link, ess_website = ess_website, format = "stata"))
  
  # Download the data and read it in stata format
  all_paths <-
    round_downloader(
      format_urls,
      country,
      tempdir()
    )
  
  folder_data <- dirname(grep("SDDF", all_paths, value = TRUE))
  
  sddf_data <- read_format_data(folder_data, 
                                format = 'stata',
                                rounds = rounds)
  
  names(sddf_data) <- tolower(names(sddf_data))
  
  # all ready
  sddf_data
}


