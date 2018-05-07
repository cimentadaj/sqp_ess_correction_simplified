library(tidyverse)
library(sqpr)

pt <- read_csv2("ItemUsageCounts2003-2015.csv")

selected_vars <- pt %>% arrange(-NO.OF.USES) %>% slice(1:25) %>% pull(`LABEL 1`) %>% tolower()

#sqp_username <- ""
#sqp_pw <- ""

sqp_login(sqp_username, sqp_pw)

study_id <- find_studies("Round 6")$id

study_id %>% find_questions(paste0("^", selected_vars, "$"))

# Unique variables present in all countries in the SQP
# data set from the top 25 variables
unique_vars <-
  study_id %>% 
  find_questions(paste0("^", selected_vars, "$")) %>% 
  split(.$country_iso) %>% 
  map(~ unique(.x$short_name)) %>% 
  reduce(intersect)

# Exclude any 'test' variable in case they appear at some point
present_all <- tibble(vars_pre = unique_vars[!str_detect(unique_vars, "^test")])

# Separate the questions by country
ids <- 
  find_questions(study_id, present_all$vars_pre) %>% 
  slice(1:100) %>% 
  split(.$country_iso) %>% 
  map(pull, id)

estimates_country <- map(ids, get_estimates)

# missing values
estimates_country[1]

# complete records
estimates_country[5]

# check if any country has the 25 variables in the SQP API
top <-
  study_id %>% 
  find_questions(paste0("^", selected_vars, "$")) %>% 
  split(.$country_iso)
  
  
map_int(top, nrow)
