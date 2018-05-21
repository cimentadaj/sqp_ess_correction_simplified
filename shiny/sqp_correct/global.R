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
ess_df <-
  setNames(
    lapply(all_countries,
           function(x) recode_missings(import_country(x, rounds = 6, ess_email = Sys.getenv("ess_email")))),
    all_countries)
