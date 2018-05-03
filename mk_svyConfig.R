# email   <- "stefan.zins@gesis.org" 
# rounds  <- 6 # don't change this one yet!!
# country <- "Albania"
source("download_sddf.R")


mk_ess_svy  <- function(svyinfo,
                        ess_data,
                        email = "stefan.zins@gesis.org") {
  country <- as.character(svyinfo$country)
  #skip Austria
  if (country %in% c("Austria", "Israel")) {
    #Austria: 'Error in dataset[[1]] : subscript out of bounds'
    #Israel doesn't have weight data for round 6 in the SDDF
    return(NULL)
  } else {
    print(country)
    sddf_data <-
      grab_sddf(country = country,
                email = email)
    
    if (country == "Denmark") {
      sddf_data %<>%
        filter(!is.na(idno))
    }
    if (country == "Iceland") {
      #no stratification information in sddf
      svyinfo$stratex1 <- "no"
    }
    
    str <-
      if (svyinfo$stratex1 == "yes") {
        ~ stratify
      } else{
        NULL
      }
    sta <-
      if (svyinfo$stages > 1) {
        ~ psu + idno
      } else{
        ~ idno
      }
    
    svydesign(
      id = sta,
      strata = str,
      weights = sddf_data$prob,
      data = full_join(ess_data, sddf_data, by = c("cntry", "idno"))
    ) # merge ess data with sddf data so that all variables are
    # together
  }
  
}

# Test for all ess countries
# ess6_svydesign <- sapply(svyinfo, mk_ess_svy)

