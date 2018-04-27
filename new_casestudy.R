library(lavaan)
library(survey)
library(jtools)
library(tidyverse)
library(essurvey)

devtools::install_github("asqm/sqpr")
library(sqpr)

### Set your credentials here
# Provide your registere ess email, your SQP email and your password
# in that order.
# Register for SQP here: http://sqp.upf.edu/accounts/login/?next=/loadui/
# Once you run the three lines below you can erase your credentials to 
# how it was before and during this session you will be logged in.
Sys.setenv(ess_email = '')
Sys.setenv(SQP_USER = '')
Sys.setenv(SQP_PW = '')

country <- "Spain"
# don't change this just yet. If you do, remember to change
# the country and language where the `sqpr` package
# extracts quality estimates from the SQP API.


### Download ESS
ess6es <-
  import_country(country, 6) %>%
  recode_missings() 

### Download SDDF data ess
# We shouldn't use the file download_sddf directly but only
# to extract the function grab_sddf
source("./download_sddf.R")

ess6sddf <- grab_sddf(country, Sys.getenv("ess_email"))

ess6es <- full_join(ess6es, ess6sddf, by = c("cntry", "idno"))

# Select all variables of the analysis available in the dataset
# Place the dependent variable as the first and all remaining independent
# variables in whatever order
selected_vars <- c("stfdem", "imbgeco", "imueclt")
# stfdem: Stf w/ democracy

# imbgeco: Attitudes towards immigration , consequences :
# bad or good for economy 10 good for the economy, 0 bad

# imueclt: Attitudes towards immigration , consequences :
# cultural life enriched or undermined 10 enriched, 0 not enriched

all_vars <- selected_vars # for using later

## ------------------------------------------------------------------------
ess6es

## ------------------------------------------------------------------------
# find the id of the study in the SQP database
sqp_login()
study_id <- find_studies("ESS Round 6")$id

# find the id of each selected question (excluding composite scores
# and agea and eisced which will be manually imputed)
questions <-
  study_id %>%
  find_questions(selected_vars) %>%
  filter(country_iso == "ES", language_iso == "spa")

## ------------------------------------------------------------------------
# Just check all questions are present in SQP and in our dataset
all(tolower(questions$short_name) %in% selected_vars)

## ------------------------------------------------------------------------
# Grab the quality estimates of the questions from the SQP API
Quality <-
  get_estimates(questions$id) %>%
  arrange(question)

## ------------------------------------------------------------------------
Quality

## ------------------------------------------------------------------------
# Make sure the quality estimates are in the same order as the 
# columns in the ess data. This is because you replace the diagonal
# of their correlation and they need be in the same order to fit
# correctly.
Quality <- Quality[match(selected_vars, Quality$question), ]

## ------------------------------------------------------------------------
# Define variables for weighting from sddf
svy_vars <- list(
  ids = ~psu,
  strata = ~stratify,
  probs = ~prob
)

# weight variables as strings for subsetting as columns
wgt_vars <- map_chr(svy_vars, ~ as.character(.x)[-1])

# Select the variables of interesting the svydesign variables
# (already merged previously from the sddf)
ess6escorr <- 
  ess6es %>%
  select(c(selected_vars, wgt_vars)) %>% 
  filter(complete.cases(.))

# Define survey design
ess_svy <-
  svydesign(
  ids = svy_vars[["ids"]],
  strata = svy_vars[["stratify"]],
  probs = svy_vars[["probs"]],
  data = ess6escorr
)

## ------------------------------------------------------------------------
# Correlation matrix:
# Excluding weight vars
original_corr_2 <- cor(ess6escorr[which(!names(ess6escorr) %in% wgt_vars)],
                       use = "complete.obs", method = "pearson")

original_corr_2
## ------------------------------------------------------------------------
# Weighted correlation matrix and replacing the diagonal 
# with the quality estimate of each variable
corr_q2 <- 
  svycor(~ stfdem + imbgeco + imueclt, design = ess_svy)$cors %>% 
  `diag<-`(Quality$quality)

# `svycor` does not belong to the `survey package` but to the `jtools` package.
# See https://cran.r-project.org/web/packages/jtools/vignettes/svycor.html
# for a description of the function. It is actually implemented by Thomas
# Lumley (survey author) in a random forum post:
# https://stackoverflow.com/questions/34418822/pearson-correlation-coefficient-in-rs-survey-package#41031088
# and someone just took the code and put it in their package so we have
# full confidence it has the same quality as the `survey` code.
corr_q2

## ------------------------------------------------------------------------
#subtract the cmv from the observed correlation
# Calculate the common method variance of some variables
# and subtrct that common method variance from the correlation
# coefficients of the variables
corr_q2_cmv <-
  sqp_cmv(x = corr_q2,
          sqp_data = Quality,
          imbgeco, imueclt)

corr_q2_cmv
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Turn into a covariance matrix
corrected_corr <- corr_q2_cmv %>% select(-rowname) %>% as.matrix() %>% cov2cor()

corrected_corr
## ------------------------------------------------------------------------
model<- paste0(selected_vars[1], " ~ ", paste0(selected_vars[-1], collapse = " + "))
sample_size <- nrow(ess6escorr)

# Model based on original correlation matrix
fit <-
  sem(model,
      sample.cov=original_corr_2,
      sample.nobs= sample_size) 

# Model based on corrected covariance matrix 
fit.corrected <-
  sem(model,
      sample.cov=corrected_corr,
      sample.nobs= sample_size) 

## ---- fig.width = 7, fig.with = 9----------------------------------------
# Difference in coefficients between models

coef_table <-
  list(fit, fit.corrected) %>%
  map(parameterestimates) %>%
  map(~ filter(.x, lhs == selected_vars[1])) %>%
  map(~ select(.x, rhs, est, ci.lower, ci.upper)) %>%
  bind_rows() %>%
  mutate(model = rep(c("original", "corrected"), each = ncol(corrected_corr)))

coef_table %>%
  ggplot(aes(rhs, est, colour = model)) +
  geom_linerange(aes(ymin = ci.lower, ymax = ci.upper), position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(x = "Predictors", y = "Estimated coefficients") +
  theme_bw()

## ------------------------------------------------------------------------
# Relative increase (they don't only go up!):
coef(fit.corrected) / coef(fit)

## ------------------------------------------------------------------------
R2_uncorr <- inspect(fit, 'r2')
R2 <- inspect(fit.corrected, 'r2')

# Change of R2:
R2 - R2_uncorr
