library(lavaan)
library(survey)
library(jtools)
library(tidyverse)
library(essurvey)
library(plotly)
library(lavaan.survey)
#load hacked lavaan.survey function 'lavaan.survey_sqp'
source("svy_sqp_lavaan.R") 
source("sqp_cmv_str.R") 

#devtools::install_github("asqm/sqpr")
library(sqpr)

### Set your credentials here
# Provide your registere ess email, your SQP email and your password
# in that order.
# Register for SQP here: http://sqp.upf.edu/accounts/login/?next=/loadui/
# Once you run the three lines below you can erase your credentials to 
# how it was before and during this session you will be logged in.
Sys.setenv(ess_email = 'stefan.zins@gesis.org')
Sys.setenv(SQP_USER = 'asqme')
Sys.setenv(SQP_PW = 'asqme')

# Chosen country. Select here.
country <- "Spain"

## All available countries in the application
all_countries <- c("Spain",
                   "Germany",
                   "Netherlands",
                   "Portugal",
                   "France",
                   "United Kingdom",
                   "Ireland")

country_abbrv <- c("ES", "DE", "NL", "PT", "FR", "GB", "GB", "IE")

chosen_cabbrv <- country_abbrv[which(country == all_countries)]
round <- 5

### Download ESS
ess6es <-
  import_country(country, round) %>%
  recode_missings() 

# Select all variables of the analysis available in the dataset
# Place the dependent variable as the first and all remaining independent
# variables in whatever order
selected_vars <- c("stfdem", "imbgeco", "imueclt")
admin_vars    <- c("cntry", "idno", "dweight", "pweight", "pspwght")
# stfdem: Stf w/ democracy

# imbgeco: Attitudes towards immigration , consequences :
# bad or good for economy 10 good for the economy, 0 bad

# imueclt: Attitudes towards immigration , consequences :
# cultural life enriched or undermined 10 enriched, 0 not enriched

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
  filter(country_iso == chosen_cabbrv)

if (chosen_cabbrv == "ES") {
  questions <- filter(questions, language_iso == "spa")
}

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
# Select the variables | complete.case analysis
# admin vars: "cntry", "idno", "dweight", "pweight", "pspwght"

grab_sddf(rounds = 5,
          country = "Spain",
          email = Sys.getenv("ess_email"))


ess6escorr <- 
  ess6es %>%
  select(c(admin_vars, selected_vars)) %>% 
  filter(complete.cases(.))

# Download SDDF data and create svydesign object
# using `mk_ess_svy`
source("./mk_svyConfig.R")

#load sampling design information
read.csv2("svydesign_info_ESS6.csv") %>%
  filter(domains %% 1 == 0) %>%
  split(.,.$country) ->
  svyinfo

# Only grab svyinfo for selected country and create svy object
ess_svy <- 
  mk_ess_svy(svyinfo = svyinfo[[country]], 
             ess_data = ess6escorr,
             email = Sys.getenv("ess_email"),
             round = 6)

#### Covariance ####

# Correlation matrix without weights:
original_cov <- cov(ess6escorr[selected_vars])

original_cov
# Weighted correlation matrix and replacing the diagonal 
# with the quality estimate of each variable

selected_vars_formula <- 
  eval(parse(text = 
               paste0("~",
                      paste0(selected_vars,
                             collapse = "+"))
             )
       )

#option to deal with lonegly PSUs
options(survey.lonely.psu = "adjust")

# We get the covariance of the variables instead of the
# correlation
cov_q2 <- 
  svyvar(selected_vars_formula, design = ess_svy, na.rm = TRUE) %>%
  as.matrix

attr(cov_q2,"statistic") <- "covariance"

# Replace diagonal by multiplying it with the quality of each variable
diag(cov_q2) <- diag(cov_q2) * Quality$quality

cov_q2

#subtract the cmv from the observed correlation
# Calculate the common method variance of some variables
# and subtract that common method variance from the correlation
# coefficients of the variables.

# We set standardized to FALSE so that `sqp_cmv` can accept a
# covariance in argument `x` rather than a correlation. This way
# the CMV coefficient is multiplied by the standard deviation
# of the variables in the original data.
corrected_cov <-
  sqp_cmv(x = cov_q2,
          sqp_data = Quality,
          imbgeco, imueclt,
          standardized = FALSE,
          original_data = ess6escorr) %>% 
  select_if(is.numeric) %>% 
  as.matrix()

# Still a covariance matrix
corrected_cov
#### Covariance ends ####

#### Correlation ####

# Correlation matrix without weights:
original <- cor(ess6escorr[selected_vars],
                use = "complete.obs",
                method = "pearson")

original

selected_vars_formula <- 
  eval(parse(text = 
               paste0("~",
                      paste0(selected_vars,
                             collapse = "+"))
    )
  )

#option to deal with lonegly PSUs
options(survey.lonely.psu="adjust")

cor_q2 <- 
  svyvar(selected_vars_formula, design = ess_svy, na.rm = TRUE) %>%
  as.matrix() %>% 
  cov2cor()

attr(cor_q2,"statistic") <- "covariance"

# Replace diagonal by multiplying it with the quality of each variable
diag(cor_q2) <- diag(cor_q2) * Quality$quality

cor_q2

#subtract the cmv from the observed correlation
# Calculate the common method variance of some variables
# and subtract that common method variance from the correlation
# coefficients of the variables.
corrected <-
  sqp_cmv(x = cor_q2,
          sqp_data = Quality,
          imbgeco, imueclt) %>% 
  select_if(is.numeric) %>% 
  as.matrix() %>% 
  cov2cor()


corrected

#### Correlation ends ####

## ------------------------------------------------------------------------
# Turn into a correlation matrix
# corrected_corr <- corr_q2_cmv %>% select(-rowname) %>% as.matrix() %>% cov2cor()


# diag(corrected_corr) <- Quality$quality
var_corrected_corr <- attr(corr_q2,"var") 
## ------------------------------------------------------------------------
# Create model formula
model <- paste0(selected_vars[1], 
                " ~ ", 
                paste0(selected_vars[-1], collapse = " + "))

sample_size <- nrow(ess6escorr)

# Model based on original correlation matrix
fit <-
  sem(model,
      sample.cov = original_cov,
      sample.nobs = sample_size)

# Model based on corrected covariance matrix 
fit.corrected <-
  sem(model,
      sample.cov=corrected_cov,
      sample.nobs= sample_size)


fit.corrected.survey <- 
  lavaan.survey_sqp(lavaan.fit = fit,
                    survey.design = ess_svy,
                    estimator = "MLM",
                    estimator.gamma = "Yuan-Bentler",
                    sqp = FALSE,
                    cmv_vars = c("imbgeco", "imueclt"),
                    sqp_data = Quality
  )



## ---- fig.width = 7, fig.with = 9----------------------------------------
# Difference in coefficients between models

coef_table <-
  list(fit, fit.corrected) %>%
  map(parameterestimates) %>%
  map(~ filter(.x, lhs == selected_vars[1])) %>%
  map(~ select(.x, rhs, est, ci.lower, ci.upper)) %>%
  bind_rows() %>%
  mutate(model = rep(c("original", "corrected"), each = ncol(corrected_cov)))

p1 <-
  coef_table %>%
  ggplot(aes(rhs, est, colour = model)) +
  geom_linerange(aes(ymin = ci.lower, ymax = ci.upper),
                 position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(x = "Predictors", y = "Estimated coefficients") +
  theme_bw()

ggplotly(p1)

## ------------------------------------------------------------------------
# Relative increase (they don't only go up!):
coef(fit.corrected) / coef(fit)

## ------------------------------------------------------------------------
R2_uncorr <- inspect(fit, 'r2')
R2 <- inspect(fit.corrected, 'r2')

# Change of R2:
R2 - R2_uncorr
