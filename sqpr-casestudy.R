library("lavaan")
library("tidyverse")
library("essurvey")

devtools::install_github("asqm/sqpr")
library("sqpr")

### Set your credentials here
# Provide your registere ess email, your SQP email and your password
# in that order.
# Register for SQP here: http://sqp.upf.edu/accounts/login/?next=/loadui/
# Once you run the three lines below you can erase your credentials to 
# how it was before and during this session you will be logged in.
Sys.setenv(ess_email = '')
Sys.setenv(SQP_USER = '')
Sys.setenv(SQP_PW = '')

ess7es <-
  import_country("Spain", 7) %>%
  recode_missings() 

# Create composite scores of several variables
ess7es <-
  ess7es %>%
  mutate(poltrst = trstprl + trstplt + trstprt,
         serv = stfedu + stfhlth,
         systmrsp = psppsgv + psppipl + ptcpplt)

# Select all variables of the analysis available
# in the dataset
selected_vars <- c("trstprl", "trstplt", "trstprt",
                   "stfedu", "stfhlth", "psppsgv",
                   "psppipl", "ptcpplt", "ppltrst",
                   "polintr", "stflife", "stfeco",
                   "agea","eisced")

# Select the compose scores
composite_scores <- c("poltrst", "serv", "systmrsp")

all_vars <- c(composite_scores, selected_vars) # for using later

## ------------------------------------------------------------------------
ess7es

## ------------------------------------------------------------------------
# find the id of the study in the SQP database
sqp_login()
study_id <- find_studies("ESS Round 7")$id

# find the id of each selected question (excluding composite scores
# and agea and eisced which will be manually imputed)
questions <-
  study_id %>%
  find_questions(selected_vars[1:12]) %>%
  filter(country_iso == "ES", language_iso == "spa")

## ------------------------------------------------------------------------
# Just check all questions are present in SQP and in our dataset
all(tolower(questions$short_name) %in% selected_vars[1:12])

## ------------------------------------------------------------------------
# Grab the quality estimates of the questions from the SQP API
sqp_data <-
  get_estimates(questions$id) %>%
  arrange(question)

## ------------------------------------------------------------------------
# `sqpr` estimates from the SQP API.
sqp_data

## ------------------------------------------------------------------------
# Calculate the `quality` of the sum scores. The `sqp_sscore` accepts
# an `sqp` data, that is returned by `get_estimates`, a `df` dataset
# (here the ess data) a new name of the composite score 
# and all variables that compose the sumscore. This calculates the
# quality of the sum scores using the data in `df` and the `quality`
# estimates of each variable in `sqp_data`.
Quality <-
  sqp_data %>%
  sqp_sscore(df = ess7es, new_name = poltrst, trstprl, trstplt, trstprt) %>%
  sqp_sscore(df = ess7es, new_name = serv, stfedu, stfhlth) %>%
  sqp_sscore(df = ess7es, new_name = systmrsp, psppsgv, psppipl, ptcpplt) 

## ------------------------------------------------------------------------
# You see that all previous datasets have been deleted and there's new
# quality estimates for the sum scores.
Quality

## ------------------------------------------------------------------------
# Then we input two `quality` scores manually using the `sqp_bind_metrics`
Quality <- 
  Quality %>%
  sqp_bind_metrics(agea, list(quality = 1)) %>%
  sqp_bind_metrics(eisced, list(quality = 0.93))

Quality

## ------------------------------------------------------------------------
# Let's order all of this ins paricular order
variables_order <- c("poltrst",
                     "serv",
                     "systmrsp",
                     "ppltrst",
                     "polintr",
                     "stflife",
                     "stfeco",
                     "agea",
                     "eisced")

Quality <- Quality[match(variables_order, Quality$question), ]

## ------------------------------------------------------------------------
ess7escorr <- ess7es %>% select(variables_order)

## ------------------------------------------------------------------------
# Exploratory correlation matrix (in order of the columns in data frame):
original_corr_2 <- cor(ess7escorr, use = "complete.obs", method = "pearson")

original_corr_2

## ------------------------------------------------------------------------
# `sqp_correlate` is just a nice wrapper around `cor` to have a nicer output
# but here we replace the diagonal with the quality estimates of each
# variable
corr_q2 <-
  sqp_correlate(x = ess7escorr,
                diag_adj = Quality$quality,
                use = "complete.obs",
                method = "pearson")

corr_q2

## ------------------------------------------------------------------------
#subtract the cmv from the observed correlation
# Calculate the common method variance of some variables
# and subtrct that common method variance from the correlation
# coefficients of the variables
corr_q2_cmv <-
  sqp_cmv(x = corr_q2,
          sqp_data = Quality,
          stfeco, stflife)

corr_q2_cmv

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# Turn into a covariance matrix
corrected_corr <- corr_q2_cmv %>% select(-rowname) %>% as.matrix() %>% cov2cor()

corrected_corr

## ------------------------------------------------------------------------
model<- "poltrst ~ ppltrst + stflife + polintr + stfeco + serv + systmrsp + agea + eisced"

# Model based on original correlation matrix
fit <-
  sem(model,
      sample.cov=original_corr_2,
      sample.nobs= 1624) 

# Model based on corrected covariance matrix 
fit.corrected <-
  sem(model,
      sample.cov=corrected_corr,
      sample.nobs= 1624) 

## ---- fig.width = 7, fig.with = 9----------------------------------------
# Difference in coefficients between models

coef_table <-
  list(fit, fit.corrected) %>%
  map(parameterestimates) %>%
  map(~ filter(.x, lhs == "poltrst")) %>%
  map(~ select(.x, rhs, est, ci.lower, ci.upper)) %>%
  bind_rows() %>%
  mutate(model = rep(c("original", "corrected"), each = 9))

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