# sqp_ess_correction_simplified

This is a simplified version of the SQP Measurement error/weights corrected Shiny Application developed at the RECSM. The original code focused only on one year, a set of countries and a set of variales, which we adjusted the correlation and covariance matrix with weights and measurement error and passed it to lavaan to estimate regression models. In this edition, we use all countries/rounds but only return the corrected correlation and covariance matrix.
