# ################################################################################
# 
# # Model based on corrected covariance matrix 
# # Create model formula
# model <- paste0(selected_vars[1], 
#                 " ~ ", 
#                 paste0(selected_vars[-1], collapse = " + "))
# 
# sample_size <- nrow(ess6escorr)
# 
# # Model based on original correlation matrix
# fit <-
#   sem(model,
#       sample.cov = original_cov,
#       sample.nobs = sample_size)
# 
# # Model based on corrected covariance matrix 
# fit.corrected <-
#   sem(model,
#       sample.cov=corrected_cov,
#       sample.nobs= sample_size)
# 
# 
# ################################################################################
# 
# #I'm not sure if this hack is applicable to multiple group SEMs
# #MI survey.design should also not be used
# 
# lavaan.fit = fit.corrected
# survey.design = ess_svy
# estimator = "MLM"
# estimator.gamma = "Yuan-Bentler"
# sqp_quality <- Quality 
# cmv_vars <- c("imbgeco", "imueclt")
# sqp = TRUE
# sqp_data = Quality


#works with 'sqp_cmv_str' not sqp_cmv

################################################################################
lavaan.survey_sqp <-
  function( lavaan.fit,
            survey.design,
            estimator = c("MLM", "MLMV",
                          "MLMVS", "WLS", "DWLS", "ML"),
            estimator.gamma = c("default",
                                "Yuan-Bentler"),
            sqp = FALSE,
            cmv_vars,
            sqp_data)
  {
    estimator <- match.arg(estimator)
    if (estimator == "ML")
      warning("Estimator 'ML' will not correct standard errors and chi-square statistic.")
    estimator.gamma <- match.arg(estimator.gamma)
    ov.names <- lavaanNames(lavaan.fit, type = "ov", group = 1)
    Dplus <- lavaan::lav_matrix_duplication_ginv(length(ov.names))
    ov.formula <-
      as.formula(paste("~", paste(ov.names, collapse = "+")))
    ngroups <- lavInspect(lavaan.fit, "ngroups")
    Gamma <- vector("list", ngroups)
    sample.cov <- vector("list", ngroups)
    sample.mean <- vector("list", ngroups)
    sample.nobs <- vector("list", ngroups)
    for (g in seq(ngroups)) {
      if (ngroups > 1) {
        survey.design.g <-
          subset(survey.design, eval(parse(
            text = sprintf(
              "%s == '%s'",
              lavInspect(lavaan.fit, "group"),
              lavInspect(lavaan.fit,
                         "group.label")[[g]]
            )
          )))
      }
      else {
        survey.design.g <- survey.design
      }
      get.stats.design <- function(survey.design.g, sample.nobs.g) {
        #<<< inserted code starts here
        
        # Estimate covariance matrix
        sample.cov.g <-
          as.matrix(svyvar(ov.formula,
                           design = survey.design.g,
                           na.rm = TRUE))

        # sqp correction here
        ## variance reduction (q^2)
        sqp_q <- function(x, sqp_data) {
          q_ <- rep(1, ncol(x))
          names(q_) <- colnames(x)
          diag(x) <-
            q_[sqp_data$question] *
            sqp_data$quality
          x
        }
        
        get_corr_varCov <- function(x, varCov.est, sqp_data) {
          
          #Calculate variance of corrected covariance matrix
          # - cmv adjustement is variance neural, since it is an substration not
          # a multiplication.
        
          q_ <- rep(1, ncol(x))
          names(q_) <- colnames(x)
          q_[sqp_data$question] <-
            q_[sqp_data$question] *
            sqp_data$quality
          
          
          cov_q <- matrix(1, nrow = nrow(x), ncol = ncol(x))
          diag(cov_q) <- q_
          
          varCov_q <- outer(as.vector(cov_q),
                            as.vector(cov_q),
                            "*")
          varCov.est * varCov_q
          
        }
        
        if (sqp) {
          Gamma.cov.g <- attr(sample.cov.g, "var")
          sample.cov.g <-
            sqp_q(x = sample.cov.g, sqp_data = sqp_data)
          sample.cov.g <-
            sqp_cmv_str(
              x = sample.cov.g,
              sqp_data = sqp_data,
              cmv_vars = cmv_vars,
              standardized = FALSE,
              original_data = survey.design.g$variables
            )
          sample.cov.g <-
            as.matrix(sample.cov.g[, sapply(sample.cov.g, is.numeric)])
          
          Gamma.cov.g <-
            get_corr_varCov(x = sample.cov.g,
                            varCov.est = Gamma.cov.g,
                            sqp_data = sqp_data)
          
        } else{
          Gamma.cov.g <- attr(sample.cov.g, "var")
        }
        #>>> instered code ends here
        
        Gamma.cov.g <- Dplus %*% Gamma.cov.g %*% t(Dplus)
        sample.mean.g <-
          svymean(ov.formula, design = survey.design.g,
                  na.rm = TRUE)
        #variance of means doesn't need an correction since the estimates of means
        #are not corrected for measurement error
        Gamma.mean.g <- attr(sample.mean.g, "var")
        Gamma.g <- lavaan::lav_matrix_bdiag(Gamma.mean.g,
                                            Gamma.cov.g)
        Gamma.g <- Gamma.g * sample.nobs.g
        if (estimator.gamma == "Yuan-Bentler") {
          r <- lavaan.survey:::get.residuals(lavaan.fit)
          Gamma.g <- Gamma.g + (sample.nobs.g / (sample.nobs.g -
                                                   1)) * (r %*% t(r))
        }
        attr(sample.cov.g, "var") <- NULL
        tmp <- as.vector(sample.mean.g)
        names(tmp) <- names(sample.mean.g)
        sample.mean.g <- tmp
        list(
          Gamma.g = Gamma.g,
          sample.cov.g = sample.cov.g,
          sample.mean.g = sample.mean.g
        )
      }
     
      if (!any(class(survey.design.g) == "svyimputationList")) {
        sample.nobs.g <- lavInspect(lavaan.fit, "nobs")[[g]]
        stats <- get.stats.design(survey.design.g, sample.nobs.g)
      }
      else {
        sample.nobs.g <- get.sample.nobs(survey.design.g,
                                         lavInspect(lavaan.fit, "group"))
        stats.list <-
          lapply(survey.design.g[[1]], get.stats.design,
                 sample.nobs = sample.nobs.g)
        m <- length(stats.list)
        sample.cov.list <- lapply(stats.list, `[[`, "sample.cov.g")
        sample.cov.g <- Reduce(`+`, sample.cov.list) / m
        cov.df <-
          Reduce(rbind,
                 lapply(sample.cov.list, lavaan::lav_matrix_vech))
        sample.mean.list <-
          lapply(stats.list, `[[`, "sample.mean.g")
        sample.mean.g <- Reduce(`+`, sample.mean.list) / m
        mean.df <- Reduce(rbind, sample.mean.list)
        Gamma.within <- Reduce(`+`, lapply(stats.list, `[[`,
                                           "Gamma.g")) / m
        Gamma.between <- cov(cbind(mean.df, cov.df))
        Gamma.g <- Gamma.within + ((m + 1) / m) * Gamma.between
        stats <-
          list(
            Gamma.g = Gamma.g,
            sample.cov.g = sample.cov.g,
            sample.mean.g = sample.mean.g
          )
      }
      Gamma[[g]] <- stats$Gamma.g
      sample.cov[[g]] <- stats$sample.cov.g
      sample.mean[[g]] <- stats$sample.mean.g
      sample.nobs[[g]] <- sample.nobs.g
    }
    new.call <- lavInspect(lavaan.fit, "call")
    new.call$data <- NULL
    new.call$sample.cov <- sample.cov
    new.call$sample.mean <- sample.mean
    new.call$sample.nobs <- sample.nobs
    new.call$estimator <- estimator
    if (substr(estimator, 1, 2) == "ML") {
      new.call$NACOV <- Gamma
    }
    if (estimator %in% c("WLS", "DWLS")) {
      new.call$WLS.V <- lapply(Gamma, ginv)
    }
    new.fit <- eval(as.call(new.call))
    if (estimator %in% c("WLS", "DWLS"))
      return(new.fit)
    evs.too.small <- sapply(Gamma, function(Gamma.g) {
      any(eigen(Gamma.g, only.values = TRUE)$values < (.Machine$double.eps *
                                                         100))
    })
    if (any(evs.too.small)) {
      V.est <- lavaan::vcov(new.fit)
      if (any(Re(eigen(V.est, only.values = TRUE)$values) <
              (.Machine$double.eps * 100))) {
        long.string <-
          sprintf(
            "Some of the standard errors may not be trustworthy.\n        Some of the observed covariances or means are\n        collinear, and this has generated collinearity in your\n        parameter estimates.  This may be a sample size issue,\n        missing data problem, or due to having too few\n        clusters relative to the number of parameters. Problem\n        encountered in group(s) %s",
            paste(which(evs.too.small), collapse = ", ")
          )
        warning(strwrap(long.string, width = 9999, simplify = TRUE))
      }
    }
    new.fit
  }
