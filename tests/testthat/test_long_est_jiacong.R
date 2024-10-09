rm(list=ls())

library(dplyr)
library(mice)
library(survival)

load("/Users/jiacong/Google Drive/Umich/research/longitudinalEHR/git/CIMPLE/data/train_data.rda")

time_var <- "time"
id_var <- "id"
outcome_var <- "Y"
VPM_variables <- c("Z", "X")
LM_fixedEffect_variables <- c("Z", "X")
LM_randomEffect_variables <- "Z"

# Fit the standard LME model
long_est(long_data=train_data,
                     method="standard_LME",
                     id_var="id",
                     outcome_var=outcome_var,
                     LM_fixedEffect_variables = LM_fixedEffect_variables,
                     time = "time",
                     LM_randomEffect_variables = LM_randomEffect_variables,
                     VPM_variables = VPM_variables,
                     optCtrl = list(method = "nlminb", kkt = FALSE, tol = 0.2, maxit = 20000),
                     control = list(verbose = FALSE,
                                    tol = 1e-3,
                                    GHk = 10,
                                    maxiter = 150))

# Fit the standard LME model with the default optimizer
long_est(long_data=train_data,
                     method="VA_LME",
                     id_var=id_var,
                     outcome_var=outcome_var,
                     LM_fixedEffect_variables = LM_fixedEffect_variables,
                     time = time_var,
                     LM_randomEffect_variables = LM_randomEffect_variables,
                     VPM_variables = VPM_variables,
                     control = list(verbose = FALSE,
                                    tol = 1e-3,
                                    GHk = 10,
                                    maxiter = 150))

# JMVL_LY
fit_JMVL_LY = long_est(long_data=train_data,
                     method="JMVL_LY",
                     id_var=id_var,
                     outcome_var=outcome_var,
                     LM_fixedEffect_variables = LM_fixedEffect_variables,
                     time = time_var,
                     LM_randomEffect_variables = LM_randomEffect_variables,
                     VPM_variables = VPM_variables,
                     control = list(verbose = FALSE,
                                    tol = 1e-3,
                                    GHk = 10,
                                    maxiter = 150))
fit_JMVL_LY

# IIRR_weighting
fit_IIRR_weighting = long_est(long_data=train_data,
                     method="IIRR_weighting",
                     id_var=id_var,
                     outcome_var=outcome_var,
                     LM_fixedEffect_variables = LM_fixedEffect_variables,
                     time = time_var,
                     LM_randomEffect_variables = LM_randomEffect_variables,
                     VPM_variables = VPM_variables,
                     control = list(verbose = FALSE,
                                    tol = 1e-3,
                                    GHk = 10,
                                    maxiter = 150))
fit_IIRR_weighting

# JMVL_G
fit_JMVL_G = long_est(long_data=train_data,
                     method="JMVL_G",
                     id_var=id_var,
                     outcome_var=outcome_var,
                     LM_fixedEffect_variables = LM_fixedEffect_variables,
                     time = time_var,
                     LM_randomEffect_variables = LM_randomEffect_variables,
                     VPM_variables = VPM_variables,
                     control = list(verbose = TRUE,
                                    tol = 1e-3,
                                    GHk = 20,
                                    maxiter = 150))
fit_JMVL_G$estimates$betas

# JMVL_Liang
fit_JMVL_Liang = long_est(long_data=train_data,
                     method="JMVL_Liang",
                     id_var=id_var,
                     outcome_var=outcome_var,
                     LM_fixedEffect_variables = LM_fixedEffect_variables,
                     time = time_var,
                     LM_randomEffect_variables = LM_randomEffect_variables,
                     VPM_variables = VPM_variables,
                     control = list(verbose = FALSE,
                                    tol = 1e-3,
                                    GHk = 10,
                                    maxiter = 150))
fit_JMVL_Liang

# imputation_LME
fit_imputation_LME = long_est(long_data=train_data,
                     method="imputation_LME",
                     id_var=id_var,
                     outcome_var=outcome_var,
                     LM_fixedEffect_variables = LM_fixedEffect_variables,
                     time = time_var,
                     LM_randomEffect_variables = LM_randomEffect_variables,
                     VPM_variables = VPM_variables,
                     imp_time_factor = 1,
                     control = list(verbose = FALSE,
                                    tol = 1e-3,
                                    GHk = 10,
                                    maxiter = 150))
fit_imputation_LME

