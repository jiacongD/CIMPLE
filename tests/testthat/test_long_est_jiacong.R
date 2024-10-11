rm(list=ls())

# Jiacong: 
# 1. generalize the input arguments for the function long_est and surv_est
# 2. write the return arguments for the documentation.
# Howard:
# 1. format the return argument. 
# 2. write the example codes for the function long_est and surv_est.
# 3. put the references in the documentation.

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

set.seed(2024)

# Fit the standard LME model
beta_hat_standardLME = long_est(long_data=train_data,
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
beta_hat_standardLME$beta_hat

# Fit the standard LME model with the default optimizer
beta_hat_VALME = long_est(long_data=train_data,
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
beta_hat_VALME$beta_hat

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

############# change the column names of the dataset and run the code again
head(train_data)
train_data1 <- train_data
colnames(train_data1) <- c("patientID","trt","age","time","outcome") # change the column names

time_var = "time"
id_var = "patientID"
outcome_var = "outcome"
VPM_variables = c("trt", "age")
LM_fixedEffect_variables = c("trt", "age")
LM_randomEffect_variables = "trt"

# run the standard LME model
fit_standardLME = long_est(long_data=train_data1,
                     method="standard_LME",
                     id_var=id_var,
                     outcome_var=outcome_var,
                     LM_fixedEffect_variables = LM_fixedEffect_variables,
                     time = time_var,
                     LM_randomEffect_variables = LM_randomEffect_variables,
                     VPM_variables = VPM_variables,
                     optCtrl = list(method = "nlminb", kkt = FALSE, tol = 0.2, maxit = 20000),
                     control = list(verbose = FALSE,
                                    tol = 1e-3,
                                    GHk = 10,
                                    maxiter = 150))
fit_standardLME

# run the VA_LME model
fit_VALME = long_est(long_data=train_data1,
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
fit_VALME

# run the JMVL_LY model
fit_JMVL_LY = long_est(long_data=train_data1,
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

# run the IIRR_weighting model
fit_IIRR_weighting = long_est(long_data=train_data1,
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

# run the JMVL_G model
fit_JMVL_G = long_est(long_data=train_data1,
                     method="JMVL_G",
                     id_var=id_var,
                     outcome_var=outcome_var,
                     LM_fixedEffect_variables = LM_fixedEffect_variables,
                     time = time_var,
                     LM_randomEffect_variables = LM_randomEffect_variables,
                     VPM_variables = VPM_variables,
                     control = list(verbose = TRUE,
                                    tol = 1e-3,
                                    GHk = 10,
                                    maxiter = 150))
fit_JMVL_G$beta_hat

# run the JMVL_Liang model
fit_JMVL_Liang = long_est(long_data=train_data1,
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

# run the imputation_LME model
fit_imputation_LME = long_est(long_data=train_data1,
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

# list all the fit, check if the column names match
beta_hat_standardLME
beta_hat_VALME
fit_JMVL_LY
fit_IIRR_weighting
fit_JMVL_G$beta_hat
fit_JMVL_Liang
fit_imputation_LME

# 1. change the output value names to beta_hat.
# 2. create the example codes for each function (long_est, surv_est). Basically very similar to the script here.
# 3. change the dataset column names and run the code again.


