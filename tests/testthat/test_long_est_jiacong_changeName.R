############# change the column names of the dataset and run the code again
library(dplyr)
library(mice)
library(survival)

load("/Users/jiacong/Google Drive/Umich/research/longitudinalEHR/git/CIMPLE/data/train_data.rda")
head(train_data)

train_data1 <- train_data
colnames(train_data1) <- c("patientID","trt","age","obsTime","outcome") # change the column names

time_var = "obsTime"
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
fit_JMVL_G$beta_hat # TODO: Error

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
fit_standardLME # done
fit_VALME # done
fit_JMVL_LY # no need to change
fit_IIRR_weighting # done
fit_JMVL_G$beta_hat # done
fit_JMVL_Liang # no need to change
fit_imputation_LME # done
