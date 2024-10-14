
### change the column names of the dataset and run the code again
rm(list=ls())

library(dplyr)
library(mice)
library(survival)

load("/Users/jiacong/Google Drive/Umich/research/longitudinalEHR/git/CIMPLE/data/surv_data.rda")
load("/Users/jiacong/Google Drive/Umich/research/longitudinalEHR/git/CIMPLE/data/long_data.rda")
head(surv_data)
head(long_data)

surv_data1 = surv_data
colnames(surv_data1) = c("id_var","year","gender","mark","survD","survd")
long_data1 = long_data
colnames(long_data1) = c("id_var","year","gender","mark","obsTime","Y1")
head(surv_data1)
head(long_data1)

id_var = "id_var"
time = "obsTime"
survTime = "survD"
survEvent = "survd"
LM_fixedEffect_variables = c("year","gender","mark")
LM_randomEffect_variables = c("mark")
SM_timeVarying_variables = c("Y1")
SM_timeInvariant_variables = c("year","gender","mark")
imp_time_factor = 1


# run the cox model
fit_cox = surv_est(surv_data = surv_data1,
         long_data = long_data1,
         method = "cox",
         id_var = id_var,
         time = time,
         survTime = survTime,
         survEvent = survEvent,
         LM_fixedEffect_variables = LM_fixedEffect_variables,
         LM_randomEffect_variables = LM_randomEffect_variables,
         SM_timeVarying_variables = SM_timeVarying_variables,
         SM_timeInvariant_variables = SM_timeInvariant_variables,
         imp_time_factor = imp_time_factor)
fit_cox

# run the JMLD model
fit_JMLD = surv_est(surv_data = surv_data1,
         long_data = long_data1,
         method = "JMLD",
         id_var = id_var,
         time = time,
         survTime = survTime,
         survEvent = survEvent,
         LM_fixedEffect_variables = LM_fixedEffect_variables,
         LM_randomEffect_variables = LM_randomEffect_variables,
         SM_timeVarying_variables = SM_timeVarying_variables,
         SM_timeInvariant_variables = SM_timeInvariant_variables,
         imp_time_factor = imp_time_factor)
fit_JMLD

# run the VA_JMLD model
fit_VA_JMLD = surv_est(surv_data = surv_data1,
         long_data = long_data1,
         method = "VA_JMLD",
         id_var = id_var,
         time = time,
         survTime = survTime,
         survEvent = survEvent,
         LM_fixedEffect_variables = LM_fixedEffect_variables,
         LM_randomEffect_variables = LM_randomEffect_variables,
         SM_timeVarying_variables = SM_timeVarying_variables,
         SM_timeInvariant_variables = SM_timeInvariant_variables,
         imp_time_factor = imp_time_factor)
fit_VA_JMLD

# run the Imputation_Cox model
fit_Imputation_Cox = surv_est(surv_data = surv_data1,
         long_data = long_data1,
         method = "Imputation_Cox",
         id_var = id_var,
         time = time,
         survTime = survTime,
         survEvent = survEvent,
         LM_fixedEffect_variables = LM_fixedEffect_variables,
         LM_randomEffect_variables = LM_randomEffect_variables,
         SM_timeVarying_variables = SM_timeVarying_variables,
         SM_timeInvariant_variables = SM_timeInvariant_variables,
         imp_time_factor = imp_time_factor)
fit_Imputation_Cox

# run the VAImputation_Cox model
fit_VAImputation_Cox = surv_est(surv_data = surv_data1,
         long_data = long_data1,
         method = "VAImputation_Cox",
         id_var = id_var,
         time = time,
         survTime = survTime,
         survEvent = survEvent,
         LM_fixedEffect_variables = LM_fixedEffect_variables,
         LM_randomEffect_variables = LM_randomEffect_variables,
         SM_timeVarying_variables = SM_timeVarying_variables,
         SM_timeInvariant_variables = SM_timeInvariant_variables,
         imp_time_factor = imp_time_factor)
fit_VAImputation_Cox

# check if the results are the same names for the alpha_hat
fit_cox
fit_JMLD
fit_VA_JMLD
fit_Imputation_Cox
fit_VAImputation_Cox
