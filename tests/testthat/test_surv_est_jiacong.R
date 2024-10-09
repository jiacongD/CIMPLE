rm(list=ls())

load("/Users/jiacong/Google Drive/Umich/research/longitudinalEHR/git/CIMPLE/data/surv_data.rda")
load("/Users/jiacong/Google Drive/Umich/research/longitudinalEHR/git/CIMPLE/data/long_data.rda")

library(dplyr)
library(mice)
library(survival)

head(surv_data)
head(long_data)

id_var = "id"
time = "time"
LM_fixedEffect_variables = c("Age","Sex","SNP","time") # Note: we need to make this consistent with the input as the long_est function
LM_randomEffect_variables = c("SNP")
SM_timeVarying_variables = c("Y")
SM_timeInvariant_variables = c("Age","Sex","SNP")
imp_time_factor = 1

# all methods = cox, JMLD, VA_JMLD, Imputation_Cox, VAImputation_Cox
# fit the cox model
fit_cox = surv_est(surv_data = surv_data,
         long_data = long_data,
         method = "cox",
         id_var = id_var,
         time = time,
         LM_fixedEffect_variables = LM_fixedEffect_variables,
         LM_randomEffect_variables = LM_randomEffect_variables,
         SM_timeVarying_variables = SM_timeVarying_variables,
         SM_timeInvariant_variables = SM_timeInvariant_variables,
         imp_time_factor = imp_time_factor)
fit_cox

# fit the JMLD model
fit_JMLD = surv_est(surv_data = surv_data,
         long_data = long_data,
         method = "JMLD",
         id_var = id_var,
         time = time,
         LM_fixedEffect_variables = LM_fixedEffect_variables,
         LM_randomEffect_variables = LM_randomEffect_variables,
         SM_timeVarying_variables = SM_timeVarying_variables,
         SM_timeInvariant_variables = SM_timeInvariant_variables,
         imp_time_factor = imp_time_factor)
fit_JMLD

# fit the VA_JMLD model
fit_VA_JMLD = surv_est(surv_data = surv_data,
         long_data = long_data,
         method = "VA_JMLD",
         id_var = id_var,
         time = time,
         LM_fixedEffect_variables = LM_fixedEffect_variables,
         LM_randomEffect_variables = LM_randomEffect_variables,
         SM_timeVarying_variables = SM_timeVarying_variables,
         SM_timeInvariant_variables = SM_timeInvariant_variables,
         imp_time_factor = imp_time_factor)
fit_VA_JMLD

# fit the Imputation_Cox model
fit_Imputation_Cox = surv_est(surv_data = surv_data,
         long_data = long_data,
         method = "Imputation_Cox",
         id_var = id_var,
         time = time,
         LM_fixedEffect_variables = LM_fixedEffect_variables,
         LM_randomEffect_variables = LM_randomEffect_variables,
         SM_timeVarying_variables = SM_timeVarying_variables,
         SM_timeInvariant_variables = SM_timeInvariant_variables,
         imp_time_factor = imp_time_factor)
fit_Imputation_Cox

# fit the VAImputation_Cox model
fit_VAImputation_Cox = surv_est(surv_data = surv_data,
         long_data = long_data,
         method = "VAImputation_Cox",
         id_var = id_var,
         time = time,
         LM_fixedEffect_variables = LM_fixedEffect_variables,
         LM_randomEffect_variables = LM_randomEffect_variables,
         SM_timeVarying_variables = SM_timeVarying_variables,
         SM_timeInvariant_variables = SM_timeInvariant_variables,
         imp_time_factor = imp_time_factor)
fit_VAImputation_Cox


### change the column names of the dataset and run the code again
surv_data1 = surv_data
colnames(surv_data1) = c("id","year","gender","mark","D","d")
long_data1 = long_data
colnames(long_data1) = c("id","year","gender","mark","time","Y1")

id_var = "id"
time = "time"
LM_fixedEffect_variables = c("year","gender","mark","time")
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
         LM_fixedEffect_variables = LM_fixedEffect_variables,
         LM_randomEffect_variables = LM_randomEffect_variables,
         SM_timeVarying_variables = SM_timeVarying_variables,
         SM_timeInvariant_variables = SM_timeInvariant_variables,
         imp_time_factor = imp_time_factor)
fit_cox
