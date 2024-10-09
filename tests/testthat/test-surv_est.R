test_that("cox works", {
  # Setup parameters
  id_var = "id"
  time = "time"
  LM_fixedEffect_variables = c("Age","Sex","SNP","time") # Note: we need to make this consistent with the input as the long_est function
  LM_randomEffect_variables = c("SNP")
  SM_timeVarying_variables = c("Y")
  SM_timeInvariant_variables = c("Age","Sex","SNP")
  imp_time_factor = 1

  fit_cox <- surv_est(surv_data = surv_data,
                     long_data = long_data,
                     method = "cox",
                     id_var = id_var,
                     time = time,
                     LM_fixedEffect_variables = LM_fixedEffect_variables,
                     LM_randomEffect_variables = LM_randomEffect_variables,
                     SM_timeVarying_variables = SM_timeVarying_variables,
                     SM_timeInvariant_variables = SM_timeInvariant_variables,
                     imp_time_factor = imp_time_factor)
})

test_that("JMLD works", {
  # Setup parameters
  id_var = "id"
  time = "time"
  LM_fixedEffect_variables = c("Age","Sex","SNP","time") # Note: we need to make this consistent with the input as the long_est function
  LM_randomEffect_variables = c("SNP")
  SM_timeVarying_variables = c("Y")
  SM_timeInvariant_variables = c("Age","Sex","SNP")
  imp_time_factor = 1

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
})



test_that("VA_JMLD works", {
  # Setup parameters
  id_var = "id"
  time = "time"
  LM_fixedEffect_variables = c("Age","Sex","SNP","time") # Note: we need to make this consistent with the input as the long_est function
  LM_randomEffect_variables = c("SNP")
  SM_timeVarying_variables = c("Y")
  SM_timeInvariant_variables = c("Age","Sex","SNP")
  imp_time_factor = 1

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
})




test_that("Imputation_Cox works", {
  # Setup parameters
  id_var = "id"
  time = "time"
  LM_fixedEffect_variables = c("Age","Sex","SNP","time") # Note: we need to make this consistent with the input as the long_est function
  LM_randomEffect_variables = c("SNP")
  SM_timeVarying_variables = c("Y")
  SM_timeInvariant_variables = c("Age","Sex","SNP")
  imp_time_factor = 1

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
})





test_that("VAImputation_Cox works", {
  # Setup parameters
  id_var = "id"
  time = "time"
  LM_fixedEffect_variables = c("Age","Sex","SNP","time") # Note: we need to make this consistent with the input as the long_est function
  LM_randomEffect_variables = c("SNP")
  SM_timeVarying_variables = c("Y")
  SM_timeInvariant_variables = c("Age","Sex","SNP")
  imp_time_factor = 1

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
})











