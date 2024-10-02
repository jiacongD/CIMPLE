# Setup parameters---

## Variables in the models ###
# dataset
outcome_var = "Y"
time_var = "time"
id_var = "id"
VPM_variables = c("Z","X") # in the VP model
LM_fixedEffect_withoutTime_variables = c("Z","X") # in the LP model
LM_fixedEffect_withTime_variables = c(c("Z","X"),time_var) # in the LP model
LM_randomEffect_variables = "Z" # in the LP model
# Method = "LME" # All methods: LME, VA-LME, JMVL-LY, JMVL-Liang, JMVL-G, IIRR, imputation_LME

# All methods: LME, VA-LME, JMVL-LY, JMVL-Liang, JMVL-G, IIRR, imputation_LME
long_data = train_data
colnames(long_data)[which(colnames(long_data)==id_var)] = "id"
colnames(long_data)[which(colnames(long_data)==outcome_var)] = "Y"
colnames(long_data)[which(colnames(long_data)==time_var)] = "time"



test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
