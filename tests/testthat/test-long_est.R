test_that("The `standard_lme` method in long_est() works", {
  # Setup parameters
  outcome_var <- "Y"
  time_var <- "time"
  id_var <- "id"
  VPM_variables <- c("Z", "X")
  LM_fixedEffect_variables <- c("Z", "X")
  LM_randomEffect_variables <- "Z"

  # Fit model
  fit_standardLME <- long_est(long_data = train_data, method = "standard_LME",
                              id_var = id_var, outcome_var = outcome_var, time = time_var,
                              LM_fixedEffect_variables = LM_fixedEffect_variables,
                              LM_randomEffect_variables = LM_randomEffect_variables)
  # Extract coefficients
  beta_hat <- unname(fit_standardLME$beta_hat)
  beta_sd <- unname(fit_standardLME$beta_sd)

  # Check beta_hat intercept
  expect_equal(round(beta_hat[1], 3), -2.185)
  # Check beta_hat Z
  expect_equal(round(beta_hat[2], 3), -0.473)
  # Check beta_hat X
  expect_equal(round(beta_hat[3], 3), 0.591)
  # Check beta_hat time
  expect_equal(round(beta_hat[4], 3), 0.100)


  # Check beta_sd intercept
  expect_equal(round(beta_sd[1], 3), 0.079)
  # Check beta_sd Z
  expect_equal(round(beta_sd[2], 3), 0.137)
  # Check beta_sd X
  expect_equal(round(beta_sd[3], 3), 0.066)
  # Check beta_sd time
  expect_equal(round(beta_sd[4], 3), 0.001)
})
