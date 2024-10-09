# Test that ---------
test_that("The `standard_LME` method in long_est() works", {
  # Set arguments
  time_var <- "time"
  id_var <- "id"
  outcome_var <- "Y"
  VPM_variables <- c("Z", "X")
  LM_fixedEffect_variables <- c("Z", "X")
  LM_randomEffect_variables <- "Z"

  # Fit the standard LME model
  standard_LME_fit <- long_est(
    long_data = train_data,
    method = "standard_LME",
    id_var = "id",
    outcome_var = outcome_var,
    LM_fixedEffect_variables = LM_fixedEffect_variables,
    time = "time",
    LM_randomEffect_variables = LM_randomEffect_variables,
    VPM_variables = VPM_variables,
    optCtrl = list(method = "nlminb", kkt = FALSE, tol = 0.2, maxit = 20000),
    control = list(
      verbose = FALSE,
      tol = 1e-3,
      GHk = 10,
      maxiter = 150
    )
  )
})

test_that("The `VA_LME` method in long_est() works", {
  # Set arguments
  time_var <- "time"
  id_var <- "id"
  outcome_var <- "Y"
  VPM_variables <- c("Z", "X")
  LM_fixedEffect_variables <- c("Z", "X")
  LM_randomEffect_variables <- "Z"

  VA_LME_fit <- long_est(
    long_data = train_data,
    method = "VA_LME",
    id_var = id_var,
    outcome_var = outcome_var,
    LM_fixedEffect_variables = LM_fixedEffect_variables,
    time = time_var,
    LM_randomEffect_variables = LM_randomEffect_variables,
    VPM_variables = VPM_variables,
    control = list(
      verbose = FALSE,
      tol = 1e-3,
      GHk = 10,
      maxiter = 150
    )
  )
})

test_that("The `JMVL_LY` method in long_est() works", {
  # Set arguments
  time_var <- "time"
  id_var <- "id"
  outcome_var <- "Y"
  VPM_variables <- c("Z", "X")
  LM_fixedEffect_variables <- c("Z", "X")
  LM_randomEffect_variables <- "Z"

  # JMVL_LY
  fit_JMVL_LY <- long_est(
    long_data = train_data,
    method = "JMVL_LY",
    id_var = id_var,
    outcome_var = outcome_var,
    LM_fixedEffect_variables = LM_fixedEffect_variables,
    time = time_var,
    LM_randomEffect_variables = LM_randomEffect_variables,
    VPM_variables = VPM_variables,
    control = list(
      verbose = FALSE,
      tol = 1e-3,
      GHk = 10,
      maxiter = 150
    )
  )
})


# IIRR_weighting
test_that("The `IIRR_weighting` method in long_est() works", {
  # Set arguments
  time_var <- "time"
  id_var <- "id"
  outcome_var <- "Y"
  VPM_variables <- c("Z", "X")
  LM_fixedEffect_variables <- c("Z", "X")
  LM_randomEffect_variables <- "Z"

  fit_IIRR_weighting <- long_est(
    long_data = train_data,
    method = "IIRR_weighting",
    id_var = id_var,
    outcome_var = outcome_var,
    LM_fixedEffect_variables = LM_fixedEffect_variables,
    time = time_var,
    LM_randomEffect_variables = LM_randomEffect_variables,
    VPM_variables = VPM_variables,
    control = list(
      verbose = FALSE,
      tol = 1e-3,
      GHk = 10,
      maxiter = 150
    )
  )
})


# JMVL_G
test_that("The `JMVL_G` method in long_est() works", {
  # Set arguments
  time_var <- "time"
  id_var <- "id"
  outcome_var <- "Y"
  VPM_variables <- c("Z", "X")
  LM_fixedEffect_variables <- c("Z", "X")
  LM_randomEffect_variables <- "Z"

  fit_JMVL_G <- long_est(
    long_data = train_data,
    method = "JMVL_G",
    id_var = id_var,
    outcome_var = outcome_var,
    LM_fixedEffect_variables = LM_fixedEffect_variables,
    time = time_var,
    LM_randomEffect_variables = LM_randomEffect_variables,
    VPM_variables = VPM_variables,
    control = list(
      verbose = TRUE,
      tol = 1e-3,
      GHk = 20,
      maxiter = 150
    )
  )
})


# JMVL_Liang
test_that("The `JMVL_Liang` method in long_est() works", {
  # Set arguments
  time_var <- "time"
  id_var <- "id"
  outcome_var <- "Y"
  VPM_variables <- c("Z", "X")
  LM_fixedEffect_variables <- c("Z", "X")
  LM_randomEffect_variables <- "Z"

  fit_JMVL_Liang <- long_est(
    long_data = train_data,
    method = "JMVL_Liang",
    id_var = id_var,
    outcome_var = outcome_var,
    LM_fixedEffect_variables = LM_fixedEffect_variables,
    time = time_var,
    LM_randomEffect_variables = LM_randomEffect_variables,
    VPM_variables = VPM_variables,
    control = list(
      verbose = FALSE,
      tol = 1e-3,
      GHk = 10,
      maxiter = 150
    )
  )
})

# imputation_LME
test_that("The `imputation_LME` method in long_est() works", {
  # Set arguments
  time_var <- "time"
  id_var <- "id"
  outcome_var <- "Y"
  VPM_variables <- c("Z", "X")
  LM_fixedEffect_variables <- c("Z", "X")
  LM_randomEffect_variables <- "Z"

  fit_imputation_LME <- long_est(
    long_data = train_data,
    method = "imputation_LME",
    id_var = id_var,
    outcome_var = outcome_var,
    LM_fixedEffect_variables = LM_fixedEffect_variables,
    time = time_var,
    LM_randomEffect_variables = LM_randomEffect_variables,
    VPM_variables = VPM_variables,
    imp_time_factor = 1,
    control = list(
      verbose = FALSE,
      tol = 1e-3,
      GHk = 10,
      maxiter = 150
    )
  )
})




