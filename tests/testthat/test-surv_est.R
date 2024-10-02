# Setup parameters---

# define the variables
id_var = "id"
time_var = "time"
SM_variables = c("Z","X","Y")
SM_base_variables = c("Z","X")
LM_fixedEffect_withTime_variables = c("Z","X","time")
LM_fixedEffect_withoutTime_variables = c("Z","X")
LM_randomEffect_variables = c("Z")
# survival_time = "D" # add the arguement
# survival_event = "d"

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
