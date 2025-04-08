test_that("SeinfitR model fitting and coef method works", {
  library(seinfitR)

  # Load dataset
  data(jambu)

  # Fit the model
  model <- seinfitR(p_i = "p_i", y = "y", data = jambu,
                    start = list(m = 0.103, t = 250, z = 0.991),
                    control = seinfitR_control(maxiter = 5))


  expect_s3_class(model, "seinfitR")


  coef_values <- coef(model)

  # Ensure parameters are present and within expected ranges
  expect_true(all(names(coef_values) %in% c("m", "t", "z", "y_max")))
  expect_true(coef_values["m"] > 0 && coef_values["m"] < 0.2)
  expect_true(coef_values["t"] > 200 && coef_values["t"] < 400)
  expect_true(coef_values["z"] > 0.9 && coef_values["z"] < 1)
  expect_true(coef_values["y_max"] == 1)

  # Check summary function
  expect_output(summary(model), "Seinhorst Model - Parameter Estimates")
})
