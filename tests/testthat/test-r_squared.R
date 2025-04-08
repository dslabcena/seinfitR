test_that("r_squared() method returns valid R² values", {
  library(seinfitR)

  # Load dataset
  data(jambu)

  # Fit the model
  model <- seinfitR(p_i = "p_i", y = "y", data = jambu,
                    start = list(m = 0.103, t = 250, z = 0.991),
                    control = seinfitR.control(maxiter = 5))

  # Extract R² values
  r2 <- r_squared(model)
  # Check that R² and adjusted R² are valid
  expect_true(r2$R2 >= 0 && r2$R2 <= 1)
  expect_true(r2$Adjusted_R2 >= 0 && r2$Adjusted_R2 <= 1)

  # Ensure R² is exactly 1 in this case
  expect_equal(r2$R2, 1, tolerance = 1e-6)
})
