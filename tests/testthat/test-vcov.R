test_that("vcov() method returns a valid variance-covariance matrix", {
  library(seinfitR)

  # Load dataset
  data(jambu)

  # Fit the model
  model <- seinfitR(p_i = "p_i", y = "y", data = jambu,
                    start = list(m = 0.103, t = 250, z = 0.991),
                    control = seinfitR.control(maxiter = 5))


  vcov_matrix <- vcov(model)

  # Check dimensions
  expect_equal(dim(vcov_matrix), c(4, 4))

  # Ensure matrix contains only numeric values
  expect_type(vcov_matrix, "double")
})
