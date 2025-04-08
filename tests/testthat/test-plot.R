test_that("plot() function runs without errors", {
  library(seinfitR)

  # Load dataset
  data(jambu)

  # Fit the model
  model <- seinfitR(p_i = "p_i", y = "y", data = jambu,
                    start = list(m = 0.103, t = 250, z = 0.991),
                    control = seinfitR.control(maxiter = 5))

  expect_silent(plot(model))
})
