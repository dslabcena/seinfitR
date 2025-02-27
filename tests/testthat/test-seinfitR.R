# test_that("SeinfitR model fitting works", {
#   # Load the dataset
#   library(readxl)
#   dataset <- readxl::read_excel(system.file("extdata", "raw_data_pathogenicity_test.xlsx", package = "seinfitR"))
#   colnames(dataset)[5] <- "y"
#   colnames(dataset)[1] <- "pi"
#
#   # Define starting values for the parameters
#   start <- list(m = 0.103, t = 250, z = 0.991)
#
#   # Fit the Seinhorst model
#   result <- seinfitR(dataset, start)
#
#   # Check that the result is a named numeric vector and its components are within expected ranges
#   expect_equal(length(result), 3)  # The result should have 3 parameters: m, t, z
#   expect_true(all(names(result) %in% c("m", "t", "z")))  # Ensure the parameters have the right names
#
#   # Test if the values of parameters are reasonable (you may need to adjust the tolerance based on your data)
#   expect_true(result$m > 0 && result$m < 0.2)  # Expected m range (this is an example, adjust as needed)
#   expect_true(result$t > 200 && result$t < 400)  # Expected t range (example, adjust as needed)
#   expect_true(result$z > 0.9 && result$z < 1)  # Expected z range (example, adjust as needed)
# })
