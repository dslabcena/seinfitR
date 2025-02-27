#' Fit the Seinhorst Model to Experimental Data
#'
#' This function fits the Seinhorst equation to experimental data on the relationship between
#' preplant nematode densities and plant growth using nonlinear least squares fitting.
#' The function uses the `nlsLM` function from the `minpack.lm` package to estimate the parameters
#' of the model.
#'
#' @param data A data frame containing the experimental data. It should include at least
#' two columns: `pi` (preplant nematode densities) and `y` (plant growth response).
#'
#' @param start A list of starting values for the parameters `m`, `t`, and `z`. These values
#' are used to initialize the nonlinear least squares fitting process.
#'
#' @return An object of class `nls`, which contains the results of the nonlinear least squares fitting,
#' including estimated parameters and residuals.
#'
#' @param control control
#'
#' @export
#'
#' @importFrom minpack.lm nlsLM nls.lm.control
#'
#' @examples
#' # example code
#'
seinfitR <- function(data, start, control = seinfitR_control(maxiter=100)){
  nlsLM(
    y ~ ifelse(x <= t,
               mean(y[x <= t]),
               (mean(y[x <= t]) * m) + (mean(y[x <= t]) * (1 - m) * z^(x - t))),
    start = list(m = start$m, t = start$t, z = start$z),
    control = control,
    lower = c(0, 0, 0),
    upper = c(max(data$y), max(data$x), 1),
    algorithm = "LM",
    trace = TRUE,
    data = data
  )
}

