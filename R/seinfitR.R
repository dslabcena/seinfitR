#' Fit the Seinhorst Model to Experimental Data
#'
#' This function fits the Seinhorst equation to experimental data describing the relationship
#' between preplant nematode densities and plant growth using nonlinear least squares fitting.
#' The fitting process is performed using the `nlsLM` function from the `minpack.lm` package.
#'
#' @param p_i A character string specifying the column name in `data` that contains preplant nematode densities.
#' @param y A character string specifying the column name in `data` that contains the plant growth response.
#' @param data A data frame containing the experimental data. It must include at least two columns:
#' one representing the preplant nematode densities (`p_i`) and another representing the plant growth response (`y`).
#' @param start A list of initial parameter values for `m`, `t`, and `z` (if `z_fixed = FALSE`). These values
#' are used to initialize the nonlinear least squares fitting process.
#' @param z_fixed Logical. If `TRUE`, the function uses the default value for \( z^t \), as described in
#' Seinhorst, J. W. (1986). *Effects of nematode attack on the growth and yield of crop plants*. In *Cyst nematodes* (pp. 191-209). Springer US.
#' @param control A control object created using `seinfitR_control()`, which specifies options for the optimization process.
#'
#' @return A list of class `"seinfitR"` containing:
#' \item{fit}{An object of class `nls` with the fitted model.}
#' \item{summary_seinfitR}{Summary statistics of the fitted model.}
#' \item{cov}{The covariance matrix of parameter estimates (if available).}
#' \item{data}{The original dataset used for fitting.}
#' \item{x}{The name of the predictor variable used (`p_i`).}
#' \item{y}{The name of the response variable used (`y`).}
#' \item{z_fixed}{Logical value indicating whether `z` was fixed.}
#'
#' @export
#'
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @importFrom stats median
#'
#' @examples
#' # Example: Modeling plant response to nematode densities using "jambu" dataset
#'
#' # Fit the model using seinfitR with specified initial values
#' model <- seinfitR(p_i = "p_i", y = "y", data = jambu,
#'                   start = list(m = 0.103, t = 250, z = 0.991),
#'                   control = seinfitR_control(maxiter = 5))
#'
#' # View model summary
#' summary(model)



seinfitR <- function(p_i = NULL, y = NULL, data = NULL, start = NULL, z_fixed = FALSE, control = seinfitR_control()) {
  #Check for data argument
  if (missing(data)){
    stop("Error: The 'data' argument is missing. Please provide a data frame containing the experimental data.")
  }

  #Check if p_i (preditor) and y (dependent) is specified
  if (is.null(p_i)){
    p_i <- readline(prompt = "Please enter the value for p_i (Predictor variable) in your data frame: ")
  }
  if (is.null(y)){
    y <- readline(prompt = "Please enter the value for y (Dependent variable) in your data frame: ")
  }

  # Check if the specified column names exist in the dataset
  if (!(p_i %in% names(data)) || !(y %in% names(data))) {
    stop("The specified columns do not exist in the dataset.")
  }

  # Extract the corresponding columns from the dataset
  x_data <- data[[p_i]]
  y_data <- data[[y]]

  # Ensure that values are numeric and contain no missing values (NA)
  if (!is.numeric(x_data) || !is.numeric(y_data) || anyNA(x_data) || anyNA(y_data)) {
    stop("The selected columns must contain only numeric values and cannot have NAs.")
  }

  # If no starting values are provided, request user input
  if (is.null(start) && !z_fixed) {
    m <- as.numeric(readline(prompt = "Please enter the value for m: "))
    t <- as.numeric(readline(prompt = "Please enter the value for t: "))
    z <- as.numeric(readline(prompt = "Please enter the value for z: "))

    start <- list(m = m, t = t, z = z)
  }

  if (is.null(start) && z_fixed) {
    m <- as.numeric(readline(prompt = "Please enter the value for m: "))
    t <- as.numeric(readline(prompt = "Please enter the value for t: "))

    start <- list(m = m, t = t)
  }

  # Compute the maximum plant growth response (y_max) based on values where x_data <= t
  y_max <- mean(y_data[x_data <= start$t])

  # Extract the control settings
  control_list <- control$control
  trace <- control$trace

  # Fit the Seinhorst model based on whether z is fixed
  fit <- tryCatch({
    if (z_fixed) {

      cat("Z_fixed option is True: the default value for z^t described by Seinhorst (1986) was used.\n")
      # If z is fixed, use the predefined value in the model equation
      nlsLM(
        y_data ~ ifelse(x_data <= t,
                        y_max,
                        (y_max * m) + (y_max * (1 - m) * 0.95^(x_data * t^(-1) - 1))),
        start = list(m = start$m, t = start$t, y_max = y_max),
        control = control_list,
        lower = c(0, min(x_data), min(x_data)),  # Lower bounds for parameter estimation
        upper = c(max(y_data), max(x_data), max(x_data)),  # Upper bounds
        algorithm = "LM",
        trace = trace
      )
    } else {
      # If z is not fixed, estimate it along with the other parameters
      nlsLM(
        y_data ~ ifelse(x_data <= t,
                        y_max,
                        (y_max * m) + (y_max * (1 - m) * z^(x_data - t))),
        start = list(m = start$m, t = start$t, z = start$z, y_max = y_max),
        control = control_list,
        lower = c(0, min(x_data), 0, min(x_data)),
        upper = c(max(y_data), max(x_data), 1, max(y_data)),
        algorithm = "LM",
        trace = trace
      )
    }
  }, error = function(e) {
    stop("Error in model fitting: ", e$message)
  })

  # Create the result object with relevant model details
  result <- list(
    fit = fit,  # Fitted model object
    summary_seinfitR = summary(fit),  # Model summary
    cov = tryCatch(vcov(fit), error = function(e) NULL),  # Covariance matrix (if available)
    data = data,  # Original dataset
    x = p_i,  # Name of predictor variable
    y = y,  # Name of response variable
    z_fixed = z_fixed  # Whether z was fixed during fitting
  )

  class(result) <- "seinfitR"  # Assign custom class for the result object

  cat("Model fitting successful\n")
  return(result)
}

