#' Fit the Seinhorst Model to Experimental Data
#'
#' This function fits the Seinhorst equation to experimental data on the relationship between
#' preplant nematode densities and plant growth using nonlinear least squares fitting.
#' The function uses the `nlsLM` function from the `minpack.lm` package to estimate the parameters
#' of the model.
#'
#
#' @param x dados de x
#' @param y dados de y
#'
#' @param data A data frame containing the experimental data. It should include at least
#' two columns: `x` (preplant nematode densities) and `y` (plant growth response).
#'
#' @param start A list of starting values for the parameters `m`, `t`, and `z`. These values
#' are used to initialize the nonlinear least squares fitting process.
#'

#'
#' @return An object of class `nls`, which contains the results of the nonlinear least squares fitting,
#' including estimated parameters and residuals.
#'
#' @param control control
#'
#' @export
#'
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @importFrom stats median
#'
#' @examples
#' library("readxl")
## Load example data from the inst/extdata folder
#'data <- read_excel(system.file("extdata", "raw_data_pathogenicity_test.xlsx", package = "seinfitR"))
#'colnames(data)[5] ="y"
#'colnames(data)[1] ="p_i"
#
#' # Fit the model using seinfitR with initial parameter values and control settings
#' seinfitR(x="p_i", y="y", data = data, start = list(m = 0.103, t = 250, z = 0.991),
#'          control = seinfitR_control(maxiter = 100))
#'
#'
#'
# seinfitR <- function(data, start = NULL, control = seinfitR_control(maxiter = 100)) {
#
#   # If start is NULL, ask the user for input
#   if (is.null(start)) {
#     m <- as.numeric(readline(prompt = "Please enter the value for m: "))
#     t <- as.numeric(readline(prompt = "Please enter the value for t: "))
#     z <- as.numeric(readline(prompt = "Please enter the value for z: "))
#
#     # Create a start list with user inputs
#     start <- list(m = m, t = t, z = z)
#   }
#
#   # Perform the fitting
#   fit <- nlsLM(
#     y ~ ifelse(x <= t,
#                mean(y[x <= t]),
#                (mean(y[x <= t]) * m) + (mean(y[x <= t]) * (1 - m) * z^(x - t))),
#     start = start,
#     control = control,
#     lower = c(0, 0, 0),
#     upper = c(max(data$y), max(data$x), 1),
#     algorithm = "LM",
#     trace = TRUE,
#     data = data
#   )
#
#   sumario <- summary(fit)
#   cov <- vcov(fit)
#
#   list_resul <- list("fit" = fit, "Sumario" = sumario, "cov" = cov)
#   return(list_resul)
# }



seinfitR <- function(x = "x", y = "y", data, start = NULL,
                     control = seinfitR_control()) {

  # Verificar se os nomes das colunas existem no conjunto de dados
  if (!(x %in% names(data)) || !(y %in% names(data))) {
    stop("The specified columns do not exist in the dataset.")
  }

  # Extrair as colunas corretas do dataset
  x_data <- data[[x]]
  y_data <- data[[y]]

  # Ensure values are numeric and contain no NAs
  if (!is.numeric(x_data) || !is.numeric(y_data) || anyNA(x_data) || anyNA(y_data)) {
    stop("The selected columns must contain only numeric values and cannot have NAs.")
  }

  # If start is NULL, ask the user for input
    if (is.null(start)) {
       m <- as.numeric(readline(prompt = "Please enter the value for m: "))
       t <- as.numeric(readline(prompt = "Please enter the value for t: "))
   z <- as.numeric(readline(prompt = "Please enter the value for z: "))

      # Create a start list with user inputs
      start <- list(m = m, t = t, z = z)
  }

  # Ajuste do modelo
  fit <- tryCatch({
    nlsLM(
      y_data ~ (x_data <= t) * mean(y_data[x_data <= t]) +
        (x_data > t) * ((mean(y_data[x_data <= t]) * m) +
                          (mean(y_data[x_data <= t]) * (1 - m) * z^(x_data - t))),
      start = start,
      control = control,
      lower = c(0, min(x_data), 0),
      upper = c(max(y_data), max(x_data), 1),
      algorithm = "LM",
      trace = TRUE
    )
  }, error = function(e) {
    stop("Error in model fitting: ", e$message)
  })

  # Criar a lista de resultados
  result <- list(
    fit = fit,
    summary_seinfitR = summary(fit),
    cov = tryCatch(vcov(fit), error = function(e) NULL), # Evita erro caso vcov não seja computável
    data = data,
    x = x,
    y = y
  )

  class(result) <- "seinfitR" #Assign the clas
  return(result)
}

