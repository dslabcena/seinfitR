#' Print contents of seinfitR object.
#'
#' @title Print SeinfitR
#' @param x Object of class 'seinfitR'.
#' @param \dots currently unused.
#' @param digits minimal number of \emph{significant} digits, see
#'   \code{\link[base]{print.default}}
#' @return No return value, called for side effects
#' @seealso \code{\link{seinfitR}}
#' @export
print.seinfitR <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("Seinhorst Model Fit Summary\n")
  cat("-----------------------------------------------------\n")
  cat("Dependent Variable:             ", x$y, "\n")
  cat("Predictor Variable:             ", x$x, "\n")
  cat("Number of Observations:         ", nrow(x$data), "\n\n")
  cat("Coefficients:\n")
  print(x$summary_seinfitR$coefficients, digits = digits)
  cat("-----------------------------------------------------\n")
}

#' Compute variance-covariance matrix for seinfitR model.
#'
#' @title Variance-Covariance Matrix
#' @param object Object of class 'seinfitR'.
#' @param \dots currently unused.
#' @return A matrix representing the covariance of the estimated coefficients.
#' @importFrom stats vcov
#' @export
vcov.seinfitR <- function(object, ...) {
  vc <- vcov(object$fit)
  if (is.null(vc)) {
    warning("Covariance matrix not available.")
  }
  return(vc)
}

#' Display a summary of the seinfitR model.
#'
#' @title Summary of seinfitR Model
#' @param object Object of class 'seinfitR'.
#' @param \dots currently unused.
#' @return No return value, called for side effects.
#' @seealso \code{\link{seinfitR}}
#' @export
summary.seinfitR <- function(object, ...) {
  r_sq <- r_squared(object)
  cat("Seinhorst Model - Parameter Estimates\n")
  cat("-----------------------------------------------------\n")
  print(object$summary_seinfitR$coefficients)
  cat("-----------------------------------------------------\n")
  cat("R2 - R squared (Coefficient of Determination): ", r_sq$R2, "\n")
  cat("Adjusted_R2 - Adjusted R squared:              ", r_sq$Adjusted_R2, "\n")
  cat("-----------------------------------------------------\n")
}

#' Compute R-squared for seinfitR model.
#'
#' @title R-squared Calculation
#' @param object Object of class 'seinfitR'.
#' @param \dots currently unused.
#' @return A list with the following components:
#' \describe{
#'   \item{R2}{The coefficient of determination (R-squared).}
#'   \item{Adjusted_R2}{The adjusted R-squared value.}
#' }
#' @export
r_squared <- function(object, ...) UseMethod("r_squared")

#' @export
r_squared.seinfitR <- function(object, ...) {
  if (!inherits(object, "seinfitR")) {
    stop("Object is not of class 'seinfitR'")
  }
  
  fitted_values <- predict(object$fit)
  ss_total <- sum((object$data[[object$y]] - mean(object$data[[object$y]]))^2)
  ss_residual <- sum((object$data[[object$y]] - fitted_values)^2)
  r_sq <- 1 - (ss_residual / ss_total)
  
  n <- nrow(object$data)
  p <- length(coef(object$fit)) - 1
  adj_r_sq <- 1 - ((1 - r_sq) * (n - 1) / (n - p - 1))
  
  return(list(R2 = r_sq, Adjusted_R2 = adj_r_sq))
}

#' Extract model coefficients from a seinfitR object.
#'
#' @title Extract Coefficients
#' @param object Object of class 'seinfitR'.
#' @param \dots currently unused.
#' @return A named numeric vector containing the estimated model coefficients.
#' @importFrom stats coef
#' @export
coef.seinfitR <- function(object, ...) {
  coef_values <- coef(object$fit)
  if (is.null(coef_values)) {
    warning("The model should be run successfully first.")
    return(NULL)
  }
  return(coef_values)
}
