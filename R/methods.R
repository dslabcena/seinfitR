#' @title Methods for 'seinfitR' objects
#' @name seinfitR-methods
#' @param x an object of class \code{seinfitR}
#' @param object An object of class `seinfitR` (output from `seinfitR()`).
#' @param \dots currently unused.
#' @param digits minimal number of \emph{significant} digits, see
#'   \code{\link[base]{print.default}}
#' @importFrom stats coef vcov
#'
NULL


#-----------------------------------------------------------------------

# Print method
#' @rdname seinfitR-methods
#' @export

print.seinfitR <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("\nSeinhorst Model Fit Summary\n")
  cat("-----------------------------------------------------\n")
  cat("Dependent Variable:", x$y, "\n")
  cat("Predictor Variable:", x$x, "\n")
  cat("Number of Observations:", nrow(x$data), "\n\n")

  print(x$summary_seinfitR$coefficients)

  cat("-----------------------------------------------------\n")
  invisible(x)
}

#-----------------------------------------------------------------------

# vcov method
#' @rdname seinfitR-methods
#' @export

vcov.seinfitR <- function(object, ...) {
  vc <- vcov(object$fit)
  if (is.null(vc)) {
    warning("Covariance matrix not available.")
  }

  return(vc)
}

#-----------------------------------------------------------------------

# Summary method
#' @rdname seinfitR-methods
#' @export

summary.seinfitR <- function(object, ...) {
  cat("\nSeinhorst Model - Parameter Estimates\n")
  cat("-----------------------------------------------------\n")
  print(object$summary_seinfitR$coefficients)
  cat("-----------------------------------------------------\n")
  cat()
  cat("-----------------------------------------------------\n")
  invisible(object)
}

#-----------------------------------------------------------------------

# r_squared method
#' @rdname seinfitR-methods
#' @export r_squared
r_squared <- function(object, ...) UseMethod("r_squared")

#'@export

r_squared.seinfitR <- function(object, ...) {

  if (!inherits(object, "seinfitR")) {
    stop("Object is not of class 'seinfitR'")
  }

  fitted_values <- predict(object$fit)

  ss_total <- sum((object$data[[object$y]] - mean(object$data[[object$y]]))^2)
  ss_residual <- sum((object$data[[object$y]] - fitted_values)^2)
  r_squared <- 1 - (ss_residual / ss_total)


  n <- nrow(object$data)
  p <- length(coef(object$fit)) - 1

  adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))

  class(r_squared) <- "r_squared.seinfitR"
  class(adjusted_r_squared) <- "r_squared.seinfitR"

  cat("R² (Coefficient of Determination): ", r_squared, "\n")
  cat("Adjusted R²: ", adjusted_r_squared, "\n")

  invisible(object)
}

#-----------------------------------------------------------------------
