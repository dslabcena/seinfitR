#' Plot method for seinfitR objects
#'
#' This function generates a scatter plot of the observed data along with the fitted SEINH model.
#'
#' @param x An object of class `seinfitR` (output from `seinfitR()`).
#' @param rel Logical. If `TRUE`, make a graph of the relative model.
#' @param \dots currently unused.
#'
#' @return A plot showing the observed data (blue points) and the fitted curve (red line).
#' @importFrom graphics legend lines
#' @importFrom stats predict
#' @export
#'
plot.seinfitR <- function(x, rel=FALSE, ...) {
  data <- x$data
  x_var <- x$x
  y_var <- x$y

  # Adjust zero values on the x-axis
  data[[x_var]] <- ifelse(data[[x_var]] == 0, min(data[[x_var]][data[[x_var]] > 0]) / 10, data[[x_var]])

  # Set options to avoid scientific notation
  options(scipen=3)

  # Get fitted values
  fitted_values <- predict(x$fit, newdata = data)

  # Plot normalized values (relative mode)
  if (rel) {
    plot(data[[x_var]], data[[y_var]] / max(fitted_values), log = "x",
         main = "Seinhorst Model", xlab = paste("log(", x_var, ")"), ylab = paste("Relative ", y_var),
         col = "blue", pch = 16, type = "p")
    lines(data[[x_var]], fitted_values / max(fitted_values), col = "red", lwd = 2)
  } else {
    # Plot observed values without normalization
    plot(data[[x_var]], data[[y_var]], log = "x",
         main = "Seinhorst Model", xlab = paste("log(",x_var,")"), ylab = y_var,
         col = "blue", pch = 16, type = "p")
    lines(data[[x_var]], fitted_values, col = "red", lwd = 2)
  }

  # Add legend
  legend("topright", legend = c("Observed", "Fitted"), col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = 2)
}

