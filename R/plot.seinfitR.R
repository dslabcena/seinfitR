#' Plot method for seinfitR objects
#'
#' This function generates a scatter plot of the observed data along with the fitted SEINH model.
#'
#' @param x An object of class `seinfitR` (output from `seinfitR()`).
#' @param \dots currently unused.
#'
#' @return A plot showing the observed data (blue points) and the fitted curve (red line).
#' @importFrom graphics legend lines
#' @importFrom stats predict
#' @export
plot.seinfitR <- function(x, ...) {
  data <- x$data
  x_var <- x$x
  y_var <- x$y

  # Create plot
  plot(data[[x_var]], data[[y_var]], main = "Seinhorst Model",
       xlab = "", ylab = "", col = "blue", pch = 16)

  # Add fitted curve
  fitted_values <- predict(x$fit, newdata = data)
  lines(data[[x_var]], fitted_values, col = "red", lwd = 2)

  legend("topright", legend = c("Observed", "Fitted"), col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = 2)
}
