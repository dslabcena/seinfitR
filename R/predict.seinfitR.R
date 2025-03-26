#' Predict method for seinfitR objects
#'
#' This function generates predictions based on a fitted Seinhorst model.
#'
#' @param object An object of class `seinfitR` (output from `seinfitR()`).
#' @param newdata Optional. A data frame containing the independent variable for which predictions should be made.
#' If not provided, predictions are made for the original data.
#' @param \dots currently unused.
#'
#' @return A data frame with the independent variable and the corresponding predicted values.
#' @export

predict.seinfitR <- function(object, newdata = NULL, ...) {
  # If newdata is NULL, use the original dataset
  if (is.null(newdata)) {
    newdata <- object$data
  }

  # Ensure the independent variable exists in newdata
  if (!(object$x %in% names(newdata))) {
    stop("New dataset must contain the independent variable: ", object$x)
  }

  new_x <- newdata[[object$x]]


  #new_x <- append(new_x, coef(object)["t"])

  #cat(unlist(new_x))


  # Compute predictions
  predictions <- predict(object$fit, newdata = newdata, ...)

  return()
}
