#' @title Methods for 'seinfitR' objects
#' @name seinfitR-methods
#' @param x an object of class \code{seinfitR}
#' @param object An object of class `seinfitR` (output from `seinfitR()`).
#' @param \dots currently unused.
#' @param digits minimal number of \emph{significant} digits, see
#'   \code{\link[base]{print.default}}
#' @return Prints a summary of the model fit to the console.
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
  cat("Independent Variable:", x$x, "\n")
  cat("Number of Observations:", nrow(x$data), "\n\n")

  print(x$summary_seinfitR$coefficients)

  cat("-----------------------------------------------------\n")
  invisible(x)
}

#-----------------------------------------------------------------------

# Print method
#@rdname seinfitR-methods
# @export
# print.seinfitR <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
#   cat("\nSeinhorst Model Fit\n")
#
#   cat("Parameters:\n")
#   print.default(format(coef(x$fit), digits = digits), print.gap = 2, quote = FALSE)
#
#   cat("\n")
#   cat("For more details, run the summary function")
#
#   invisible(x)
# }

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
# summary.seinfitR <- function(object, ...) {
#   summary(object$fit)
# }


summary.seinfitR <- function(object, ...) {
  cat("\nSeinhorst Model - Parameter Estimates\n")
  cat("-----------------------------------------------------\n")
  print(object$summary_seinfitR$coefficients)
  cat("-----------------------------------------------------\n")
  invisible(object)
}
