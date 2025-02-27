#' @title Methods for 'seinfitR' objects
#' @name seinfitR-methods
#' @param object an object of class \code{seinfitR}
#' @param x an object of class \code{seinfitR}
#' @param \dots currently unused.
#' @param digits minimal number of \emph{significant} digits, see
#'   \code{\link[base]{print.default}}
#' @return .
#' @importFrom stats coef vcov
#'
NULL

#-----------------------------------------------------------------------

# Print method
#' @rdname seinfitR-methods
#' @export
print.seinfitR <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("\nSeinhorst Model Fit\n")

  cat("Parameters:\n")
  print.default(format(coef(x$fit), digits = digits), print.gap = 2, quote = FALSE)

  cat("\n")
  cat("For more details, run the summary function")

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
  summary(object$fit)
}
