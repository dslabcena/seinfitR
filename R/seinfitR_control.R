#' Custom Control Function for the SeinfitR Model Fitting
#'
#' This function returns a list of control parameters for the Levenberg-Marquardt algorithm
#' used by the \code{nlsLM} function from the \code{minpack.lm} package. These parameters
#' are specifically designed to control the fitting process in the \code{seinfitR} function.
#'
#' @title SeinfitR Control
#' @param ftol Termination condition for relative reduction in the sum of squares.
#' @param ptol Termination based on relative error between two consecutive iterations.
#' @param gtol Controls the orthogonality between the function vector and the Jacobian.
#' @param diag Multiplicative scale factors for the parameters.
#' @param epsfcn Step size for forward-difference approximation of the Jacobian.
#' @param factor Initial step bound factor.
#' @param maxfev Maximum number of function evaluations.
#' @param maxiter Maximum number of iterations.
#' @param nprint Controls printing of iteration details.
#' @param trace A logical value indicating if a trace of the iteration progress should be printed.
#'
#' @return A list of control parameters to be used in the \code{nlsLM} function during the
#' fitting of the Seinhorst model using \code{seinfitR}.
#' @export
seinfitR_control <- function(ftol = sqrt(.Machine$double.eps),
                             ptol = sqrt(.Machine$double.eps),
                             gtol = 0,
                             diag = list(),
                             epsfcn = 0,
                             factor = 100,
                             maxfev = integer(),
                             maxiter = 50,
                             nprint = 0,
                             trace = FALSE) {

  control_list <- list(
    ftol = ftol,
    ptol = ptol,
    gtol = gtol,
    diag = diag,
    epsfcn = epsfcn,
    factor = factor,
    maxfev = maxfev,
    maxiter = maxiter,
    nprint = nprint
  )

  return(list(control = control_list, trace = trace))
}
