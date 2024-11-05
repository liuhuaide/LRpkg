#' Print Method for ridgereg Class
#'
#' @param x An object of class "ridgereg".
#' @param ... Additional arguments (not used).
#' @export
print.ridgereg <- function(x, ...) {
  formula_str <- paste0("ridgereg(formula = ", deparse(x$formula), ", data = ", x$data_name, ", lambda = ", x$lambda, ")")
  cat(formula_str, "\n\n")
  cat("Coefficients:\n")
  coef_vector <- as.vector(x$coefficients)
  names(coef_vector) <- rownames(x$coefficients)
  print(coef_vector)
}


#' Prediction Method for ridgereg Class
#'
#' @param object An object of class "ridgereg".
#' @param ... Additional arguments (not used).
#' @return A numeric vector of fitted values.
#' @export
pred.ridgereg <- function(object, ...) {
  return(object$fitted_values)
}


#' Coefficient Method for ridgereg Class
#'
#' @param object An object of class "ridgereg".
#' @param ... Additional arguments (not used).
#' @return A numeric vector of coefficients.
#' @export
coef.ridgereg <- function(object, ...) {
  names(object$coefficients) <- colnames(object$fitted_values)
  return(object$coefficients)
}
