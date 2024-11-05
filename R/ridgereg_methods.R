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


#' Predict Fitted Values from Ridge Regression Model
#'
#' This function predicts the fitted values from a ridge regression model. It can handle new data
#' or use the data originally used to fit the model if no new data is provided. The function
#' standardizes the predictor variables using the mean and standard deviation from the original data.
#'
#' @param object A ridge regression model object returned by the \code{ridgereg} function.
#' @param newdata A data frame containing the new data for which predictions are to be made.
#' If \code{NULL} (default), the original data used to fit the model will be used.
#' @param ... Additional arguments to be passed to methods.
#'
#' @return A numeric vector of fitted values corresponding to the new data.
#' @export
pred.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    newdata <- object$data
  }
  
  beta_hat <- object$coefficients
  X_new <- model.matrix(object$formula, newdata)
  
  X_new_intercept <- X_new[, -1]
  
  X_new_norm <- scale(X_new_intercept, center = sapply(colnames(X_new_intercept), function(col) mean(object$data[[col]])), 
                      scale = sapply(colnames(X_new_intercept), function(col) sd(object$data[[col]])))
  
  X_new_norm <- cbind(Intercept = 1, X_new_norm)
  
  fitted_values_new <- X_new_norm %*% beta_hat
  return(fitted_values_new)
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
