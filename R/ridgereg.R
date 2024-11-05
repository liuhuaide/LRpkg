#' Ridge Regression using QR Decomposition
#'
#' This function performs ridge regression on a given dataset using QR decomposition
#' for solving the normal equations, while also normalizing the covariates.
#'
#' @param formula A formula specifying the model to be fitted. The response variable 
#'                should be on the left-hand side and the predictor variables on the right-hand side.
#' @param data A data frame containing the variables specified in the formula.
#' @param lambda A non-negative numeric value representing the ridge penalty term.
#' 
#' @return A list containing the following components:
#' \item{coefficients}{A numeric vector of estimated coefficients for the model.}
#' \item{fitted_values}{A numeric vector of fitted values from the model.}
#' \item{residuals}{A numeric vector of residuals from the model.}
#' \item{df}{Degrees of freedom for the model.}
#' \item{residual_variance}{Estimated variance of the residuals.}
#' \item{beta_variance}{Estimated variance-covariance matrix of the coefficients.}
#' \item{t_values}{A numeric vector of t-values for the estimated coefficients.}
#' \item{p_values}{A numeric vector of p-values corresponding to the t-values.}
#' \item{lambda}{The value of the ridge penalty used in the model.}
#' \item{formula}{The formula used to fit the model.}
#' \item{data}{The data frame used in the analysis.}
#' \item{gcv}{Generalized Cross-Validation score for the model.}
#'
#' @examples
#' data(mtcars)
#' ridge_result <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 1)
#' print(ridge_result$coefficients)
#' 
#' @export
ridgereg <- function(formula, data, lambda) {
  if (!is.numeric(lambda) || length(lambda) != 1 || lambda < 0) {
    stop("Error: lambda must be a non-negative number.")
  }
  
  X <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]
  
  # Standardize the independent variables
  X_intercept <- X[, -1]
  X_norm <- apply(X_intercept, 2, function(column) {
    (column - mean(column)) / sd(column)
  })
  X_norm <- cbind(Intercept = 1, X_norm)
  
  # Construct penalty matrix
  penalty_matrix <- diag(ncol(X_norm))
  penalty_matrix[1, 1] <- 0
  
  # Augmented matrix
  B <- rbind(X_norm, sqrt(lambda) * penalty_matrix)
  b <- c(y, rep(0, ncol(X_norm)))
  
  # QR decomposition
  qr_decomp <- qr(B)
  Q <- qr.Q(qr_decomp)
  R <- qr.R(qr_decomp)
  
  # Calculate ridge regression coefficients
  beta_hat <- solve(R, t(Q) %*% b)
  
  # Calculate fitted values and residuals
  fitted_values <- X %*% beta_hat
  residuals <- y - fitted_values
  
  n <- nrow(X)
  p <- ncol(X)
  df <- n - p
  
  # Residual variance
  residual_variance <- sum(residuals^2) / df
  beta_variance <- as.numeric(residual_variance) * solve(t(R) %*% R + lambda * diag(ncol(R)))
  
  # Calculate t-values and p-values
  t_values <- as.vector(beta_hat / sqrt(diag(beta_variance)))
  p_values <- 2 * pt(-abs(t_values), df)
  
  ridge_result <- list(
    coefficients = beta_hat,
    fitted_values = fitted_values,
    residuals = residuals,
    df = df,
    residual_variance = residual_variance,
    beta_variance = beta_variance,
    t_values = t_values,
    p_values = p_values,
    lambda = lambda,
    formula = formula,
    data = data,
    data_name = deparse(substitute(data))
  )
  
  class(ridge_result) <- "ridgereg"
  return(ridge_result)
}
