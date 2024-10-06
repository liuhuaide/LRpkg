#' Linear Regression Model using QR Decomposition
#'
#' This function performs a multiple linear regression using QR decomposition.
#' It calculates the regression coefficients, fitted values, residuals, degrees of freedom,
#' residual variance, variance of the regression coefficients, t-values, and p-values for each coefficient.
#'
#' @param formula A formula object describing the model to be fitted.
#' @param data A data.frame containing the variables in the model.
#'
#' @return An object of class "linreg" containing the model results.
#' @examples
#' data(mtcars)
#' model <- linreg(mpg ~ wt + hp, data = mtcars)
#' summary(model)
#'
#' @export
linreg <- function(formula, data) {
  X <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]

  #QR decomposition
  qr_decomp <- qr(X)
  Q <- qr.Q(qr_decomp)
  R <- qr.R(qr_decomp)

  beta_hat <- solve(R, t(Q) %*% y)

  fitted_values <- X %*% beta_hat
  residuals <- y - fitted_values

  n <- nrow(X)
  p <- ncol(X)
  df <- n - p

  residual_variance <- t(residuals) %*% residuals / df
  beta_variance <- as.numeric(residual_variance) * solve(t(R) %*% R)

  t_values <- as.vector(beta_hat / sqrt(diag(beta_variance)))
  p_values <- 2 * pt(-abs(t_values), df)

  data_name <- deparse(substitute(data))

  LR_result <- list(
    coefficients = beta_hat,
    fitted_values = fitted_values,
    residuals = residuals,
    df = df,
    residual_variance = residual_variance,
    beta_variance = beta_variance,
    t_values = t_values,
    p_values = p_values,
    formula = formula,
    data = data,
    data_name = as.name(data_name)
  )
  class(LR_result) <- "linreg"
  return(LR_result)

}

