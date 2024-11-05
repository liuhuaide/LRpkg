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
  
  # 提取设计矩阵和响应变量
  X <- model.matrix(formula, data)  # 包含截距项
  y <- data[[all.vars(formula)[1]]]
  
  # 标准化自变量（不包括截距项）
  X_intercept <- X[, -1]
  #X_norm <- scale(X_intercept)  # 标准化自变量
  X_norm <- apply(X_intercept, 2, function(column) {
    (column - mean(column)) / sd(column)
  })
  X_norm <- cbind(Intercept = 1, X_norm)  # 重新添加截距列
  
  # 构建惩罚矩阵，确保截距项不受惩罚
  penalty_matrix <- diag(ncol(X_norm))
  penalty_matrix[1, 1] <- 0  # 不惩罚截距项
  
  # 增广矩阵，将设计矩阵和惩罚矩阵结合
  B <- rbind(X_norm, sqrt(lambda) * penalty_matrix)
  b <- c(y, rep(0, ncol(X_norm)))
  
  # QR 分解
  qr_decomp <- qr(B)
  Q <- qr.Q(qr_decomp)
  R <- qr.R(qr_decomp)
  
  # 计算岭回归系数
  beta_hat <- solve(R, t(Q) %*% b)
  
  # 计算拟合值和残差
  fitted_values <- X %*% beta_hat  # 拟合值包含截距项
  residuals <- y - fitted_values
  
  n <- nrow(X)
  p <- ncol(X)
  df <- n - p
  
  # 残差方差
  residual_variance <- sum(residuals^2) / df
  beta_variance <- as.numeric(residual_variance) * solve(t(R) %*% R + lambda * diag(ncol(R)))
  
  # 计算t值和p值
  t_values <- as.vector(beta_hat / sqrt(diag(beta_variance)))
  p_values <- 2 * pt(-abs(t_values), df)
  
  # 计算 GCV（广义交叉验证）
  gcv <- (n * residual_variance) / (n - p)^2
  
  # 创建结果列表
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
    data_name = deparse(substitute(data)),
    gcv = gcv  # 添加 GCV 值
  )
  
  class(ridge_result) <- "ridgereg"
  return(ridge_result)
}