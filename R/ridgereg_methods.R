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


#' pred.ridgereg: Generate predictions from a ridge regression model
#'
#' This function generates predictions based on a ridge regression model created with `ridgereg`.
#' It can either use the original data (if `newdata` is not provided) or make predictions
#' on a new dataset.
#'
#' @param object An object of class `ridgereg`, which contains the ridge regression model, 
#'               the fitted coefficients, and the original data used to build the model.
#' @param newdata A data frame containing new observations for the predictors. If provided, 
#'                predictions will be generated for these new observations. If NULL (the default), 
#'                the function will use the original data from `object`.
#' @param ... Additional arguments (currently not used).
#'
#' @return A vector of predicted values. If `newdata` is provided, the predictions are based on 
#'         this data. Otherwise, the predictions are based on the original data in `object`.
#' @export
pred.ridgereg <- function(object, newdata = NULL, ...) {
  # 获取模型的系数
  beta_hat <- object$coefficients
  
  # 如果没有提供 newdata
  if (is.null(newdata)) {
    # 使用模型数据生成设计矩阵
    X_new <- model.matrix(object$formula, object$data)
    
    # 去掉截距列（假设第一列是截距），然后进行标准化
    X_new_intercept <- X_new[, -1]
    X_new_norm <- scale(X_new_intercept,
                        center = sapply(colnames(X_new_intercept), function(col) mean(object$data[[col]])), 
                        scale = sapply(colnames(X_new_intercept), function(col) sd(object$data[[col]])))
    
    # 重新添加截距列
    X_new_norm <- cbind(Intercept = 1, X_new_norm)
    
  } else {
    # 如果提供了 newdata，直接使用 newdata 的自变量构建标准化矩阵
    
    # 如果没有截距列，则手动添加
    if (!"(Intercept)" %in% colnames(newdata)) {
      newdata <- data.frame("(Intercept)" = 1, newdata, check.names = FALSE)
    }
    
    # 提取自变量（忽略第一列截距列），并检查数值型列
    X_new_intercept <- newdata[, -1, drop = FALSE]
    numeric_cols <- sapply(X_new_intercept, is.numeric)
    X_new_numeric <- X_new_intercept[, numeric_cols, drop = FALSE]
    
    # 对数值型自变量进行标准化
    X_new_norm <- scale(X_new_numeric,
                        center = sapply(names(X_new_numeric), function(col) mean(object$data[[col]], na.rm = TRUE)), 
                        scale = sapply(names(X_new_numeric), function(col) sd(object$data[[col]], na.rm = TRUE)))
    
    # 将截距列与标准化后的数值自变量合并
    X_new_norm <- cbind(Intercept = 1, X_new_norm)
  }
  
  # 计算并返回拟合值
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
