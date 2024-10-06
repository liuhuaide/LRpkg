#' Print Method for linreg Objects
#'
#' Prints the coefficients of a linreg object.
#'
#' @param x An object of class "linreg".
#' @param ... Additional arguments (not used).
#'
#' @export
print.linreg <- function(x, ...) {
  formula_str <- paste0("linreg(formula = ", deparse(x$formula), ", data = ", x$data_name, ")")
  cat(formula_str, "\n\n")
  cat("Coefficients:\n")
  coef_vector <- as.vector(x$coefficients)
  names(coef_vector) <- rownames(x$coefficients)
  print(coef_vector)
}


#' Plot Method for linreg Objects
#'
#' Creates residuals vs fitted and scale-location plots for linreg objects.
#'
#' @param x An object of class "linreg".
#' @param ... Additional arguments (not used).
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_hline labs theme_minimal
#' @importFrom gridExtra grid.arrange
#' @importFrom stats sd
#' 
#' @export
plot.linreg <- function(x, ...) {
  # Residuals vs Fitted plot
  df1 <- data.frame(
    Fitted = x$fitted_values,
    Residuals = x$residuals
  )
  p1 <- ggplot(df1, aes(x = Fitted, y = Residuals)) +
    geom_point(shape = 1) +
    geom_smooth(se = FALSE, color = "red", method = "lm") + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
    theme_minimal()
  
  standardized_residuals <- (x$residuals) / sd(x$residuals)
  df2 <- data.frame(
    Fitted = x$fitted_values,
    Sqrt_Std_Residuals = sqrt(abs(standardized_residuals))  # Use sqrt of standardized residuals
  )
  p2 <- ggplot(df2, aes(x = Fitted, y = Sqrt_Std_Residuals)) +
    geom_point(shape = 1) +
    geom_smooth(se = FALSE, color = "red", method = "lm") +
    labs(title = "Scale-Location", x = "Fitted Values", y = expression(sqrt(abs(Standardized~Residuals)))) +
    theme_minimal()
  grid.arrange(p1, p2, nrow = 2)
}


#' Residuals Method for linreg Objects
#'
#' Returns the residuals of a linreg object.
#'
#' @param object An object of class "linreg".
#' @param ... Additional arguments (not used).
#'
#' @importFrom stats resid
#'
#' @return A numeric vector of residuals.
#' @export
resid.linreg <- function(object, ...) {
  return(object$residuals)
}



#' Prediction Method for linreg Class
#'
#' @param object An object of class "linreg".
#' @param ... Additional arguments (not used).
#' @return A numeric vector of fitted values.
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}
#' @export
pred.linreg <- function(object, ...) {
  return(object$fitted_values)
}



#' Coefficients Method for linreg Objects
#'
#' Returns the coefficients of a linreg object as a named vector.
#'
#' @param object An object of class "linreg".
#' @param ... Additional arguments (not used).
#'
#' @return A named numeric vector of coefficients.
#' @export
coef.linreg <- function(object, ...) {
  names(object$coefficients) <- colnames(object$fitted_values)
  return(object$coefficients)
}



#' Summary Method for linreg Objects
#'
#' Provides a summary of the linreg object including coefficients, standard errors,
#' t-values, and residual statistics.
#'
#' @param object An object of class "linreg".
#' @param ... Additional arguments (not used).
#'
#' @return A summary of the linear regression model.
#' @export
summary.linreg <- function(object, ...) {
  cat("Coefficients:\n")
  cat(" ", deparse(object$formula), "\n\n")

  Pr <- object$p_values
  n <- length(Pr)
  Pr_alpha <- character(n)
  for (i in 1:n) {
    if (Pr[i] < 0.001){
      Pr_alpha[i] <- "***"
    }else{
      if(Pr[i] < 0.01){
        Pr_alpha[i] <- "**"
      }else{
        if(Pr[i] < 0.05){
          Pr_alpha[i] <- "*"
        }
      }
    }
  }

  cat("Coefficients:\n")
  result <- data.frame(
    Estimate = object$coefficients,
    Std.Error = sqrt(diag(object$beta_variance)),
    t.value = object$t_values,
    Pr = Pr_alpha
  )
  rownames(result) <- rownames(object$coefficients)
  print(result)

  residual_std_error <- sqrt(object$residual_variance)
  df <- object$df
  cat("\nResidual standard error:", round(residual_std_error, 4), "on", df, "degrees of freedom\n")
  invisible(result)
}
