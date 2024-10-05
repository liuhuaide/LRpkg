#' Print Method for linreg Objects
#'
#' Prints the coefficients of a linreg object.
#'
#' @param x An object of class "linreg".
#' @param ... Additional arguments (not used).
#'
#' @export
print.linreg <- function(x, ...) {
  cat("Coefficients:\n")
  print(coef(x))
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
#'
#' @export
plot.linreg <- function(x, ...) {

  df1 <- data.frame(
    Fitted = x$fitted_values,
    Residuals = x$residuals
  )
  p1 <- ggplot(df1, aes(x = Fitted, y = Residuals)) +
    geom_point(shape = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
    theme_minimal()

  df2 <- data.frame(
    Fitted = x$fitted_values,
    Sqrt_Residuals = sqrt(abs(x$residuals))
  )
  p2 <- ggplot(df2, aes(x = Fitted, y = Sqrt_Residuals)) +
    geom_point(shape = 1) +
    geom_smooth(se = F , color = "red") +
    labs(title = "Scale-Location", x = "Fitted Values", y = expression(sqrt(abs(Residuals)))) +
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
  coef_df <- data.frame(
    Estimate = as.vector(object$coefficients),
    `Std. Error` = sqrt(diag(object$beta_variance)),
    `t value` = object$t_values,
    `Pr(>|t|)` = object$p_values
  )
  print(coef_df)

  cat("\nResidual standard error:", sqrt(object$residual_variance),
      "on", object$df, "degrees of freedom\n")
}
