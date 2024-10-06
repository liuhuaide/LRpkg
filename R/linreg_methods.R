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
#' @importFrom ggplot2 geom_line theme element_blank element_rect element_text
#' @importFrom gridExtra grid.arrange
#' @importFrom stats sd aggregate median
#' 
#' @export
plot.linreg <- function(x, ...) {
  
  df1 <- data.frame(
    Fitted = x$fitted_values,
    Residuals = x$residuals
  )
  med_df1 <- aggregate(Residuals ~ Fitted, data = df1, median)
  p1 <- ggplot(df1, aes(Fitted, Residuals)) +
    geom_point(shape = 1) +
    geom_line(data = med_df1, aes(Fitted, Residuals), color = "red") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
    theme_minimal() +
    theme(aspect.ratio = 4/5) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", linewidth = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  standardized_residuals <- (x$residuals) / sd(x$residuals)
  df2 <- data.frame(
    Fitted = x$fitted_values,
    Sqrt_Std_Residuals = sqrt(abs(standardized_residuals))
  )
  med_df2 <- aggregate(Sqrt_Std_Residuals ~ Fitted, data = df2, median)
  p2 <- ggplot(df2, aes(Fitted, Sqrt_Std_Residuals)) +
    geom_point(shape = 1) +
    geom_smooth(se = FALSE, color = "red", method = "lm") +
    labs(title = "Scale-Location", x = "Fitted Values", y = expression(sqrt(abs(Standardized~Residuals)))) +
    theme_minimal() +
    theme(aspect.ratio = 4/5) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", linewidth = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  
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
