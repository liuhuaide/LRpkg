#' LRpkg: A Package for Linear Regression Models
#'
#' This package provides functionalities to handle linear regression models
#' using QR decomposition. It implements an S3 class to manage special methods
#' such as print(), plot(), resid(), pred(), coef(), and summary(). The package
#' focuses on the implementation of linear regression using linear algebra techniques
#' and allows users to easily interpret regression results.
#'
#' Key features of the package include:
#' - Calculation of regression coefficients, fitted values, residuals, and their variances.
#' - Comprehensive summary statistics including t-values and p-values for each coefficient.
#' - Methods for visualizing regression results, such as residuals vs fitted values and scale-location plots.
#' - User-friendly interface to extract residuals and predicted values.
#' - Example usage provided in the package documentation and vignettes.
#'
#' A vignette is included to demonstrate how to conduct a simple regression analysis
#' using a dataset included in the package.
#'
#' @name LRpkg
#' @aliases LRpkg-package
#' @importFrom ggplot2 ggplot aes geom_point geom_hline labs theme_minimal geom_smooth
#' @importFrom gridExtra grid.arrange
#' @importFrom stats model.matrix pt coef
#'
#' @title Linear Regression Models
#' @description A package for linear regression analysis using QR decomposition.
#'
NULL
Fitted <- NULL
Residuals <- NULL
Sqrt_Residuals <- NULL
Sqrt_Std_Residuals <- NULL
