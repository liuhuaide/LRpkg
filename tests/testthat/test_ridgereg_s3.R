context("ridgereg")

data("iris")

test_that("ridgereg rejects erroneous input", {
  expect_error(ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = -1)) 
  expect_error(ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = "invalid")) 
})

test_that("class is correct", {
  ridge_mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1)
  
  expect_s3_class(ridge_mod, "ridgereg")
})

test_that("print() works", {
  ridge_mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1)
  
  expect_output(print(ridge_mod), "ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris, lambda = 1\\)")
  expect_output(print(ridge_mod), "Coefficients:\\n   Intercept  Sepal\\.Width Sepal\\.Length")
  
})


test_that("pred() works", {
  ridge_mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1)
  
  pred_values <- pred.ridgereg(ridge_mod)
  expect_equal(round(unname(pred_values[c(1, 5, 7)]), 2), c(1.86,1.55,1.11), tolerance = 0.01)
})



test_that("coef() works", {
  ridge_mod <- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris, lambda = 1)
  
  expect_equal(round(unname(as.vector(coef(ridge_mod))), 2), c(3.76, -0.58, 1.46), tolerance = 0.01)
})


