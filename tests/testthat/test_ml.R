context("Test on ML models")


library(testthat)
library(crossvalidation)

# dataset

 set.seed(123)
 n <- 1000 ; p <- 10
 X <- matrix(rnorm(n * p), n, p)
 y <- rnorm(n)

# linear model example -----

res1 <- crossvalidation::crossval_ml(x = X, y = y, k = 5, repeats = 3)


# randomForest example -----

require(randomForest)

# fit randomForest with mtry = 2

res2 <- crossvalidation::crossval_ml(x = X, y = y, k = 5, repeats = 3,
                   fit_func = randomForest::randomForest, predict_func = predict,
                   packages = "randomForest", fit_params = list(mtry = 2))

# fit randomForest with mtry = 4

res3 <- crossvalidation::crossval_ml(x = X, y = y, k = 5, repeats = 3,
                   fit_func = randomForest::randomForest, predict_func = predict,
                   packages = "randomForest", fit_params = list(mtry = 4))

# fit randomForest with mtry = 4, with a validation set

res4 <- crossvalidation::crossval_ml(x = X, y = y, k = 5, repeats = 2, p = 0.8,
                   fit_func = randomForest::randomForest, predict_func = predict,
                   packages = "randomForest", fit_params = list(mtry = 4))


# tests -----

test_that("tests on mean and sd error", {
  expect_equal(round(res1$mean, 2), 1.01)
  expect_equal(round(res1$sd, 2), 0.03)
  expect_equal(round(res2$mean, 2), 1.02)
  expect_equal(round(res2$sd, 2), 0.03)
  expect_equal(round(res3$mean, 2), 1.02)
  expect_equal(round(res3$sd, 2),  0.03)
  expect_equal(round(res4$mean_training, 2), 1.03)
  expect_equal(round(res4$sd_training, 2), 0.04)
})

