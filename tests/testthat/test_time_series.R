context("Test on time series models")


library(testthat)
library(crossvalidation)
library(forecast)


# Example 1 -----

res1 <- crossval_ts(y=AirPassengers, initial_window = 10, fcast_func = thetaf)

res6 <- crossval_ts(y=AirPassengers, initial_window = 10, fcast_func = thetaf,
                    p = 0.8)


# Example 2 -----

fcast_func <- function (y, h, ...)
{
      forecast::forecast(forecast::auto.arima(y, ...),
      h=h, ...)
}

res2 <- crossval_ts(y=AirPassengers, initial_window = 10, fcast_func = fcast_func)



# Example 3 -----

fcast_func <- function (y, h, ...)
{
      forecast::forecast(forecast::ets(y, ...),
      h=h, ...)
}

res3 <- crossval_ts(y=AirPassengers, initial_window = 10, fcast_func = fcast_func)


# Example 4 -----

 set.seed(123)
 x <- ts(matrix(rnorm(50), nrow = 25))

 fcast_func <- function(y, h = 5, type_forecast=c("mean", "median"))
 {
  type_forecast <- match.arg(type_forecast)

  if (type_forecast == "mean")
  {
   means <- colMeans(y)
   return(list(mean = t(replicate(n = h, expr = means))))
  } else {
   medians <- apply(y, 2, median)
   return(list(mean = t(replicate(n = h, expr = medians))))
  }

 }

res4 <- crossvalidation::crossval_ts(y = x, fcast_func = fcast_func,
                                     fit_params = list(type_forecast = "median"))

res5 <- crossvalidation::crossval_ts(y = x, fcast_func = fcast_func,
                                     fit_params = list(type_forecast = "mean"))

res7 <- crossvalidation::crossval_ts(y = x, fcast_func = fcast_func,
                                     fit_params = list(type_forecast = "mean"),
                                     p = 0.8)

# Example 5 -----

set.seed(123)
res8 <- eval_ts(y=rnorm(25), initial_window = 10,
                horizon = 5, fcast_func = forecast::thetaf,
                show_progress = FALSE, q=0.2)

# tests -----

mean1 <- colMeans(res1)
mean2 <- colMeans(res2)
mean3 <- colMeans(res3)
mean4 <- colMeans(res4)
mean5 <- colMeans(res5)
mean8 <- colMeans(res8)

test_that("tests on mean RMSE and MAPE", {
  expect_equal(as.numeric(round(mean1["RMSE"], 2)), 51.43)
  expect_equal(as.numeric(round(mean1["MAPE"], 2)), 0.16 )
  expect_equal(as.numeric(round(mean2["RMSE"], 2)), 51.91)
  expect_equal(as.numeric(round(mean2["MAPE"], 2)), 0.15)
  expect_equal(as.numeric(round(mean3["RMSE"], 2)), 45.34)
  expect_equal(as.numeric(round(mean3["MAPE"], 2)), 0.14)
  expect_equal(as.numeric(round(mean4["RMSE"], 2)), 0.97)
  expect_equal(as.numeric(round(mean5["MAPE"], 2)), 1.43)
  expect_equal(res1[103, ], res6[103, ])
  expect_equal(res5[13, ], res7[13, ])
  expect_equal(as.numeric(round(mean8["RMSE"], 2)), 1.11)
  expect_equal(as.numeric(round(mean8["MAPE"], 2)), 1.35)
})

