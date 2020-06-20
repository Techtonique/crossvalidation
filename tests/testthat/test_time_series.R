context("Test on time series models")


library(testthat)
library(crossval)
library(forecast)


# Example 1 -----

res1 <- crossval_ts(y=AirPassengers, initial_window = 10, fcast_func = thetaf)


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

xreg <- cbind(1, 1:length(AirPassengers))
res4 <- crossval_ts(y=AirPassengers, x=xreg, fcast_func = thetaf,
                    initial_window = 10, horizon = 3,
                    fixed_window = TRUE, type_forecast="quantiles")


mean1 <- colMeans(res1)
mean2 <- colMeans(res2)
mean3 <- colMeans(res3)
mean4 <- colMeans(res4)


# tests -----

test_that("tests on mean RMSE and MAPE", {
  expect_equal(as.numeric(round(mean1["RMSE"], 2)), 51.43 )
  expect_equal(as.numeric(round(mean1["MAPE"], 2)), 0.16 )
  expect_equal(as.numeric(round(mean2["RMSE"], 2)), 51.91)
  expect_equal(as.numeric(round(mean2["MAPE"], 2)), 0.15)
  expect_equal(as.numeric(round(mean3["RMSE"], 2)), 45.34)
  expect_equal(as.numeric(round(mean3["MAPE"], 2)), 0.14)
  expect_equal(as.numeric(round(mean4["RMSE"], 2)), 86.13)
  expect_equal(as.numeric(round(mean4["MAPE"], 2)), 0.24)
})

