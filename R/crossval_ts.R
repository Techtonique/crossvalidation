#' Generic cross-validation function for time series
#'
#' Generic cross-validation for univariate and multivariate time series
#'
#' @param y response time series; a vector or a matrix
#' @param x input covariates' matrix (optional) for ML models
#' @param fit_func a function for fitting the model (if validation of ML model)
#' @param predict_func a function for predicting values from the model (if validation of ML model)
#' @param fcast_func time series forecasting function (e.g forecast::thetaf)
#' @param fit_params a list; additional (model-specific) parameters to be passed
#' to \code{fit_func}
#' @param p a float; percentage of original data in the training/testing procedure, default is 1 and
#' must be > 0.5.
#' @param initial_window an integer; the initial number of consecutive values in each training set sample
#' @param horizon an integer; the number of consecutive values in test set sample
#' @param fixed_window a boolean; if FALSE, all training samples start at 1
#' @param level a numeric vector; confidence levels for prediction intervals.
#' @param seed random seed for reproducibility of results
#' @param eval_metric a function measuring the test errors; if not provided: RMSE for regression and
#' accuracy for classification
#' @param cl an integer; the number of clusters for parallel execution
#' @param errorhandling specifies how a task evalution error should be handled.
#' If value is "stop", then execution will be stopped if an error occurs. If value
#' is "remove", the result for that task will not be returned. If value is "pass",
#' then the error object generated by task evaluation will be included with the
#' rest of the results. The default value is "stop".
#' @param packages character vector of packages that the tasks depend on
#' @param verbose logical flag enabling verbose messages. This can be very useful for
#' troubleshooting.
#' @param show_progress show evolution of the algorithm
#' @param ... additional parameters
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' require(forecast)
#' data("AirPassengers")
#'
#' # Example 1 -----
#'
#' res <- crossval_ts(y=AirPassengers, initial_window = 10,
#' horizon = 3, fcast_func = forecast::thetaf)
#' print(colMeans(res))
#'
#'
#' # Example 2 -----
#'
#' \dontrun{
#' fcast_func <- function (y, h, ...)
#' {
#'       forecast::forecast(forecast::auto.arima(y, ...),
#'       h=h, ...)
#' }
#'
#' res <- crossval_ts(y=AirPassengers, initial_window = 10, horizon = 3,
#' fcast_func = fcast_func)
#' print(colMeans(res))
#' }
#'
#'
#' # Example 3 -----
#'
#' fcast_func <- function (y, h, ...)
#' {
#'       forecast::forecast(forecast::ets(y, ...),
#'       h=h, ...)
#' }
#'
#' res <- crossval_ts(y=AirPassengers,
#' initial_window = 10, horizon = 3, fcast_func = fcast_func)
#' print(colMeans(res))
#'
#'
#' # Example 4 -----
#'
#' xreg <- cbind(1, 1:length(AirPassengers))
#' res <- crossval_ts(y=AirPassengers, x=xreg, fit_func = crossvalidation::fit_lm,
#' predict_func = crossvalidation::predict_lm,
#' initial_window = 10,
#' horizon = 3,
#' fixed_window = TRUE)
#' print(colMeans(res))
#'
#'
#' # Example 5 -----
#'
#' res <- crossval_ts(y=AirPassengers, fcast_func = forecast::thetaf,
#' initial_window = 10,
#' horizon = 3,
#' fixed_window = TRUE)
#' print(colMeans(res))
#'
#'
#'#' # Example 6 -----
#'
#' xreg <- cbind(1, 1:length(AirPassengers))
#' res <- crossval_ts(y=AirPassengers, x=xreg, fit_func = crossvalidation::fit_lm,
#' predict_func = crossvalidation::predict_lm,
#' initial_window = 10,
#' horizon = 3,
#' fixed_window = TRUE)
#' print(colMeans(res))
#'
#'
#' # Example 7 -----
#'
#' x <- ts(matrix(rnorm(50), nrow = 25))
#'
#' fcast_func <- function(y, h = 5, type_forecast=c("mean", "median"))
#' {
#'  type_forecast <- match.arg(type_forecast)
#'
#'  if (type_forecast == "mean")
#'  {
#'   means <- colMeans(y)
#'   return(list(mean = t(replicate(n = h, expr = means))))
#'  } else {
#'   medians <- apply(y, 2, median)
#'   return(list(mean = t(replicate(n = h, expr = medians))))
#'  }
#'
#' }
#'
#' print(fcast_func(x))
#'
#' res <- crossval_ts(y = x, fcast_func = fcast_func, fit_params = list(type_forecast = "median"))
#' colMeans(res)
#'
#' res <- crossval_ts(y = x, fcast_func = fcast_func, fit_params = list(type_forecast = "mean"))
#' colMeans(res)
#'
#' # Example 8 -----
#'
#' eval_metric <- function(predicted, observed)
#' {
#'   error <- observed - predicted
#'
#'   res <- apply(error, 2, function(x) sqrt(mean(x ^ 2, na.rm = FALSE)))
#'
#'   return(res)
#' }
#'
#' res <- crossval_ts(y = x, fcast_func = fcast_func, fit_params = list(type_forecast = "mean"),
#' eval_metric = eval_metric)
#'
#' colMeans(res)
#'
crossval_ts <- function(y,
                        x = NULL,
                        fit_func = crossvalidation::fit_lm,
                        predict_func = crossvalidation::predict_lm,
                        fcast_func = NULL,
                        fit_params = NULL,
                        p = 1,
                        # parameters of funcs
                        initial_window = 5,
                        horizon = 3,
                        fixed_window = TRUE,
                        level = c(80, 95),
                        seed = 123,
                        eval_metric = NULL,
                        cl = NULL,
                        errorhandling = c('stop', 'remove', 'pass'),
                        packages = c("stats", "Rcpp"),
                        verbose = FALSE,
                        show_progress = TRUE,
                        ...) {

  stopifnot(p <= 1 && p >= 0.5)

  if (p < 1) # cross validation on 1:floor(p*length(y)) indices
  {
    if(!is.null(ncol(y))) # multivariate input y
    {
      if (is.ts(y))
      {
        y <- ts(y[1:floor(p*nrow(y)), ],
                start = start(y),
                frequency = frequency(y))
      } else {
        y <- ts(y[1:floor(p*nrow(y)), ])
      }
    } else {# univariate input y
      if (is.ts(y))
      {
        y <- ts(y[1:floor(p*length(y))],
                start = start(y),
                frequency = frequency(y))
      } else {
        y <- ts(y[1:floor(p*length(y))])
      }
    }
  }

  if(!is.null(ncol(y))) # multivariate time series input
  {
    n_y <- dim(y)[1]

    time_slices <-
      crossvalidation::create_time_slices(
        y[, 1],
        initial_window = initial_window,
        horizon = horizon,
        fixed_window = fixed_window
      )
  } else { # univariate time series input

    n_y <- length(y)

    time_slices <-
      crossvalidation::create_time_slices(
        y,
        initial_window = initial_window,
        horizon = horizon,
        fixed_window = fixed_window
      )
  }


  n_slices <- length(time_slices$train)

  if (!is.null(x)) # regression, ML model
  {
    if (p < 1)
    {
      x <- x[1:floor(p*nrow(x)), ]
    }
    n_x <- dim(x)[1]
    p_x <- dim(x)[2]
    stopifnot(n_x == n_y)
  }

  # performance metrics
  if (is.null(eval_metric))
  {
    eval_metric <- function(predicted, observed)
    {
      error <- observed - predicted
      pe <- predicted / observed - 1

      res <- c(
        mean(error, na.rm = FALSE),
        sqrt(mean(error ^ 2, na.rm = FALSE)),
        mean(abs(error), na.rm = FALSE),
        mean(pe, na.rm = FALSE),
        mean(abs(pe), na.rm = FALSE)
      )

      names(res) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")

      return(res)
    }
    eval_metric <- compiler::cmpfun(eval_metric)
  }

  # progress bars
  if (!is.null(cl)) {
    cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
    doSNOW::registerDoSNOW(cl_SOCK)
    `%op%` <- foreach::`%dopar%`
  } else {
    `%op%` <- foreach::`%do%`
  }

  if (show_progress)
  {
    pb <- txtProgressBar(min = 0,
                         max = n_slices,
                         style = 3)
    progress <- function(n) {utils::setTxtProgressBar(pb, n)}
    opts <- list(progress = progress)
  } else {
    opts <- NULL
  }


  if (!is.null(fcast_func)) { # if fcast_func is not NULL, ts models are used

    # 1 - interface for forecasting functions --------------------------------------------------

    i <- NULL
    res <- foreach::foreach(
      i = 1:n_slices,
      .packages = packages,
      .combine = rbind,
      .errorhandling = errorhandling,
      .options.snow = opts,
      .verbose = verbose
    ) %op% {

      train_index <- time_slices$train[[i]]
      test_index <- time_slices$test[[i]]

      # cat("train_index", "\n")
      # print(train_index)
      # cat("\n")
      # cat("test_index", "\n")
      # print(test_index)
      # cat("\n")

      # 1 - 1 interface for forecasting functions: univariate --------------------------------------------------

      if (is.null(ncol(y))) # univariate time series case
      {
       preds <- try(do.call(what = fcast_func,
                               args = c(list(y = y[train_index],
                                          h = horizon), fit_params))$mean, silent = FALSE)

        if (class(preds)[1] == "try-error")
        {
          preds <- rep(NA, horizon)
        }

        # measure the error
        error_measure <-
          eval_metric(preds, y[test_index]) # univariate

      } else { #multivariate time series case

        # 1 - 2 interface for forecasting functions: multivariate --------------------------------------------------

        preds <- try(do.call(
                          what = fcast_func,
                          args = c(list(y = y[train_index, ],
                                        h = horizon), fit_params)
                        )$mean, silent = FALSE)

        if (class(preds)[1] == "try-error" | is.null(preds))
        {
          preds <- rep(NA, horizon)
        }

        # measure the error
        error_measure <-
          eval_metric(preds, y[test_index, ])

      }

      if (show_progress)
      {
        setTxtProgressBar(pb, i)
      }

      error_measure

    }

    if (show_progress)
    {
      close(pb)
    }

    if (!is.null(cl))
    {
      snow::stopCluster(cl_SOCK)
    }

  } else { # if fcast_func is NULL, ML models are used

    stopifnot(!is.null(fit_func))
    stopifnot(!is.null(predict_func))

    # 2 - interface for ml functions --------------------------------------------------

    i <- NULL
    res <- foreach::foreach(
      i = 1:n_slices,
      .packages = packages,
      .combine = rbind,
      .errorhandling = errorhandling,
      .options.snow = opts,
      .verbose = verbose
    ) %op% {
      # predict
      train_index <-
        time_slices$train[[i]]
      test_index <- time_slices$test[[i]]


      if (is.null(ncol(y)))
      {
        # 2 - 1 interface for ml function: univariate --------------------------------------------------
        fit_obj <-
          do.call(what = fit_func,
                  args = c(list(x = x[train_index,],
                                y = y[train_index]),
                           fit_params))

        # predict
        preds <-
          try(predict_func(fit_obj, newdata = x[test_index,]),
              silent = TRUE)

        if (class(preds)[1] == "try-error")
        {
          preds <- try(predict_func(fit_obj, newx = x[test_index,]),
                       silent = FALSE)
          if (class(preds)[1] == "try-error")
          {
            preds <- rep(NA, length(test_index))
          }
        }

        # measure the error
        error_measure <-
          eval_metric(preds, y[test_index])

      } else {

        # 2 - 2 interface for ml function: multivariate (ko so far) --------------------------------------------------
        stop("Not implemented")

        }

      if (show_progress)
      {
        setTxtProgressBar(pb, i)
      }

      error_measure
    }

    if (show_progress)
    {
      close(pb)
    }

    if (!is.null(cl))
    {
      snow::stopCluster(cl_SOCK)
    }
  }



  return(res)

}
compiler::cmpfun(crossval_ts)
