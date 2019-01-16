# do an objective function with seeds/
# function(x, y, fit = glm, predict = predict.glm, params, seed)
# with a function fit and a function predict (any) e.g bayesianrvfl::fit,
# bayesianrvfl::predict

# for xgboost, for random forest, for lm, for custom model (rvfl1 & 2)
# with only one set of parameters list(nrounds = 1000, max_depth = 3,
# eta = 0.01, subsample = 0.75)
# and do.call(what, args, quote = FALSE, envir = parent.frame())
# and metric ("rmse", )
# ModelMetrics::auc

crossval <- function(x, y,
                     fit_func = stats::glm.fit,
                     predict_func = stats::predict.glm,
                     fit_params = list(family = quasi()), # and hyperparameters
                     k = 5, repeats = 3, seed = 123,
                     eval_metric = NULL, cl = NULL,
                     errorhandling = c('stop', 'remove', 'pass'),
                     packages = c("stats", "Rcpp"), verbose = FALSE,
                     ...){

  x <- as.matrix(x)

  # evaluation metric
  if (is.null(eval_metric))
  {
    if (is.factor(y)) # classification
    {
      eval_metric <- function (preds, actual)
      {
        res <- mean(preds == actual)
        names(res) <- "accuracy"
        return(res)}

    } else { # regression

      eval_metric <- function (preds, actual)
      {
        res <- sqrt(mean((preds - actual)^2))
        names(res) <- "res"
        return(res)
      }

    }
  }

  # parallel exec.
  if(!is.null(cl) && cl > 0)
  {
    cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
    doSNOW::registerDoSNOW(cl_SOCK)
    `%op%` <-  foreach::`%dopar%`

    pb <- txtProgressBar(min = 0, max = nb_iter, style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    i <- NULL
    res <- foreach::foreach(i = 1:nb_iter, .packages = c("doSNOW", "Rcpp"),
                            .combine = rbind, .errorhandling = "remove",
                            .options.snow = opts, .verbose = FALSE,
                            .export = c("create_folds"))%op%{
      0
    }
    close(pb)
    snow::stopCluster(cl_SOCK)

  }  else { # sequential exec.

    set.seed(seed)
    list_folds <- lapply(1:repeats,
                         function (i) crossval::create_folds(y = y, k = k))

    `%op%` <-  foreach::`%do%`
    pb <- txtProgressBar(min = 0, max = k*repeats, style = 3)

    i <- j <- NULL
    res <- foreach::foreach(j = 1:repeats, .packages = packages,
                            .combine = rbind, .verbose = FALSE,
                            .errorhandling = errorhandling,
                            .export = c("fit_params"))%op%{

        temp <- foreach::foreach(i = 1:k, .combine = 'rbind', .errorhandling = "stop")%op%{

        train_index <- -list_folds[[j]][[i]]
        test_index <- -train_index

        # fit
        fit_func_train <- function(x, y, ...) fit_func(x = x[train_index, ],
                                                        y = y[train_index],
                                                        ...)

        fit_obj <- do.call(what = fit_func_train,
                           args = c(list(x = x, y = y), fit_params))

        # predict
        preds <- try(predict_func(fit_obj, newdata = x[-train_index, ]),
                     silent = TRUE)
        if (class(preds) == "try-error")
        {
          preds <- try(predict_func(fit_obj, newx = x[-train_index, ]),
                       silent = TRUE)
        }

        error_measure <- eval_metric(preds, y[test_index])

        setTxtProgressBar(pb, i*j)

        error_measure
      } # end loop i = 1:k
    } # end loop j = 1:repeats

    close(pb)
  }

  return(res)

}
compiler::cmpfun(crossval)

# library(xgboost)
#
# dtrain <- xgb.DMatrix(data = as.matrix(training_set[,-ncol(veteran_wod_wt)]),
#                       label = training_set$log_ratio)
#
# (cv <- xgb.cv(data = dtrain, nthread = 2, nfold = 5, metrics = "rmse",
#               nrounds = 1000, max_depth = 3, eta = 0.01, subsample = 0.75, objective = "reg:linear"))
#
# print(cv)
#
# OF <- function(xx) xgb.cv(data = dtrain, nthread = 2, # do repeats with seed
#                           nfold = 5, metrics = "rmse",
#                           nrounds = 1000, max_depth = 3,
#                           eta = 0.01, subsample = 0.75,
#                           objective = "reg:linear")
#
# library(caret)
# set.seed(123)
# trControl <- caret::trainControl(method = "repeatedcv", number = 5, repeats = 3,
#                                  p = 0.8)
# caret::train(x = as.matrix(training_set[,-ncol(veteran_wod_wt)]),
#              y = training_set$log_ratio, method = "xgbTree",
#              trControl = trControl,
#              tuneGrid = data.frame(nrounds = 1000, max_depth = 3,
#                                    eta = 0.01, gamma = 0.1, subsample = 0.75,
#                                    colsample_bytree = 1, min_child_weight = 1))
#
#
