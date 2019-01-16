# say what to do for functions whose training and response names aren't x, y
#' Title
#'
#' @param x
#' @param y
#' @param fit_func
#' @param predict_func
#' @param fit_params
#' @param k
#' @param repeats
#' @param seed
#' @param eval_metric
#' @param cl
#' @param errorhandling
#' @param packages
#' @param verbose
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' crossval::crossval(x = X, y = y, k = 5, repeats = 3,
#' fit_func = glmnet::glmnet, predict_func = predict.glmnet,
#' packages = "glmnet", fit_params = list(alpha = 1, lambda = 0.1))
#'
#' crossval::crossval(x = X, y = y, k = 5, repeats = 3,
#' fit_func = glmnet::glmnet, predict_func = predict.glmnet,
#' packages = "glmnet", fit_params = list(alpha = 0, lambda = 0.01))
#'
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
      # think about which loop to do in parallel
      # + case when repeats == 1
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
                            .combine = cbind, .verbose = FALSE,
                            .errorhandling = errorhandling,
                            .export = c("fit_params"))%op%{

        temp <- foreach::foreach(i = 1:k, .combine = 'rbind',
                                 .errorhandling = errorhandling)%op%{

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

  colnames(res) <- paste0("repeat", 1:repeats)
  rownames(res) <- paste0("fold", 1:k)
  return(res)

}
compiler::cmpfun(crossval)
