# crossval

Generic R functions for cross-validation 


### Installation

- __1st method__: from Github

```bash
devtools::install_github(crossval)
```

- __2nd method__: from CRAN

```bash
TODO
```


### Demo

```R
# dataset
 set.seed(123)
 n <- 100 ; p <- 5
 X <- matrix(rnorm(n * p), n, p)
 y <- rnorm(n)
```

Linear model 

```R

# 'X' contains the explanatory variables
# 'y' is the response
# 'k' is the number of folds in k-fold cross-validation
# 'repeats' is the number of repeats of the k-fold cross-validation procedure

# linear model example -----

crossval::crossval(x = X, y = y, k = 5, repeats = 3)

# linear model example, with validation set

crossval::crossval(x = X, y = y, k = 5, repeats = 3, p = 0.8)
```

glmnet

```R
# glmnet example -----

# fit glmnet, with alpha = 1, lambda = 0.1

require(glmnet)
require(Matrix)

 crossval::crossval(x = X, y = y, k = 5, repeats = 3,
 fit_func = glmnet::glmnet, predict_func = predict.glmnet,
 packages = c("glmnet", "Matrix"), fit_params = list(alpha = 0.5, lambda = 0.1))

# fit glmnet, with alpha = 0, lambda = 0.01

 crossval::crossval(x = X, y = y, k = 5, repeats = 3,
 fit_func = glmnet::glmnet, predict_func = predict.glmnet,
 packages = c("glmnet", "Matrix"), fit_params = list(alpha = 0, lambda = 0.01))

 # fit glmnet, with alpha = 0, lambda = 0.01, with validation set

 crossval::crossval(x = X, y = y, k = 5, repeats = 2, p = 0.8,
 fit_func = glmnet::glmnet, predict_func = predict.glmnet,
 packages = c("glmnet", "Matrix"), fit_params = list(alpha = 0, lambda = 0.01))
```

Random Forest

```R
# randomForest example -----

require(randomForest)

# fit randomForest with mtry = 2

crossval::crossval(x = X, y = y, k = 5, repeats = 3,
fit_func = randomForest::randomForest, predict_func = predict,
packages = "randomForest", fit_params = list(mtry = 2))

# fit randomForest with mtry = 4

crossval::crossval(x = X, y = y, k = 5, repeats = 3,
fit_func = randomForest::randomForest, predict_func = predict,
packages = "randomForest", fit_params = list(mtry = 4))

# fit randomForest with mtry = 4, with validation set

crossval::crossval(x = X, y = y, k = 5, repeats = 2, p = 0.8,
fit_func = randomForest::randomForest, predict_func = predict,
packages = "randomForest", fit_params = list(mtry = 4))
```

xgboost

```R
# xgboost example -----

require(xgboost)

# The response and covariates are named 'label' and 'data'
# So, we do this:

f_xgboost <- function(x, y, ...) xgboost::xgboost(data = x, label = y, ...)

# fit xgboost with nrounds = 5

crossval::crossval(x = X, y = y, k = 5, repeats = 3,
  fit_func = f_xgboost, predict_func = predict,
   packages = "xgboost", fit_params = list(nrounds = 5,
   verbose = FALSE))

# fit xgboost with nrounds = 10

crossval::crossval(x = X, y = y, k = 5, repeats = 3,
  fit_func = f_xgboost, predict_func = predict,
   packages = "xgboost", fit_params = list(nrounds = 10,
   verbose = FALSE))

# fit xgboost with nrounds = 10, with validation set

crossval::crossval(x = X, y = y, k = 5, repeats = 2, p = 0.8,
  fit_func = f_xgboost, predict_func = predict,
   packages = "xgboost", fit_params = list(nrounds = 10,
   verbose = FALSE))
```

### Contributing

Your contributions are welcome, and valuable. Please, make sure to __read__ the [Code of Conduct](CONTRIBUTING.md) first.


### License

[BSD 3-Clause](LICENSE) © Thierry Moudiki, 2019. 

