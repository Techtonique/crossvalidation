#' Create Stratified Cross-Validation Folds
#'
#' Creates approximately balanced folds for cross-validation.
#' For numeric outcomes, the response is first discretized into
#' quantile-based groups to preserve the distribution across folds.
#' For categorical outcomes, stratified sampling is applied so that
#' each fold contains approximately the same class proportions.
#'
#' Inspired by \code{caret::createFolds()}.
#'
#' @param y A numeric or categorical response vector.
#' @param k Integer. The number of folds to create.
#'
#' @return A named list of integer vectors. Each element contains
#' the row indices for a fold.
#'
#' @details
#' If \code{y} is numeric, the values are grouped into quantile-based
#' intervals before stratification. The number of quantile groups is
#' automatically determined based on the sample size and number of folds.
#'
#' Fold names are returned in the format \code{"Fold01"},
#' \code{"Fold02"}, etc.
#'
#' @examples
#' # Classification example
#' set.seed(123)
#' y <- sample(c("A", "B"), size = 100, replace = TRUE)
#' folds <- create_folds(y, k = 5)
#'
#' # Regression example
#' set.seed(123)
#' y_num <- rnorm(100)
#' folds_num <- create_folds(y_num, k = 5)
#'
#' @seealso
#' \code{\link{create_time_slices}}
#'
#' @export
create_folds <- function(y, k = 10)
{
  if (is.numeric(y)) {
    cuts <- floor(length(y)/k)
    if (cuts < 2)
      cuts <- 2
    if (cuts > 5)
      cuts <- 5
    breaks <- unique(quantile(y, probs = seq(0, 1, length = cuts)))
    y <- cut(y, breaks, include.lowest = TRUE)
  }
  if (k < length(y)) {
    y <- factor(as.character(y))
    numInClass <- table(y)
    foldVector <- vector(mode = "integer", length(y))
    for (i in 1:length(numInClass)) {
      min_reps <- numInClass[i]%/%k
      if (min_reps > 0) {
        spares <- numInClass[i]%%k
        seqVector <- rep(1:k, min_reps)
        if (spares > 0)
          seqVector <- c(seqVector, sample(1:k, spares))
        foldVector[which(y == names(numInClass)[i])] <- sample(seqVector)
      }
      else {
        foldVector[which(y == names(numInClass)[i])] <- sample(1:k,
                                                               size = numInClass[i])
      }
    }
  }
  else foldVector <- seq(along = y)

  out <- split(seq(along = y), foldVector)
  names(out) <- paste("Fold", gsub(" ", "0", format(seq(along = out))),
                      sep = "")


  return(out)

}
create_folds <- compiler::cmpfun(create_folds)


#' Create Rolling Time Series Training and Test Slices
#'
#' Generates rolling training and testing index sets for time series
#' resampling and forecasting evaluation.
#'
#' Inspired by \code{caret::createTimeSlices()}.
#'
#' @param y A vector, matrix, or data frame representing the time series.
#' Only the number of observations is used.
#' @param initial_window Integer. The number of observations used
#' in the initial training window.
#' @param horizon Integer. The forecasting horizon size.
#' Defaults to \code{1}.
#' @param fixed_window Logical. If \code{TRUE}, all training windows
#' have fixed size equal to \code{initial_window}. If \code{FALSE},
#' the training window grows over time.
#' @param skip Integer. Number of resampling slices to skip between
#' consecutive windows. Defaults to \code{0}.
#'
#' @return A list with two components:
#' \describe{
#'   \item{train}{A named list of integer vectors containing
#'   training indices.}
#'   \item{test}{A named list of integer vectors containing
#'   testing indices.}
#' }
#'
#' @details
#' Training and testing slices are created sequentially in time order,
#' making this function suitable for forecasting and time series
#' cross-validation.
#'
#' Slice names are returned in the format
#' \code{"training001"} and \code{"testing001"}.
#'
#' @examples
#' y <- 1:20
#'
#' # Fixed rolling window
#' slices <- create_time_slices(
#'   y,
#'   initial_window = 10,
#'   horizon = 2
#' )
#'
#' # Expanding window
#' slices_expanding <- create_time_slices(
#'   y,
#'   initial_window = 10,
#'   horizon = 2,
#'   fixed_window = FALSE
#' )
#'
#' @seealso
#' \code{\link{create_folds}}
#'
#' @export
create_time_slices <- function(y, initial_window, horizon = 1,
                               fixed_window = TRUE, skip = 0)
{
  if(!is.null(ncol(y)))
  {
    n_y <- dim(y)[1]
  } else {
    n_y <- length(y)
  }

  stops <- seq(initial_window, (n_y - horizon), by = skip + 1)

  if (fixed_window) {
    starts <- stops - initial_window + 1
  }
  else {
    starts <- rep(1, length(stops))
  }
  train <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
  test <- mapply(seq, stops + 1, stops + horizon, SIMPLIFY = FALSE)
  nums <- gsub(" ", "0", format(stops))
  names(train) <- paste("training", nums, sep = "")
  names(test) <- paste("testing", nums, sep = "")
  out <- list(train = train, test = test)
  out
}
create_time_slices  <- compiler::cmpfun(create_time_slices)


#' Split a time series
#'
#' @param y univariate or multivariate time series
#' @param p proportion of data in training set
#' @param return_indices return indices instead of time series?
#'
#' @return
#' @export
#'
#' @examples
split_ts <- function(y, p = 0.8, return_indices = FALSE)
{
  n_y <- base::ifelse(test = is.null(dim(y)),
                      yes = length(y),
                      no = dim(y)[1])

  index_train <- 1:floor(p*n_y)
  if (return_indices)
    return(index_train)

  start_y <- stats::start(y)
  frequency_y <- stats::frequency(y)

  if(is.null(ncol(y))) # univariate case
  {
    training <- ts(y[index_train],
                   start = start_y,
                   frequency = frequency_y)
    start_testing <- tsp(training )[2] + 1 / frequency_y
    return(list(training = training,
                testing = ts(y[-index_train],
                             start = start_testing,
                             frequency = frequency_y)))
  } else { # multivariate case
    training <- ts(y[index_train, ],
                   start = start_y,
                   frequency = frequency_y)
    start_testing <- tsp(training)[2] + 1 / frequency_y
    return(list(training = training,
                testing = ts(y[-index_train, ],
                             start = start_testing,
                             frequency = frequency_y)))
  }
}
split_ts  <- compiler::cmpfun(split_ts)
