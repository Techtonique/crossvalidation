# inspired from caret::createFolds
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


# borrowed from caret::createTimeSlices
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
