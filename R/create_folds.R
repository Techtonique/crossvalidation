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
  stops <- seq(initial_window, (length(y) - horizon), by = skip +
                 1)
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
