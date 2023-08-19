#' Fit linear model
#'
#' @param x design matrix of dimension n * p.
#' @param y vector of observations of length n, or a matrix with n rows.
#' @param ... additional parameters to be passed to .lm.fit
#'
#' @return a list
#' @export
#'
#' @examples
#'
#' NULL
#'
fit_lm <- function(x, y, ...)
{
  stats::.lm.fit(x = x, y = y, ...)
}

#' Linear model prediction
#'
#' @param fit_obj object adjusted by \code{crossvalidation::fit_lm}
#' @param newx unseen data
#'
#' @return a vector or a matrix
#' @export
#'
#' @examples
#'
#' NULL
#'
predict_lm <- function(fit_obj, newx)
{
  drop(newx%*%fit_obj$coef)
}
