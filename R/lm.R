#' Title
#'
#' @param x
#' @param y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
fit_lm <- function(x, y, ...)
{
  stats::.lm.fit(x = x, y = y, ...)
}

#' Title
#'
#' @param fit_obj
#' @param newx
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
predict_lm <- function(fit_obj, newx, ...)
{
  drop(newx%*%fit_obj$coef)
}
