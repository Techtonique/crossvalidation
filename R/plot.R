#' Boxplots of cross-validation performances
#'
#' @param x a data frame containing models cross-validation performances
#' (using) \code{\link{crossvalidation::create_samples}}
#' @param ... additional parameters to be passed to \code{boxplot}
#'
#' @return
#' @export
#'
#' @examples
boxplot.cvsamples <- function(x, ...)
{
  x <- data.frame(x)
  jet_colors <- colorRampPalette(c("lightyellow","lightgreen"))
  nbcol <- ncol(x)
  color <- jet_colors(nbcol)
  graphics::boxplot(x, col = color, ...)
}
