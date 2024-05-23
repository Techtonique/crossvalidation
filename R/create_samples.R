#' Create a data structure of cross-validation results
#'
#' @param ... list of cross-validation results for multiple models
#' @param model_names model names
#'
#' @return a list of results to be used in \code{plot}
#' @export
#'
#' @examples
#'
#' \dontrun{
#' print("see vignettes")
#' }
#'
create_samples <- function(...,
                           model_names)
{
  stopifnot(!missing(model_names))
  input_list <- list(...)
  n_items <- length(input_list)
  samples <- vector("list", n_items)

  for (i in 1:n_items)
  {
    samples[[i]] <- as.numeric(input_list[[i]]$folds)
  }

  names(samples) <- model_names

  class(samples) <- c("cvsamples", class(samples))

  return(samples)
}
