#' Calculate Variance
#'
#' Given a list of numeric values,
#' returns the variance of the list.
#'
#' @param list A list of numeric value
#' @return The variance of the list
#' @export
mrbr.var <- function(list) {
  avg <- mrbr.avg(list)
  sum <- mrbr.sum((list - avg) ^ 2)
  var <- sum / (length(list) - 1)

  return(var)
}
