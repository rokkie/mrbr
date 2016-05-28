#' Calculate Standard Deviation
#'
#' Given a list of numeric values,
#' returns the standard deviation.
#'
#' @param list A list of numeric values
#' @return The standard deviation of the list
#' @export
mrbr.stddev <- function(list) {
  var   <- mrbr.var(list)
  stdev <- sqrt(var)

  return(stdev)
}
