#' Calculate Standard Deviation
#'
#' Given a list of numeric values,
#' returns the standard deviation using Map/Reduce.
#'
#' @param input Path to the input folder on HDFS
#' @param output Path to the destination folder on HDFS
#' @return The standard deviation of the list
#' @export
mrbr.stddev <- function(input, output = NULL) {
  var   <- mrbr.var(input, output)
  stdev <- sqrt(var)

  return(stdev)
}
