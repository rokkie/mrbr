## Import libraries
library(rmr2)

#' Calculate Average
#'
#' Given a list of numeric values,
#' returns the average of the list using Map/Reduce.
#'
#' @param input Path to the input folder on HDFS
#' @param output Path to the destination folder on HDFS
#' @return The average of the list
#' @export
mrbr.avg <- function (input, output = NULL) {
  sum <- mrbr.sum(input, output)
  len <- mrbr.len(input, output)
  avg <- sum / len

  return(avg)
}
