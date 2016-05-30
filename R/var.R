## Import libraries
library(rmr2)

#' Calculate Variance
#'
#' Given a list of numeric values,
#' returns the variance of the list using Map/Reduce.
#'
#' @param input Path to the input folder on HDFS
#' @param output Path to the destination folder on HDFS
#' @return The variance of the list
#' @export
mrbr.var <- function(input, output = NULL) {
  avg <- mrbr.avg(input, output)
  len <- mrbr.len(input, output)
  sum <- mapreduce(
    input        = input,
    input.format = "text",
    output       = output,
    map          = mrbr.var.map(avg),
    reduce       = mrbr.var.reduce
  ) %>% from.dfs %>% values
  var <- (sum / (len - 1))

  return(var)
}

#' Map function factory for variance
#'
#' Based on a given average,
#' returns a function that takes a Key and Value.
#' Splits the value on whitespaces and parses the pieces as integers
#' Subtracts the average from the number and squares it.
#' Emits a Key/Value pair with '1' as key and the squared difference as value
#'
#' @param avg Average
#' @return function
mrbr.var.map <- function (avg) {
  fn <- function (k, v) {
    chars <- strsplit(v, '\\s')
    nrs   <- unlist(chars)
    ints  <- strtoi(nrs)
    disq  <- ((ints - avg) ^ 2)
    kv    <- keyval(1, disq)
  }

  return (fn)
}

#' Reduce function for variance
#'
#' Loops over values and adds them together
#' Emits key/Value pair with key argument as key, and the sum as value
#'
#' @param k Key
#' @param v Value
#' @return key-value object
mrbr.var.reduce <- function (k, v) {
  sum <- 0

  for (i in v) {
    sum <- sum + i;
  }

  kv <- keyval(k, sum)

  return(kv)
}
