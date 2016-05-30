## Import libraries
library(rmr2)
library(magrittr)

#' Calculate Sum
#'
#' Given a list of numeric values,
#' calculates the sum of the list using Map/Reduce.
#'
#' @param input Path to the input folder on HDFS
#' @param output Path to the destination folder on HDFS
#' @return The sum of the list
#' @export
mrbr.sum <- function(input, output = NULL) {
  sum <- mapreduce(
    input        = input,
    input.format = "text",
    output       = output,
    map          = mrbr.sum.map,
    reduce       = mrbr.sum.reduce
  ) %>% from.dfs %>% values

  return(sum)
}

#' Map function for sum
#'
#' Takes Key and Value
#' Splits the value on whitespaces and parses the pieces as integers
#' Emits a Key/Value pair with '1' as key and the integers as value
#'
#' @param k Key
#' @param v Value
#' @return key-value object
mrbr.sum.map <- function (k, v) {
  chars <- strsplit(v, '\\s')
  nrs   <- unlist(chars)
  ints  <- strtoi(nrs)
  kv    <- keyval(1, ints)

  return(kv)
}

#' Reduce function for sum
#'
#' Loops over values and adds them together
#' Emits key/Value pair with key argument as key, and the sum as value
#'
#' @param k Key
#' @param v Value
#' @return key-value object
mrbr.sum.reduce <- function (k, v) {
  sum <- 0

  for (i in v) {
    sum <- sum + i;
  }

  kv <- keyval(k, sum)

  return(kv)
}
