## Import libraries
library(rmr2)

#' Calculate Length
#'
#' Given a list of values,
#' calculates the length of the list using Map/Reduce.
#'
#' @param input Path to the input folder on HDFS
#' @param output Path to the destination folder on HDFS
#' @return The length of the list
#' @export
mrbr.len <- function (input, output = NULL) {
  len <- mapreduce(
    input        = input,
    input.format = "text",
    output       = output,
    map          = mrbr.len.map,
    reduce       = mrbr.len.reduce
  ) %>% from.dfs %>% values

  return(len)
}

#' Map function for len
#'
#' Takes Key and Value
#' Splits the value on whitespaces
#' Emits a Key/Value pair with '1' as key and the integers as value
#'
#' @param k Key
#' @param v Value
#' @return key-value object
mrbr.len.map <- function (k, v) {
  chars <- strsplit(v, '\\s')
  els   <- unlist(chars)
  kv    <- keyval(1, els)

  return(kv)
}

#' Reduce function for len
#'
#' Loops over values and adds 1 to the count on each iteration
#' Emits key/Value pair with key argument as key, and the count as value
#'
#' @param k Key
#' @param v Value
#' @return key-value object
mrbr.len.reduce <- function (k, v) {
  qty <- 0

  for (i in v) {
    qty <- qty + 1;
  }

  kv <- keyval(k, qty)

  return(kv)
}
