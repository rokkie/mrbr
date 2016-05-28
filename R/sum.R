#' Calculate Sum
#'
#' Given a list of numeric values,
#' calculates the sum of the list.
#'
#' @param list A list of numeric values
#' @return The sum of the list
#' @export
mrbr.sum <- function(list) {
  sum <- 0

  for (i in list) {
    sum <- sum + i;
  }

  return(sum)
}
