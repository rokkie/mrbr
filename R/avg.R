#' Calculate Average
#'
#' Given a list of numeric values,
#' returns the average of the list.
#'
#' @param list A list of numeric values
#' @return The average of the list
#' @export
mrbr.avg <- function (list) {
  sum <- mrbr.sum(list)
  avg <- sum / length(list)

  return(avg)
}
