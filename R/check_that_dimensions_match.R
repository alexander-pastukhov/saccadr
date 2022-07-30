#' Checks that all vectors in the list have the same length.
#'
#' @description Checks that all vectors in the list have the same length.
#' Raises an error, if that is not the case.
#'
#' @param list_of_vectors
#'
#' @return NULL
#' @export
#' @keywords internal
#'
#' @examples
#' check_that_lengths_match(list(x = c(1, 2, 3), y = c(4, 5, 6)))
check_that_lengths_match <- function(list_of_vectors){
  for(iother in 2:length(list_of_vectors)){
    if (length(list_of_vectors[[1]]) != length(list_of_vectors[[2]])) stop("Dimensions of input vectors mismatch.")
  }
}
