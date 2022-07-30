#' Converts inputs to matrix
#' 
#' Converts inputs (vector, matrix, data.frame/tibble) to 
#' a matrix preserving number of columns
#'
#' @param x vector, matrix, data.frame/tibble 
#'
#' @return matrix
#' @export 
#' @keywords internal
#'
#' @examples
#' input_to_matrix(1:5)
input_to_matrix <- function(x){
  if (is.matrix(x)) return(x)
  if (is.vector(x)) return(as.matrix(x))
  if (is.data.frame(x)) return(as.matrix(x))
  stop("Unknown input format")
}
