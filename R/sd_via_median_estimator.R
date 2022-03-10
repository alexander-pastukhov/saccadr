#' Compute standard deviation via median estimator.
#'
#' @description Compute standard deviation via median estimator.
#' Please refer to formula #2 in Engbert & Kliegl (2003).
#' Falls back on mean estimator, if computed standard deviation
#' is smaller than \code{.Machine$double.eps}. Raises an error
#' if the results using the mean estimator is still smaller than
#' \code{.Machine$double.eps}.
#'
#'
#' @param x Numeric values
#' @param na.rm Whether to exclude NA values, defaults to \code{FALSE}.
#'
#' @return float
#' @export
#'
#' @examples
#' sd_via_median_estimator(rnorm(100))
sd_via_median_estimator <- function(x, na.rm = FALSE){
  if (na.rm) x <- na.omit(x)
  
  estimated_sd <- sqrt(median(x^2) - median(x)^2)
  # estimated_sd <- sqrt(median((x - median(x))^2)) # This is a formula from R-file
  if (estimated_sd < .Machine$double.eps) {
    estimated_sd <- sqrt(mean(x^2) - mean(x)^2)
    
    if (estimated_sd < .Machine$double.eps) stop("Estimated standard deviation is smaller than .Machine$double.eps")
  }
  estimated_sd
}
