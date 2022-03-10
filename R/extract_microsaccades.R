#' Extracts microsaccades from samples using one of the implemented algorithms.
#'
#' @param x Horizontal coordinate, ether a vector for monocular data or a two-column matrix for binocular data. 
#' @param y Vertical coordinate, ether a vector for monocular data or a two-column matrix for binocular data. 
#' @param sample_rate Sampling rate in Hz.
#' @param method A string with a method name. Currently implemented are Engbret & Kliegl (2003) (\code{"ek"}, default).
#' @param binocular Specifies how a binocular data is treated. Options are \code{"cyclopean"} (binocular data is
#' converted to an average cyclopean image before microsaccades are extracted), \code{"independent"} (microsaccades
#' are extracted independently for each eye), \code{"merge"} (default, microsaccades are extracted for each eye
#' independently but microsaccades from different eyes that temporally overlap are averaged into a binocular
#' microsaccade).
#' @param trial Optional vector with trial ID. If omitted, all samples are assumed to belong to a single trial.
#' @param options A named list with options for a specific method.
#'
#' @return
#' @export
#'
#' @examples
extract_microsaccades <- function(x, y, sample_rate, method = "ek", binocular = "merge", trial = NULL, options = list()){
  
}
