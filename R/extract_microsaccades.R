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
  # Converting x and y to matrices, so we can treat monocular and binocular cases similarly.
  if (is.vector(x)) x <- matrix(x)
  if (is.vector(y)) y <- matrix(y)
  
  # Checking that matrices dimensions match and are valid.
  if (all(dim(x) == dim(y))) stop("Dimensions for x and y do not match.")
  if (ncol(x) != 1 & ncol(x) != 2) stop("x and y must be a vector or a two-column matrix.")
  
  # Checking trial information
  if (!is.null(trial)) {
    # All samples belong to the same trial.
    itrial <- rep(1, nrow(x))
  } else {
    if (length(trial) != nrow(x)) stop("Dimensions for x/y and trial do not match.")
    # converting to an integer via factor for consistency
    itrial <- as.integer(as.factor(factor))
  }
  
  # Binocular data, checks and optional preprocessing.
  if ((ncol(x) == 2)) {
    # Checking validity of a binocular option, if data is binocular
    if (!(binocular %in% c("cyclopean", "independent", "merge"))) stop ('Unknown binocular option. Must be "cyclopean", "independent", or "merge".')
    
    # Special case, cyclopean data via averaging
    if (binocular == "cyclopean") {
      x <- matrix(rowMeans(x))
      y <- matrix(rowMeans(y))
    }
  }
  
}
