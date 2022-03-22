#' Extracts microsaccades from samples using one of the implemented algorithms.
#'
#' @param x Horizontal coordinate, ether a vector for monocular data or a two-column matrix for binocular data. 
#' @param y Vertical coordinate, ether a vector for monocular data or a two-column matrix for binocular data. 
#' @param sample_rate Sampling rate in Hz.
#' @param method A string with a method name. Currently implemented are Engbret & Kliegl (2003) (\code{"ek"}, default).
#' @param binocular Specifies how a binocular data is treated. Options are \code{"cyclopean"} (binocular data is
#' converted to an average cyclopean image before microsaccades are extracted), \code{"monocular"} (microsaccades
#' are extracted independently for each eye), \code{"merge"} (default, microsaccades are extracted for each eye
#' independently but microsaccades from different eyes that temporally overlap are averaged into a binocular
#' microsaccade).
#' @param trial Optional vector with trial ID. If omitted, all samples are assumed to belong to a single trial.
#' @param options A named list with options for a specific method.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' data(single_trial)
#' 
#' # extract microsaccades from a single trial data using the default method
#' ms <- extract_microsaccades(single_trial$x, single_trial$y, 500)
extract_microsaccades <- function(x,
                                  y,
                                  sample_rate,
                                  velocity_time_window = 20,
                                  method = "ek",
                                  binocular = "merge",
                                  trial = NULL,
                                  options = list()){
  # Converting x and y to matrices, so we can treat monocular and binocular cases similarly.
  x <- input_to_matrix(x)
  y <- input_to_matrix(y)

  # Checking that matrices dimensions match and are valid.
  if (any(dim(x) != dim(y))) stop("Dimensions for x and y do not match.")
  if (ncol(x) != 1 & ncol(x) != 2) stop("x and y must be vectord or two-column matrices.")
  
  # Checking method.
  supported_methods <- list("ek" = saccadr::extract_ms_ek)
  if (!(method %in% names(supported_methods))) stop("Unknown method.")
  
  # Checking trial information
  if (is.null(trial)) {
    # All samples belong to the same trial.
    itrial <- rep(1, nrow(x))
  } else {
    if (length(trial) != nrow(x)) stop("Dimensions for x/y and trial do not match.")
    # converting to an integer via factor for consistency
    itrial <- as.integer(as.factor(factor))
  }
  
  # Binocular data, checks and optional preprocessing.
  if ((ncol(x) == 2)) {
    # Checking validity of a binocular option.
    if (!(binocular %in% c("cyclopean", "independent", "merge"))) stop ('Unknown binocular option. Must be "cyclopean", "monocular", or "merge".')
    
    # Special case, cyclopean data via averaging.
    if (binocular == "cyclopean") {
      x <- as.matrix(rowMeans(x))
      y <- as.matrix(rowMeans(y))
    }
  }
  
  # Computing saccades for one (monocular or cyclopean) eye at a time
  potential_saccades <- matrix(0, ncol = ncol(x), nrow = nrow(x))
  
  for(iEye in 1:ncol(x)) {
    # compute velocity
    vel_df <- data.frame(
      velx = compute_velocity(x[, iEye], trial, sample_rate, velocity_time_window),
      vely = compute_velocity(y[, iEye], trial, sample_rate, velocity_time_window)
    )
    vel_df$vel <- sqrt(vel_df[['velx']]^2 + vel_df[['vely']]^2)
    
    # compute acceleration (for methods that require it)
    acc_df <- data.frame(
      accx = compute_velocity(vel_df[['velx']], trial, sample_rate, velocity_time_window),
      accy = compute_velocity(vel_df[['vely']], trial, sample_rate, velocity_time_window)
    )
    acc_df$acc <- sqrt(acc_df[['accx']]^2 + acc_df[['accy']]^2)
    
    
    # turning options into parameters passed via do.call
    call_arguments <- c(list(x = x[, iEye], y = y[, iEye], vel=vel, acc=acc, sample_rate = sample_rate, trial = trial), options)
    
    # add votes for potential saccades
    potential_saccades[, iEye] <- potential_saccades[, iEye] + do.call(supported_methods[[method]], call_arguments)
    
    # eye label  
    if (ncol(x) == 1) {
      eye_saccades$Eye <- "Cyclopean"
    } else {
      eye_saccades$Eye <- c("Left", "Right")[iEye]
    }
    
    # adding to the overall table
    saccades <- rbind(saccades, eye_saccades)
  }
  
  # Post-processing
  if (ncol(x) == 2 & binocular == "merge") {
    # TODO: merger
  }
  
  saccades
}
