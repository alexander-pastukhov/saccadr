#' Computes ensemble vote for each sample using selected methods.
#' 
#' This function is called by \code{\link{extract_saccades}} but can be used directly,
#' if you are interested in the vote-per-sample.
#' @param x Horizontal coordinate, either a vector for monocular data or a two-column matrix for binocular data.
#' @param y Vertical coordinate, either a vector for monocular data or a two-column matrix for binocular data.
#' @param sample_rate Sampling rate in Hz.
#' @param trial Optional vector with trial ID. If omitted, all samples are assumed to belong to a single trial.
#' @param methods A \emph{list} (not a vector!) with names of package methods (character) or external functions that
#' implement sample classification (see vignette on using custom methods). Package methods include
#' Engbret & Kliegl (2003) (\code{"ek"}), Otero-Millan et al. (\code{"om"}), Nyström and Holmqvist (2010) (\code{"nh"}).
#' @param options A named list with options, see documentation on specific method for details.
#' @param velocity_time_window Time window for computing velocity and acceleration in milliseconds.
#' @param binocular Specifies how a binocular data is treated. Options are \code{"cyclopean"} (binocular data is
#' converted to an average cyclopean image before saccades are extracted), \code{"monocular"} (saccades
#' are extracted independently for each eye), \code{"merge"} (default, saccades are extracted for each eye
#' independently but saccades from different eyes that temporally overlap are averaged into a binocular
#' saccade). Note that \code{binocular = "merge"} is overridden by \code{normalize = FALSE},
#' votes are left as they are per method and eye. 
#' @param normalize Logical, whether to aggregate and normalize votes to 0..1 range. Note that
#' \code{normalize = FALSE} overrides \code{binocular = "merge"}, votes are left as they are per method and eye.
#'
#' @return list Either a list of matrices (one per eye) with votes for each method (\code{normalized = FALSE}) or
#' a matrix with normalized (0..1) vote on each sample either for each eye or for single cyclopean or merged
#' binocular time series (\code{normalized = TRUE}).
#' @seealso \code{\link{extract_saccades}}, \code{\link{method_ek}}, \code{\link{method_om}}, \code{\link{method_nh}}
#' @examples 
#' data(single_trial)
#' vote <- vote_on_samples(x = single_trial[['x']],
#'                         y = single_trial[['y']],
#'                         methods = list("ek", "om", "nh"),
#'                         sample_rate = 500,
#'                         normalize = FALSE)
vote_on_samples <- function(x,
                            y,
                            sample_rate,
                            trial = NULL,
                            methods = list("ek", "om", "nh"),
                            options = NULL,
                            velocity_time_window = 20,
                            binocular = "merge",
                            normalize = TRUE) {
  # Converting x and y to matrices, so we can treat monocular and binocular cases the same way.
  x <- input_to_matrix(x)
  y <- input_to_matrix(y)
  
  # Checking that matrices dimensions match and are valid.
  if (any(dim(x) != dim(y))) stop("Dimensions for x and y do not match.")
  if (ncol(x) != 1 & ncol(x) != 2) stop("x and y must be vectors or two-column matrices.")
  
  # Checking methods
  if (!is.list(methods)) stop("methods must be a list (not a vector) of method names or functions")
  internal_methods <- list("ek" = saccadr::method_ek,
                           "om" = saccadr::method_om,
                           "nh" = saccadr::method_nh)
  method_handle <- list()
  for(iM in 1:length(methods)){
    # figuring out whether we are calling internal function (string with its name)
    # or an externally supplied one
    if (is.character(methods[[iM]]) & methods[[iM]] %in% names(internal_methods)) {
      # internal method, references by name (string)
      method_handle[[iM]] <- internal_methods[[methods[[iM]]]]
    } else if (is.function(methods[[iM]])) {
      # a function (probably externally defined method)
      method_handle[[iM]] <- methods[[iM]]
    } else {
      # Neither? No idea what we are doing...
      stop(sprintf("Method #%d is neither a valid name, nor a function.", iM))
    }
  }
    
  # Checking trial information
  if (is.null(trial)) {
    # All samples belong to the same trial.
    trial <- rep(1, nrow(x))
  } else {
    if (length(trial) != nrow(x)) stop("Dimensions for x/y and trial do not match.")
    
    # converting to an integer via factor for consistency
    trial <- as.integer(as.factor(trial))
  }
    
  # Binocular data, checks and optional pre-processing.
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
  eye_data <- list()

  for(iEye in 1:ncol(x)) {
    eye_data[[iEye]] <- list()
    eye_data[[iEye]][['sample_vote']] <- matrix(0, nrow = nrow(x), ncol = length(methods))

    # compute velocity
    eye_data[[iEye]][['vel_df']] <- compute_velocity_table(x[, iEye], y[, iEye], trial, sample_rate, velocity_time_window)

    # compute acceleration (for methods that require it)
    eye_data[[iEye]][['acc_df']] <- compute_velocity_table(eye_data[[iEye]][['vel_df']][['x']], eye_data[[iEye]][['vel_df']][['y']], trial, sample_rate, velocity_time_window)

    for(iM in 1:length(methods)){
      # record votes for potential saccades
      eye_data[[iEye]][['sample_vote']][, iM] <- method_handle[[iM]](x = x[, iEye], y = y[, iEye], vel=eye_data[[iEye]][['vel_df']], eye_data[[iEye]][['acc_df']], sample_rate = sample_rate, trial = trial, options = options)
    }
  }
  
  #  if no normalization required, return votes per method and eye
  if (!normalize) return(eye_data)
  
  # normalize votes per eye
  for(iEye in 1:ncol(x)) eye_data[[iEye]][['sample_vote']] <- rowMeans(eye_data[[iEye]][['sample_vote']])
  
  # if binocular processing was selected, average over normalized votes, velocity and acceleration 
  if ( ncol(x) == 2 & binocular == "merge") {
      return(list(
        'sample_vote' = (eye_data[[1]][['sample_vote']] + eye_data[[2]][['sample_vote']]) / 2.0,
        'vel_df' = data.frame('x' = (eye_data[[1]][['vel_df']][['x']] + eye_data[[2]][['vel_df']][['x']]) / 2.0,
                              'y' = (eye_data[[1]][['vel_df']][['y']] + eye_data[[2]][['vel_df']][['y']]) / 2.0,
                              'amp' = (eye_data[[1]][['vel_df']][['amp']] + eye_data[[2]][['vel_df']][['amp']]) / 2.0),
        'acc_df' = data.frame('x' = (eye_data[[1]][['acc_df']][['x']] + eye_data[[2]][['acc_df']][['x']]) / 2.0,
                              'y' = (eye_data[[1]][['acc_df']][['y']] + eye_data[[2]][['acc_df']][['y']]) / 2.0,
                              'amp' = (eye_data[[1]][['acc_df']][['amp']] + eye_data[[2]][['acc_df']][['amp']]) / 2.0)))
  }
  
  # returning normalized votes
  eye_data
}