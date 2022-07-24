#' Computes ensemble vote for each sample using selected methods.
#' 
#' This function is called by \code{\link{extract_microsaccades}} but can be used directly,
#' if you are interested in the vote-per-sample.
#' @param x Horizontal coordinate, ether a vector for monocular data or a two-column matrix for binocular data. 
#' @param y Vertical coordinate, ether a vector for monocular data or a two-column matrix for binocular data. 
#' @param sample_rate Sampling rate in Hz.
#' @param methods A _list_ (not a vector!) with names of package methods (character) or external functions that
#' implement sample classification (see vignette on using custom method). Package methods include 
#' Engbret & Kliegl (2003) (\code{"ek"}).
#' @param binocular Specifies how a binocular data is treated. Options are \code{"cyclopean"} (binocular data is
#' converted to an average cyclopean image before microsaccades are extracted), \code{"monocular"} (microsaccades
#' are extracted independently for each eye), \code{"merge"} (default, microsaccades are extracted for each eye
#' independently but microsaccades from different eyes that temporally overlap are averaged into a binocular
#' microsaccade). Note that \code{binocular = "merge"} is overridden by \code{normalize = FALSE},
#' votes are left as they are per method and eye. 
#' @param normalize Logical, whether to aggregate and normalize votes to 0..1 range. Note that
#' \code{normalize = FALSE} overrides \code{binocular = "merge"}, votes are left as they are per method and eye.
#' @param trial Optional vector with trial ID. If omitted, all samples are assumed to belong to a single trial.
#' @param options A named list with options for a specific method.
#'
#' @return list Either a list of matrices (one per eye) with votes for each method (\code{normalized = FALSE}) or
#' a matrix with normalized (0..1) vote on each sample either for each eye or for single cyclopean or merged
#' binocular time series (\code{normalized = TRUE}).
vote_on_samples <- function(x,
                            y,
                            sample_rate,
                            velocity_time_window = 20,
                            methods = list("ek"),
                            binocular = "merge",
                            normalize = TRUE,
                            trial = NULL,
                            options = NULL) {
  # Converting x and y to matrices, so we can treat monocular and binocular cases the same way.
  x <- input_to_matrix(x)
  y <- input_to_matrix(y)
  
  # Checking that matrices dimensions match and are valid.
  if (any(dim(x) != dim(y))) stop("Dimensions for x and y do not match.")
  if (ncol(x) != 1 & ncol(x) != 2) stop("x and y must be vectors or two-column matrices.")
  
  # Checking methods
  if (!is.list(methods)) stop("methods must be a list (not a vector) of method names or functions")
  internal_methods <- list("ek" = saccadr::extract_ms_ek,
                           "om" = saccadr::extract_ms_om)
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
  sample_vote_per_eye <- list()
  for(iEye in ncol(x)) sample_vote_per_eye[[iEye]] <- matrix(0, nrow = nrow(x), ncol = length(methods))

  for(iEye in 1:ncol(x)) {
    # compute velocity
    vel_df <- data.frame(
      x = compute_velocity(x[, iEye], trial, sample_rate, velocity_time_window),
      y = compute_velocity(y[, iEye], trial, sample_rate, velocity_time_window)
    )
    vel_df[['amp']] <- sqrt(vel_df[['x']]^2 + vel_df[['y']]^2)
    
    # compute acceleration (for methods that require it)
    acc_df <- data.frame(
      x = compute_velocity(vel_df[['x']], trial, sample_rate, velocity_time_window),
      y = compute_velocity(vel_df[['y']], trial, sample_rate, velocity_time_window)
    )
    acc_df[['amp']] <- sqrt(acc_df[['x']]^2 + acc_df[['y']]^2)
    
    for(iM in 1:length(methods)){
      # turning options into parameters passed via do.call (unused are simply ignored in R)
      call_arguments <- c(list(x = x[, iEye], y = y[, iEye], vel=vel_df, acc=acc_df, sample_rate = sample_rate, trial = trial), options)
      
      # record votes for potential saccades
      sample_vote_per_eye[[iEye]][, iM] <- do.call(method_handle[[iM]], call_arguments)
    }
  }
  
  #  if no normalization required, return votes per method and eye
  if (!normalize) return(sample_vote_per_eye)
  
  # normalize votes per eye
  sample_vote <- matrix(0, nrow = nrow(x), ncol = ncol(x))
  for(iEye in 1:ncol(x)) sample_vote[, iEye] <- rowMeans(sample_vote_per_eye[[iEye]])
  
  # if binocular processing was selected, average over normalized votes
  if ( ncol(x) == 2 & binocular == "merge") {
    sample_vote <- rowMeans(sample_vote)
  }
  
  # returning normalized votes
  sample_vote
}