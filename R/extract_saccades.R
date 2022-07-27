#' Extracts saccades from samples using votes from selected algorithms.
#'
#' @param x Horizontal coordinate, either a vector for monocular data or a two-column matrix for binocular data.
#' @param y Vertical coordinate, either a vector for monocular data or a two-column matrix for binocular data.
#' @param sample_rate Sampling rate in Hz.
#' @param trial Optional vector with trial ID. If omitted, all samples are assumed to belong to a single trial.
#' @param methods A \emph{list} (not a vector!) with names of package methods (character) or external functions that
#' implement sample classification (see vignette on using custom methods). Package methods include
#' Engbret & Kliegl (2003) (\code{"ek"}), Otero-Millan et al. (\code{"om"}), Nyström and Holmqvist (2010) (\code{"nh"}).
#' @param options A named list with options for a specific method, see documentation on specific method for details.
#' @param velocity_time_window Time window for computing velocity and acceleration in milliseconds.
#' @param binocular Specifies how a binocular data is treated. Options are \code{"cyclopean"} (binocular data is
#' converted to an average cyclopean image before saccades are extracted), \code{"monocular"} (saccades
#' are extracted independently for each eye), \code{"merge"} (default, saccades are extracted for each eye
#' independently but saccades from different eyes that temporally overlap are averaged into a binocular
#' saccade). Note that \code{binocular = "merge"} is overridden by \code{normalize = FALSE},
#' votes are left as they are per method and eye.
#' @param vote_threshold Value between 0..1 defining a vote threshold for a saccade.
#'
#' @return \code{data.frame}
#' @export
#'
#' @examples
#' data(single_trial)
#' ms <- extract_saccades(single_trial$x, single_trial$y, 500)
extract_saccades <- function(x,
                             y,
                             sample_rate,
                             trial = NULL,
                             methods = list("ek", "om", "nh"),
                             options = NULL,
                             velocity_time_window = 20,
                             binocular = "merge",
                             vote_threshold = ((length(methods) - 1) / length(methods)) * 0.99,
                             minimal_duration_ms = 12,
                             minimal_separation_ms = 12){
  
  # Converting x and y to matrices, so we can treat monocular and binocular cases the same way.
  x <- input_to_matrix(x)
  y <- input_to_matrix(y)
  
  # getting sample votes
  sample_votes <- vote_on_samples(x = x,
                                  y = y,
                                  sample_rate = sample_rate, 
                                  velocity_time_window = velocity_time_window,
                                  methods = methods,
                                  binocular = binocular,
                                  normalize = TRUE,
                                  trial = trial,
                                  options = options)
  
  # converting from units of milliseconds to units of samples
  delta_t_ms <- 1000 / sample_rate
  minimal_separation_in_samples <- ceiling(minimal_separation_ms / delta_t_ms)
  minimal_duration_in_samples <- ceiling(minimal_duration_ms / delta_t_ms)

  # labeling potential saccade samples: either super-threshold or brief sub-threshold periods
  saccades <- list()
  for(iEye in 1:ncol(x)){
    thresholded_periods <- rle(sample_votes[, iEye] >= vote_threshold)
    thresholded_periods$values <- (thresholded_periods$values == TRUE) |  # 1. super-threshold
                                  (thresholded_periods$values == FALSE &  # 2. or sub-threshold
                                   lead(thresholded_periods$values) == TRUE & # but surrounded by super-threshold
                                   lag(thresholded_periods$values) == TRUE &  # on both sides
                                   thresholded_periods$lengths <= minimal_separation_in_samples) # and shorter then minimal fixation
    marked_samples <- inverse.rle(thresholded_periods)

    # retaining only potential saccades that are longer than minimal required duration
    potential_saccades <- rle(marked_samples)
    potential_saccades$values <- potential_saccades$values == TRUE & potential_saccades$lengths >= minimal_duration_in_samples

    # TODO: extract saccades from votes
    saccades[[iEye]] <- potential_saccades
  }
  
  if (ncol(x) == 1) return(saccades[[1]])
  saccades
}
