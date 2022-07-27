#' Extracts saccades from samples using votes from selected algorithms.
#'
#' @param x Horizontal coordinate, either a vector for monocular data or a two-column matrix for binocular data.
#' @param y Vertical coordinate, either a vector for monocular data or a two-column matrix for binocular data.
#' @param sample_rate Sampling rate in Hz.
#' @param methods A \emph{list} (not a vector!) with names of package methods (character) or external functions that
#' implement sample classification (see vignette on using custom methods). Package methods include
#' Engbret & Kliegl (2003) (\code{"ek"}), Otero-Millan et al. (\code{"om"}), Nyström and Holmqvist (2010) (\code{"nh"}).
#' @param binocular Specifies how a binocular data is treated. Options are \code{"cyclopean"} (binocular data is
#' converted to an average cyclopean image before saccades are extracted), \code{"monocular"} (saccades
#' are extracted independently for each eye), \code{"merge"} (default, saccades are extracted for each eye
#' independently but saccades from different eyes that temporally overlap are averaged into a binocular
#' saccade). Note that \code{binocular = "merge"} is overridden by \code{normalize = FALSE},
#' votes are left as they are per method and eye.
#' @param vote_threshold Value between 0..1 defining a vote threshold for a saccade.
#' @param trial Optional vector with trial ID. If omitted, all samples are assumed to belong to a single trial.
#' @param options A named list with options for a specific method, see documentation on specific method for details.
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
                             velocity_time_window = 20,
                             methods = list("ek", "om", "nh"),
                             binocular = "merge",
                             vote_threshold = (1 - length(methods)) / length(methods) * 0.99,
                             trial = NULL,
                             options = NULL){
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
  
  # thresholding votes
  is_saccade <- sample_votes >= vote_threshold

  # TODO: extract saccades from votes
}
