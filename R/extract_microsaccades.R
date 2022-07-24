#' Extracts microsaccades from samples using votes from selected algorithms.
#'
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
#' microsaccade).
#' @param vote_threshold Value between 0..1 defining a vote threshold for a saccade. The default (\code{vote_threshold = 1})
#' means that _all_ methods must agree.
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
extract_saccades <- function(x,
                             y,
                             sample_rate,
                             velocity_time_window = 20,
                             methods = list("ek"),
                             binocular = "merge",
                             vote_threshold = 1,
                             trial = NULL,
                             options = list()){
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
