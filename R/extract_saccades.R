#' Extract saccades from samples using votes from selected methods.
#' 
#' @description Extract saccades from samples using votes from selected methods. Each method votes whether
#' a given sample belongs to a saccade. Next, saccades are identified via a majority vote using the 
#' \code{vote_threshold} parameter, as well as a minimal duration and minimal temporal separation criteria.
#' Please note that units of the gaze samples must be in  \strong{degrees of visual angle}. The units are important
#' as some methods use specific (e.g., physiologically plausible) velocity and acceleration thresholds.
#'  
#' By default, ensemble includes methods proposed by Engbret & Kliegl (2003) (\code{"ek"}),
#' Otero-Millan et al. (\code{"om"}), and Nyström & Holmqvist (2010) (\code{"nh"}), 
#' see \emph{Implemented Methods} vignette. However, it can be extended
#' via custom methods, see \emph{Using Custom Methods} vignette.
#' 
#' By default, the function returns a table with identified saccades but can return a matrix with method's votes
#' per sample instead (\code{return_votes = TRUE}).
#'
#' @param x Horizontal coordinate, either a vector for monocular data or a two-column matrix for binocular data.
#' @param y Vertical coordinate, either a vector for monocular data or a two-column matrix for binocular data.
#' @param sample_rate Sampling rate in Hz. It is assumed to be common for the entire time series.
#' If the time series contains chunks (trials) that were recorded using different acquisition rate
#' (e.g., SR Research Eyelink allows to set different acquisition rate for each recording / trial),
#' you would need to split the time series and analyze them separately.
#' @param trial Optional vector with trial ID. If omitted, all samples are assumed to belong to a single trial.
#' Velocity, acceleration, and saccades themselves are computed respecting trial borders. 
#' @param methods A \emph{list} (not a vector!) with names of package methods (character) or external functions that
#' implement sample classification (see \emph{Using Custom Methods} vignette). Package methods include
#' Engbret & Kliegl (2003) (\code{"ek"}), Otero-Millan et al. (\code{"om"}), Nyström and Holmqvist (2010) (\code{"nh"}).
#' @param options A named list with options for a specific method, see documentation on specific method for details.
#' @param velocity_time_window Time window for computing velocity and acceleration in milliseconds.
#' @param binocular Specifies how a binocular data is treated. Options are \code{"cyclopean"} (binocular data is
#' converted to an average cyclopean image before saccades are extracted), \code{"monocular"} (saccades
#' are extracted independently for each eye), \code{"merge"} (default, saccades are extracted for each eye
#' independently but saccades from different eyes that temporally overlap are averaged into a binocular
#' saccade).
#' @param vote_threshold Value between 0..1 defining a vote threshold for a saccade.
#' By default, all but one method (\eqn{threshold = \frac{N-1}{N}}
#' where N is number of methods used) must agree for a sample to be considered for a saccade.
#' Threshold of 1 is applied if a single method is used.
#' @param minimal_duration_ms Minimal duration of a saccade in milliseconds. Shorter candidate saccades are discarded,
#' @param minimal_separation_ms Minimal time separation between saccades in milliseconds. Saccades that are
#' separated by a shorter interval of "not a saccade" votes, will be merged including that period.
#' @param return_votes Logical. Whether function should return extracted microsaccades (\code{FALSE}, default)
#' or votes per sample (\code{TRUE}). 
#'
#' @return A \code{data.frame} with saccade properties (see **details**), if \code{return_votes = FALSE}.
#' Alternatively, it returns votes per sample (\code{return_votes = TRUE}). For a monocular processing (monocular
#' input, cyclopean or merged binocular data) it is a matrix with \code{nrow(x)} rows and \code{length(methods)}
#'  columns with 0/1 votes for each sample and method. For binocular processing, function returns a two element
#'  \code{list} with same matrices but per eye.
#' @details Variables that describe saccade
#' \itemize{
#' \item{\code{Trial}} Trial index.
#' \item{\code{Eye}} {\code{"Monocular"} for monocular inputs. \code{"Cyclopean"} for binocular data that
#' was averaged \emph{before} applying algorithms. \code{"Binocular"} for binocular data with votes
#' averaged \emph{after} applying algorithms. \code{"Left"} or \code{"Right"} for binocular data
#' when eyes are processed independently.}
#' \item{\code{OnsetSample}} {Index of the first sample.}
#' \item{\code{OffsetSample}} {Index of the last sample.}
#' \item{\code{Onset}} {Onset time relative to the trial start in milliseconds.}
#' \item{\code{Offset}} {Offset time relative to the trial start in milliseconds.}
#' \item{\code{Duration}} {Duration in milliseconds.}
#' \item{\code{DisplacementX}} {Horizontal displacement measured from the \emph{first} to the \emph{last} sample.}
#' \item{\code{DisplacementY}} {Vertical displacement measured from the \emph{first} to the \emph{last} sample.}
#' \item{\code{Displacement}} {Displacement magnitude measured from the \emph{first} to the \emph{last} sample.}
#' \item{\code{DisplacementPhi}} {Displacement direction measured from the \emph{first} to the \emph{last} sample.}
#' \item{\code{AmplitudeX}} {Horizontal displacement measured from the \emph{leftmost} to the \emph{rightmost} sample.}
#' \item{\code{AmplitudeY}} {Vertical displacement measured from the \emph{lowest} to the \emph{uppermost} sample.}
#' \item{\code{Amplitude}} {Displacement magnitude measured from the most extreme samples.}
#' \item{\code{Amplitude}} {Displacement direction measured from the most extreme samples.}
#' \item{\code{VelocityPeak}} {Peak velocity.}
#' \item{\code{VelocityAvg}} {Average velocity.}
#' \item{\code{AccelerationPeak}} {Peak acceleration.}
#' \item{\code{AccelerationAvg}} {Average acceleration.}
#' \item{\code{AccelerationStart}} {Peak acceleration \emph{before} peak velocity was reached.}
#' \item{\code{AccelerationStop}} {Peak acceleration \emph{after} peak velocity was reached.}
#' }
#' @export
#' @importFrom dplyr mutate group_by filter select relocate rowwise ungroup bind_rows case_when
#' @importFrom rlang .data
#'
#' @examples
#' data(single_trial)
#' saccades <- extract_saccades(single_trial$x, single_trial$y, 500)
extract_saccades <- function(x,
                             y,
                             sample_rate,
                             trial = NULL,
                             methods = list("ek", "om", "nh"),
                             options = NULL,
                             velocity_time_window = 20,
                             binocular = "merge",
                             vote_threshold = ifelse(length(methods) == 1, 1, ((length(methods) - 1) / length(methods)) * 0.99),
                             minimal_duration_ms = 12,
                             minimal_separation_ms = 12,
                             return_votes = FALSE){
  
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
  sample_vote <- list()
  vel <- list()
  acc <- list()
  
  for(iEye in 1:ncol(x)) {
    sample_vote[[iEye]] <- matrix(0, nrow = nrow(x), ncol = length(methods))
    
    # compute velocity
    vel[[iEye]] <- compute_velocity_table(x[, iEye], y[, iEye], trial, sample_rate, velocity_time_window)
    
    # compute acceleration (for methods that require it)
    acc[[iEye]] <- compute_velocity_table(vel[[iEye]][['x']], vel[[iEye]][['y']], trial, sample_rate, velocity_time_window)
    
    for(iM in 1:length(methods)){
      # record votes for potential saccades
      sample_vote[[iEye]][, iM] <- method_handle[[iM]](x = x[, iEye],
                                                       y = y[, iEye],
                                                       vel=vel[[iEye]],
                                                       acc[[iEye]],
                                                       sample_rate = sample_rate,
                                                       trial = trial,
                                                       options = options)
      }
  }
  
  # is it only the votes?
  if (return_votes) {
    if (length(sample_vote) == 1) {
      # monocular or cyclopean data, return the matrix itself
      return(sample_vote[[1]])
    }
    
    # return the list of matrices
    return(sample_vote)
  }
  
  # for simpler voting, normalize votes per eye
  normalized_votes <- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
  for(iEye in 1:ncol(x)) normalized_votes[, iEye] <- rowMeans(sample_vote[[iEye]])
  
  # if binocular processing was selected, average over normalized votes
  # also average location, velocity, and acceleration 
  original_x_ncol <- ncol(x)
  if ( ncol(x) == 2 & binocular == "merge") {
    normalized_votes <- rowMeans(normalized_votes)
    x <- rowMeans(x)
    y <- rowMeans(y)
    vel <- list(data.frame('x' = (vel[[1]][['x']] + vel[[2]][['x']]) / 2.0,
                           'y' = (vel[[1]][['y']] + vel[[2]][['y']]) / 2.0,
                           'amp' = (vel[[1]][['amp']] + vel[[2]][['amp']]) / 2.0))
    acc <-  list(data.frame('x' = (acc[[1]][['x']] + acc[[2]][['x']]) / 2.0,
                            'y' = (acc[[1]][['y']] + acc[[2]][['y']]) / 2.0,
                            'amp' = (acc[[1]][['amp']] + acc[[2]][['amp']]) / 2.0))
  }
  
  # converting from units of milliseconds to units of samples
  delta_t_ms <- 1000 / sample_rate
  minimal_separation_in_samples <- ceiling(minimal_separation_ms / delta_t_ms)
  minimal_duration_in_samples <- ceiling(minimal_duration_ms / delta_t_ms)
    
  # compute time within trial
  time_within_trial <- 
    data.frame(trial = trial) %>%
    dplyr::group_by(trial) %>%
    dplyr::mutate(TimeWithinTrial = 0:(n() - 1) * delta_t_ms) %>%
    dplyr::pull(.data$TimeWithinTrial)

  # labeling potential saccade samples: either super-threshold or brief sub-threshold periods
  eye_saccades <- list()
  for(iEye in 1:ncol(x)){
    thresholded_periods <- rle(normalized_votes[, iEye] >= vote_threshold)
    thresholded_periods$values <- (thresholded_periods$values == TRUE) |  # 1. super-threshold
                                  (thresholded_periods$values == FALSE &  # 2. or sub-threshold
                                   lead(thresholded_periods$values) == TRUE & # but surrounded by super-threshold
                                   lag(thresholded_periods$values) == TRUE &  # on both sides
                                   thresholded_periods$lengths <= minimal_separation_in_samples) # and shorter then minimal fixation
    remarked_samples <- inverse.rle(thresholded_periods)

    # retaining only potential saccades that are longer than minimal required duration
    potential_saccades <- rle(remarked_samples)
    potential_saccades$values <- potential_saccades$values == TRUE & potential_saccades$lengths >= minimal_duration_in_samples

    eye_saccades[[iEye]] <-
      # turn rle into a table
      data.frame(Eye = case_when(ncol(x) == 2 ~ c("Left", "Right")[iEye],
                                 original_x_ncol == 1 ~ "Monocular",
                                 (original_x_ncol == 2) & (binocular == "cyclopean") ~ "Cyclopean",
                                 (original_x_ncol == 2) & (binocular == "merge") ~ "Binocular"),
                 IsSaccade = potential_saccades$values,
                 LengthInSamples = potential_saccades$lengths) %>%
      
      # determine onset and offset in samples
      dplyr::mutate(OnsetSample = cumsum(c(1, .data$LengthInSamples[1:(n() - 1)])),
                    OffsetSample = .data$OnsetSample + .data$LengthInSamples - 1) %>%

      # add trial information
      dplyr::mutate(Trial = trial[.data$OnsetSample]) %>%
      dplyr::relocate("Trial") %>%
      
      # retain only saccades
      dplyr::filter(.data$IsSaccade) %>%
      dplyr::select(-c("IsSaccade", "LengthInSamples")) %>%
      
      
      # compute onset/offset within trial and duration in milliseconds
      dplyr::mutate(Onset = time_within_trial[.data$OnsetSample],
                    Offset = time_within_trial[.data$OffsetSample],
                    Duration = .data$Offset - .data$Onset) %>%
      
      # compute x/y and velocity-based properties for each saccade
      dplyr::rowwise() %>%
      dplyr::mutate(DisplacementX = x[.data$OffsetSample[1], iEye] - x[.data$OnsetSample[1], iEye],
                    DisplacementY = y[.data$OffsetSample[1], iEye] - y[.data$OnsetSample[1], iEye],
                    Displacement = sqrt(.data$DisplacementX^2 + .data$DisplacementY^2),
                    DisplacementPhi = atan2(.data$DisplacementY, .data$DisplacementX),
                    
                    AmplitudeX = sign(which.max(x[.data$OnsetSample[1]:.data$OffsetSample[1], iEye]) - which.min(x[.data$OnsetSample[1]:.data$OffsetSample[1], iEye])) *
                                 (max(x[.data$OnsetSample[1]:.data$OffsetSample[1], iEye]) - min(x[.data$OnsetSample[1]:.data$OffsetSample[1], iEye])),
                    AmplitudeY = sign(which.max(y[.data$OnsetSample[1]:.data$OffsetSample[1], iEye]) - which.min(y[.data$OnsetSample[1]:.data$OffsetSample[1], iEye])) *
                                 (max(y[.data$OnsetSample[1]:.data$OffsetSample[1], iEye]) - min(y[.data$OnsetSample[1]:.data$OffsetSample[1], iEye])),
                    Amplitude = sqrt(.data$AmplitudeX^2 + .data$AmplitudeY^2),
                    AmplitudePhi = atan2(.data$AmplitudeY, .data$AmplitudeX),
                    
                    iVelocityPeak = .data$OnsetSample[1]  + which.max(vel[[iEye]][['amp']][.data$OnsetSample[1]:.data$OffsetSample[1]]) - 1,
                    VelocityPeak =  max(vel[[iEye]][['amp']][.data$OnsetSample[1]:.data$OffsetSample[1]], na.rm = TRUE),
                    VelocityAvg = mean(vel[[iEye]][['amp']][.data$OnsetSample[1]:.data$OffsetSample[1]], na.rm = TRUE),
                    AccelerationPeak = max(acc[[iEye]][['amp']][.data$OnsetSample[1]:.data$OffsetSample[1]], na.rm = TRUE),
                    AccelerationAvg = mean(acc[[iEye]][['amp']][.data$OnsetSample[1]:.data$OffsetSample[1]], na.rm = TRUE),
                    AccelerationStart = max(acc[[iEye]][['amp']][.data$OnsetSample[1]:.data$iVelocityPeak[1]], na.rm = TRUE),
                    AccelerationBreak = max(acc[[iEye]][['amp']][.data$iVelocityPeak[1]:.data$OffsetSample[1]], na.rm = TRUE)) %>%
      
      dplyr::ungroup() %>%
      dplyr::select(-c("iVelocityPeak"))
  }
  
  return(dplyr::bind_rows(eye_saccades))
}
