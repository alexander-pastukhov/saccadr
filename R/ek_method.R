#' Extract microsaccades using an algorithm proposed by Engbert and Kliegl (2003)
#'
#' @description Extract microsaccades using an algorithm proposed by Engbert and
#' Kliegl (2003).
#'
#' @param x Gaze x coordinate, arbitrary units as threshold velocity is computed in units of standard deviation.
#' @param y Gaze x coordinate, arbitrary units as threshold velocity is computed in units of standard deviation.
#' @param time Sample time in milliseconds. Optional, can be omitted, if
#' \code{sample_rate} is specified instead. Either \code{time} or
#' \code{sample_rate} must be specified. \code{time} is used and
#' \code{sample_rate} is ignored if both are specified.
#' @param sample_rate Sample rate of the recording in Hz.
#' @param trial Trial id. Trial borders are respected when computing velocity.
#' All samples are assumed to belong to the same trial, if \code{trial = NULL}.
#' @param velocity_time_window Time span in milliseconds relative to the sample
#' that is used  to compute the velocity. Defaults to 20 ms.
#' @param velocity_threshold Velocity threshold for microsaccade detection
#' in medians. Defaults to 6.
#' @param sd_fun Function used to compute standard deviation for velocities.
#' Defaults to \code{sd_via_median_estimator}, as per formula #2 in the paper.
#' Could be replaced with \code{mad}, \code{sd}, etc.
#' @param minimal_duration_ms Minimal duration of a microsaccade in milliseconds.
#' All shorter microsaccades are discarded. Defaults to 12 ms.
#' @param minimal_separation_ms A minimal required time gap between two microsaccades.
#' Two microsaccades that a separated by a smaller time gap are merged into a single
#' microsaccade. Defaults to 0 ms.
#'
#' @return \code{data.frame}
#' @export
#' @importFrom dplyr %>% mutate filter select relocate rowwise n
#'
#' @examples
extract_ms_ek <- function(x,
                          y,
                          sample_rate,
                          trial = NULL,
                          velocity_time_window = 20,
                          velocity_threshold = 6,
                          sd_fun = sd_via_median_estimator,
                          minimal_duration_ms = 12,
                          minimal_separation_ms = 0){

  # single sample duration (Δt in formula #1)
  delta_t <- 1 / sample_rate
  delta_t_ms <- 1000 * delta_t
  minimal_separation_in_samples <- ceiling(minimal_separation_ms / delta_t_ms)

  ## computing time window for velocity IN SAMPLES.
  time_window_in_samples <- ceiling(velocity_time_window / delta_t_ms)
  # making sure we have odd number of samples, so it is centered on the sample...
  if (time_window_in_samples %% 2 == 0) time_window_in_samples <- time_window_in_samples + 1
  # and is at least three samples long.
  if (time_window_in_samples < 3) time_window_in_samples <- 3

  # do we have trial information? If not, all samples are from the same trial.
  if (is.null(trial)) trial <- rep(1, length(x))

  # check that dimensions actually match, raises an error, if that is not the case.
  check_that_lengths_match(list(x, y, trial))
  
  # # compute velocity per trial.
  vel_xy <- matrix(c(compute_velocity_ek(x, trial, time_window_in_samples, delta_t),
                     compute_velocity_ek(y, trial, time_window_in_samples, delta_t)),
                   ncol = 2)
  v <- sqrt(vel_xy[, 1]^2 + vel_xy[, 2]^2)
  
  # standard deviation (by default via median estimator as per formula #2)
  sigma_xy <- apply(vel_xy, MARGIN = 2, FUN = sd_fun, na.rm = TRUE)
  
  # velocity threshold, formula #3
  vel_threshold <- sigma_xy * velocity_threshold

  # computing normalized velocity in the units of the threshold
  vel_norm <- sqrt((vel_xy[, 1] / vel_threshold[1])^2 + (vel_xy[, 2] / vel_threshold[2])^2)

  # marking out consecutive periods of high (above threshold) velocity
  thresholded_periods <- rle(vel_norm > 1.0)
  grouped_periods <- 
    data.frame(DurationInSamples = thresholded_periods$lengths,
               IsAboveThreshold = thresholded_periods$values) %>%
    
    # merging over subthreshold velocity periods that are shorter than minimal_separation
    mutate(MarkAsAbove = IsAboveThreshold | 
           (!IsAboveThreshold & DurationInSamples <= minimal_separation_in_samples))
  
  # rerunning grouping taking into account merging over the interruptions
  is_above_threshold <- rep(grouped_periods$MarkAsAbove, times=grouped_periods$DurationInSamples)
  thresholded_periods <- rle(is_above_threshold)

  saccades <-
    data.frame(DurationInSamples = thresholded_periods$lengths,
               IsAboveThreshold = thresholded_periods$values) %>%
    
    # computing timing of each period
    dplyr::mutate(OnsetSample = c(1, 1 + cumsum(DurationInSamples[1:(dplyr::n()-1)])),
                  OffsetSample = cumsum(DurationInSamples),
                  DurationMS = DurationInSamples * delta_t_ms) %>%

    # retaining only saccades
     dplyr::filter(IsAboveThreshold, DurationMS >= minimal_duration_ms)

  if (nrow(saccades) == 0) {
    return(NULL);
  }

  # computing saccades' properties
  saccades %>%
    rowwise() %>%
    dplyr::mutate(vPeak =  max(v[OnsetSample[1]:OffsetSample[1]]),
                  DeltaX = x[OffsetSample[1]] - x[OnsetSample[1]],
                  DeltaY = y[OffsetSample[1]] - y[OnsetSample[1]],
                  DeltaPhi = atan2(DeltaY, DeltaX),
                  AmpX = sign(which.max(x[OnsetSample[1]:OffsetSample[1]]) - which.min(x[OnsetSample[1]:OffsetSample[1]])) * 
                         (max(x[OnsetSample[1]:OffsetSample[1]]) - min(x[OnsetSample[1]:OffsetSample[1]])),
                  AmpY = sign(which.max(y[OnsetSample[1]:OffsetSample[1]]) - which.min(y[OnsetSample[1]:OffsetSample[1]])) *
                         (max(y[OnsetSample[1]:OffsetSample[1]]) - min(y[OnsetSample[1]:OffsetSample[1]])),
                  Amplitude = sqrt(AmpX^2 + AmpY^2),
                  AmpPhi = atan2(AmpY, AmpX)
                  ) %>%
    dplyr::select(-IsAboveThreshold) %>%
    dplyr::relocate(OnsetSample, OffsetSample)
}
