#' Compute standard deviation via median estimator.
#'
#' @description Compute standard deviation via median estimator.
#' Please refer to formula #2 in Engbert & Kliegl (2003).
#' Falls back on mean estimator, if computed standard deviation
#' is smaller than \code{.Machine$double.eps}. Raises an error
#' if the results using the mean estimator is still smaller than
#' \code{.Machine$double.eps}.
#'
#'
#' @param x Numeric values
#'
#' @return float
#' @export
#'
#' @examples
#' sd_via_median_estimator(rnorm(100))
sd_via_median_estimator <- function(x){
  estimated_sd <- sqrt(median(x^2) - median(x)^2)
  if (estimated_sd < .Machine$double.eps) {
    estimated_sd <- sqrt(median(x^2) - median(x)^2)

    if (estimated_sd < .Machine$double.eps) stop("Estimated standard deviation is smaller than .Machine$double.eps")
  }
  estimated_sd
}

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
#' @param sample_rate Sample rate of the recording in Hz. Optional, can be omitted, if
#' \code{time} is specified instead. Either \code{time} or
#' \code{sample_rate} must be specified. \code{time} is used and
#' \code{sample_rate} is ignored if both are specified.
#' @param trial Trial id. Trial borders are respected when computing velocity.
#' All samples are assumed to belong to the same trial, if \code{trial = NULL}.
#' @param velocity_time_window Time span in milliseconds relative to the sample
#' that is used  to compute the velocity. Defaults to 20 ms.
#' @param velocity_threshold Velocity threshold for microsaccade detection
#' in medians. Defaults to 6.
#' @param sd_fun Function used to compute standard deviation for velocities.
#' Defaults to \code{sd_via_median_estimator}, as per formula #2 in the paper.
#' Could be replaced with \code{mad}, \code{sd}, etc.
#' @param minimal_duration Minimal duration of a microsaccade in milliseconds.
#' All shorter microsaccades are discarded. Defaults to 12 ms.
#' @param minimal_separation A minimal required time gap between two microsaccades.
#' Two microsaccades that a separated by a smaller time gap are merged into a single
#' microsaccade. Defaults to 12 ms.
#'
#' @return \code{data.frame}
#' @export
#'
#' @examples
extract_ms_ek <- function(x,
                          y,
                          time = NULL,
                          sample_rate = NULL,
                          trial = NULL,
                          velocity_time_window = 20,
                          velocity_threshold = 6,
                          sd_fun = sd_via_median_estimator,
                          minimal_duration = 12,
                          minimal_separation = 12){

  # figuring out whether we need to compute time
  if (is.null(time)) {
    if (is.null(sample_rate)) stop("Either time or sample_rate must be specified")

    frame_duration <- 1000 / sample_rate
    time <- (0:length(x)) * frame_duration
  }
  else {
    if (!is.null(sample_rate)) message("Both time and sample_rate are specified, using time")
    frame_duration <- median(diff(time))
  }

  # computing time window for computing velocity IN SAMPLES
  time_window_samples <- ceiling(velocity_time_window / frame_duration)

  # making sure we have odd number of samples, so it is centered on the sample
  if (time_window_samples %% 2 == 0) time_window_samples <- time_window_samples + 1

  # do we have trial information?
  if (is.null(trial)) trial= rep(1, length(time))

  # check that dimensions actually match (raises an error, if that is not the case)
  check_that_lengths_match(list(x, y, time, trial))


  # filter for convolution via conv is backwards
  i_time_window <- 0:(time_window_samples - 1)
  square_wave <- -sign(i_time_window - median(i_time_window))
  square_wave <- square_wave / (sum(abs(square_wave)) * frame_duration / 1000) # normalizing to per second

  # combining x and y into a single matrix
  # xy <- cbind(x - mean(x, na.rm = TRUE), y - mean(y, na.rm = TRUE))
  xy <- cbind(x, y)

  # computing speed, formula #1 in the paper
  vxy <- apply(xy, MARGIN = 2, FUN=signal::conv, y = square_wave)

  # clip to match the size
  vxy <- vxy[(1 + floor(time_window_samples / 2)) : (nrow(vxy) - floor(time_window_samples / 2)), ]

  # nill the leading and trailing (incomplete convolution) bits
  vxy[-((1 + floor(time_window_samples / 2)) : (nrow(vxy) - floor(time_window_samples / 2))), ] <- 0

  # standard deviation (by default via median estimator as per formula #2)
  sigma_xy <- apply(vxy, MARGIN = 2, FUN=sd_fun)

  # velocity threshold, formula #3
  threshold <- sigma_xy * velocity_threshold

  # computing velocity in the units of the threshold
  v_norm <- sqrt((vxy[, 1] / threshold[1])**2 + (vxy[, 2] / threshold[2])**2)
  v <- sqrt(vxy[, 1]**2 + vxy[, 2]**2)

  # marking out consecutive periods of high (above threshold) velocity
  thresholded_periods <- rle(v_norm > 1.0)
  grouped_periods <-
    data.frame(DurationSamples = thresholded_periods$lengths,
               IsAboveThreshold = thresholded_periods$values) %>%

    # merging over subthreshold velocity periods that are shorter than minimal_separation
    mutate(MarkAsAbove = IsAboveThreshold |
             (!IsAboveThreshold & DurationSamples * frame_duration < minimal_separation))

  # rerunning grouping taking into acount merging over the interruptions
  is_above <- rep(grouped_periods$MarkAsAbove, times=grouped_periods$DurationSamples)
  thresholded_periods2 <- rle(is_above)

  saccades <-
    data.frame(DurationSamples = thresholded_periods2$lengths,
               IsAboveThreshold = thresholded_periods2$values) %>%
    # computing timing of each period
    dplyr::mutate(OnsetSamples = c(1, 1 + cumsum(DurationSamples[1:(n()-1)])),
                  OffsetSamples = cumsum(DurationSamples),
                  DurationMS = DurationSamples * frame_duration) %>%

    # retaining only saccades
    dplyr::filter(IsAboveThreshold, DurationMS > minimal_duration)

  if (nrow(saccades) == 0) {
    return(saccades);
  }

  # computing saccades' properties
  saccades %>%
    rowwise() %>%
    dplyr::mutate(vPeak =  max(v[OnsetSamples[1]:OffsetSamples[1]]),
                  DeltaX = x[OffsetSamples[1]] - x[OnsetSamples[1]],
                  DeltaY = y[OffsetSamples[1]] - y[OnsetSamples[1]],
                  Amplitude = sqrt(DeltaX**2 + DeltaY**2),
                  Phi = atan2(DeltaY, DeltaX),
                  StartTime = time[OnsetSamples[1]],
                  EndTime = time[OffsetSamples[1]],
                  Duration = EndTime - StartTime) %>%
    dplyr::select(-IsAboveThreshold)
}
