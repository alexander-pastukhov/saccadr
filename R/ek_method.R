#' Extract microsaccades using an algorithm proposed by Engbert and Kliegl (2003)
#'
#' @description Marks out samples that belong to saccades using an algorithm
#' proposed by Engbert and Kliegl (2003). Do not call this function directly,
#' instead use the common interface via \code{\link{extract_microsaccades}} passing
#' additional parameters (\code{velocity_threshold}, \code{sd_fun},
#' \code{minimal_duration_ms}, \code{minimal_separation_ms}) via its \code{options}
#' parameter.
#'
#' @param x Gaze x coordinate, _arbitrary units_ as threshold velocity is computed in units of standard deviation.
#' @param y Gaze x coordinate, _arbitrary units_ as threshold velocity is computed in units of standard deviation.
#' @param vel Velocity data.frame with columns \code{x}, \code{y}, \code{amp}.
#' @param acc Acceleration data.frame with columns \code{x}, \code{y}, \code{amp}.
#' @param sample_rate Sample rate of the recording in Hz.
#' @param trial Trial id. Trial borders are respected when computing velocity.
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
#' @return logical vector marking samples that belong to saccades
#' @seealso \code{\link{extract_microsaccades}}
#' @export
#'
#' @examples
#' # do not run this function directly, use extract_microsaccades
extract_ms_ek <- function(x,
                          y,
                          vel,
                          acc,
                          sample_rate,
                          trial,
                          velocity_threshold = 6,
                          sd_fun = sd_via_median_estimator,
                          minimal_duration_ms = 12,
                          minimal_separation_ms = 12){

  # single sample duration (Δt in formula #1)
  delta_t_ms <- 1000 / sample_rate
  minimal_separation_in_samples <- ceiling(minimal_separation_ms / delta_t_ms)
  minimal_duration_in_samples <- ceiling(minimal_duration_ms / delta_t_ms)

  # standard deviation (by default via median estimator as per formula #2)
  sigma_xy <- apply(vel[, c("x", "y")], MARGIN = 2, FUN = sd_fun, na.rm = TRUE)
  
  # velocity threshold, formula #3
  vel_threshold <- sigma_xy * velocity_threshold

  # computing normalized velocity in the units of the threshold
  vel_norm <- sqrt((vel[['x']]/ vel_threshold[1])^2 + (vel[['y']] / vel_threshold[2])^2)

  # marking out consecutive periods of high (above threshold) velocity
  thresholded_periods <- rle(vel_norm > 1.0)
  
  # labeling potential saccade samples: either super-threshold or brief sub-threshold periods
  thresholded_periods$values <- (thresholded_periods$values == TRUE) |  # 1. super-threshold
                                (thresholded_periods$values == FALSE &  # 2. or sub-threshold
                                 lead(thresholded_periods$values) == TRUE & # but surrounded by super-threshold 
                                 lag(thresholded_periods$values) == TRUE &  # on both sides
                                 thresholded_periods$lengths <= minimal_separation_in_samples) # and shorter then minimal fixation
  marked_samples <- inverse.rle(thresholded_periods)
  
  # retaining only potential saccades that are longer than minimal required duration
  potential_saccades <- rle(marked_samples)
  potential_saccades$values <- potential_saccades$values == TRUE & potential_saccades$lengths >= minimal_duration_in_samples
  inverse.rle(potential_saccades)
}
