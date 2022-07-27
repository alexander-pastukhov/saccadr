#' Extract saccades using an algorithm proposed by Engbert and Kliegl (2003) \doi{10.1016/S0042-6989(03)00084-1}
#'
#' @details Method options, please refer to Engbert and Kliegl (2003) for details on parameters and the rationale for default values.
#' \itemize{
#' \item{\code{ek_velocity_threshold}} {Velocity threshold for saccade detection in medians. Defaults to \code{6}.}
#' \item{\code{ek_sd_fun}} {Function used to compute standard deviation for velocities. Defaults to \code{\link{sd_via_median_estimator}}, as per formula #2 in Engbert and Kliegl (2003). Can be replaced with \code{mad}, \code{sd}, etc.}
#' \item{\code{ek_minimal_duration_ms}} {Minimal duration of a saccade in milliseconds. Defaults to \code{12}.}
#' \item{\code{ek_minimal_separation_ms}} {A minimal required time gap between saccades. Defaults to \code{12}.}
#' }
#' @param x Gaze x coordinate, _arbitrary units_ as threshold velocity is computed in units of standard deviation.
#' @param y Gaze x coordinate, _arbitrary units_ as threshold velocity is computed in units of standard deviation.
#' @param vel Velocity \code{data.frame} with columns \code{x}, \code{y}, \code{amp}.
#' @param acc Acceleration \code{data.frame} with columns \code{x}, \code{y}, \code{amp}.
#' @param sample_rate Sample rate in Hz.
#' @param trial Trial id, so that trial borders are respected when computing velocity and saccades.
#' @param options Names list with method options. See \emph{details} for further information.
#' @return logical vector marking samples that belong to saccades
#' @export
#' @seealso \code{\link{vote_on_samples}}, \code{\link{extract_saccades}}
#' @examples
#' # Do not run this function directly, use vote_on_samples() or extract_saccades()
method_ek <- function(x,
                      y,
                      vel,
                      acc,
                      sample_rate,
                      trial,
                      options){
  # options
  velocity_threshold <- option_or_default(options, "ek_velocity_threshold", 6)
  sd_fun <- option_or_default(options, "ek_sd_fun", sd_via_median_estimator)
  minimal_duration_ms <- option_or_default(options, "ek_minimal_duration_ms", 12)
  minimal_separation_ms <- option_or_default(options, "ek_minimal_separation_ms", 12)

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
