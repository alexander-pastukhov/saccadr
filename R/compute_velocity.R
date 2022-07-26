#' Compute velocity based on Engbert & Kliegl (2003)
#' @param x Vector of values
#' @param sample_rate Sample rate of the recording in Hz.
#' @param velocity_time_window Time span in milliseconds relative to the sample
#' that is used  to compute the velocity. Defaults to 20 ms.
#' @return vector of float
#' @export
#'
#' @examples
#' compute_velocity(rnorm(100), 500, 20)
compute_velocity <- function(x, trial, sample_rate, velocity_time_window){
  # single sample duration, Δt in formula #1 in Engbert & Kliegl (2003)
  delta_t <- 1 / sample_rate

  ## computing time window for velocity IN SAMPLES.
  time_window_in_samples <- ceiling(velocity_time_window / (1000 * delta_t))
  # making sure we have odd number of samples, so it is centered on the sample...
  if (time_window_in_samples %% 2 == 0) time_window_in_samples <- time_window_in_samples + 1
  # and is at least three samples long.
  if (time_window_in_samples < 3) time_window_in_samples <- 3
  
  # # compute velocity per trial.
  compute_velocity_ek(x, trial, time_window_in_samples, delta_t)
}
