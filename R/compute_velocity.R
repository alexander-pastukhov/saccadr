#' Compute velocity based on Engbert & Kliegl (2003)
#' @param x Vector of values
#' @param trial Vector of trial indexes.
#' @param sample_rate Sample rate of the recording in Hz.
#' @param velocity_time_window Time window in milliseconds relative to the sample that is used  to compute the velocity.
#' @return vector of float
#' @export
#'
#' @examples
#' compute_velocity(rnorm(100), rep(1, 100), 500, 20)
compute_velocity <- function(x, trial, sample_rate, velocity_time_window){
  # single sample duration, Δt in formula #1 in Engbert & Kliegl (2003)
  delta_t <- 1 / sample_rate

  ## computing time window for velocity IN SAMPLES.
  time_window_in_samples <- ceiling(velocity_time_window / (1000 * delta_t))
  # making sure we have odd number of samples, so it is centered on the sample...
  if (time_window_in_samples %% 2 == 0) time_window_in_samples <- time_window_in_samples + 1
  # and is at least three samples long.
  if (time_window_in_samples < 3) time_window_in_samples <- 3
  
  # compute velocity per trial using Engbert and Kliegl (2003) formula
  compute_velocity_via_ek(x, trial, time_window_in_samples, delta_t)
}

#' Compute velocity for x, y, and its amplited 
#'
#' @param x vector with x coordinates in \emph{degrees of visual angle}
#' @param y vector with y coordinates in \emph{degrees of visual angle}
#' @param trial vector with trial index
#' @param sample_rate sample rate in Hz
#' @param velocity_time_window Time window for velocity computation in \emph{milliseconds}
#'
#' @return \code{data.frame} with columns \code{x}, \code{y}, and \code{amp}
#' @export
#'
#' @examples
#' compute_velocity_table(rnorm(1000), rnorm(1000), rep(1, 1000), 250, 20)
compute_velocity_table <- function(x, y,  trial, sample_rate, velocity_time_window) {
  vel_df <- data.frame(
    x = compute_velocity(x, trial, sample_rate, velocity_time_window),
    y = compute_velocity(y, trial, sample_rate, velocity_time_window)
  )
  vel_df[['amp']] <- sqrt(vel_df[['x']]^2 + vel_df[['y']]^2)
  
  vel_df
}