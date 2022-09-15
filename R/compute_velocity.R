#' Differentiate x and y and compute change amplitude via an algorithm proposed by Engbert & Kliegl (2003)
#'
#' @param x vector with x coordinates in \emph{degrees of visual angle}
#' @param y vector with y coordinates in \emph{degrees of visual angle}
#' @param trial vector with trial index
#' @param sample_rate sample rate in Hz
#' @param velocity_time_window Time window for velocity computation in \emph{milliseconds}
#'
#' @return \code{data.frame} with columns \code{x}, \code{y}, and \code{amp}
#' @export
#' @seealso compute_velocity_ek
#'
#' @examples
#' diff_ek(rnorm(1000), rnorm(1000), rep(1, 1000), 250, list("ek_velocity_time_window" = 20))
diff_ek <- function(x, y,  trial, sample_rate, velocity_time_window) {
  # extracting velocity time window from options
  velocity_time_window <- option_or_default(options, "ek_velocity_time_window", 20)
  
  # single sample duration, Δt in formula #1 in Engbert & Kliegl (2003)
  delta_t <- 1 / sample_rate
  
  ## computing time window for velocity IN SAMPLES.
  time_window_in_samples <- ceiling(velocity_time_window / (1000 * delta_t))
  # making sure we have odd number of samples, so it is centered on the sample...
  if (time_window_in_samples %% 2 == 0) time_window_in_samples <- time_window_in_samples + 1
  # and is at least three samples long.
  if (time_window_in_samples < 3) time_window_in_samples <- 3

  # actual differentiation
  vel_df <- data.frame(
    x = compute_velocity_ek(x, trial, time_window_in_samples, delta_t),
    y = compute_velocity_ek(y, trial, time_window_in_samples, delta_t),
  )
  vel_df[['amp']] <- sqrt(vel_df[['x']]^2 + vel_df[['y']]^2)
  
  vel_df
}
