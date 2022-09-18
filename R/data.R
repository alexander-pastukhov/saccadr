#' A single trial monocular samples recorded at 500 Hz.
#'
#' A single trial monocular samples recorded at 500 Hz.
#'
#' @format A data frame with 1006 rows and 2 variables:
#' \describe{
#'   \item{x}{X coordinate in degrees of visual angle.}
#'   \item{y}{Y coordinate in degrees of visual angle.}
#' }
"single_trial"

#' A monocular multi-trial recording
#'
#' A monocular recording, 10 trials, sampling rate 500 Hz.
#'
#' @format A data frame with 14353 rows and 4 variables:
#' \describe{
#'   \item{trial}{Trial index.}
#'   \item{x}{X coordinate in degrees of visual angle.}
#'   \item{y}{Y coordinate in degrees of visual angle.}
#'   \item{time}{Sample time in milliseconds.}
#' }
"monocular_ten_trials"

#' A single trial binocular recording.
#'
#' A single trial binocular recording sampled at 1000 Hz.
#'
#' @format A data frame with 2000 rows and 6 variables:
#' \describe{
#'   \item{trial}{Trial index.}
#'   \item{time_rel}{Sample time in milliseconds relative to the trial start.}
#'   \item{xL}{X coordinate for the left eye in degrees of visual angle.}
#'   \item{xR}{X coordinate for the right eye in degrees of visual angle.}
#'   \item{yL}{Y coordinate for the left eye in degrees of visual angle.}
#'   \item{yR}{Y coordinate for the right eye in degrees of visual angle.}
#' }
"single_trial_binocular"
