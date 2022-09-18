#' Extract saccades using an algorithm proposed by Nyström and Holmqvist (2010) \doi{10.3758/BRM.42.1.188}.
#'
#' @details Method options, please refer to Nyström and Holmqvist (2010) for details on parameters and the rationale for default values.
#' \itemize{
#' \item{\code{nh_sg_filter_order}} {Order of Savitzky-Golay filter. Defaults to \code{2}.}
#' \item{\code{nh_max_velocity}} {Maximal physiologically plausible velocity in °/s. Defaults to \code{1000}.}
#' \item{\code{nh_max_acceleration}} {Maximal physiologically plausible acceleration in °/s². Defaults to \code{100000}.}
#' \item{\code{nh_initial_velocity_threshold}} {Initial velocity threshold in °/s. Defaults to \code{100}.}
#' }
#'
#' @param x Gaze x coordinate, _arbitrary units_ as threshold velocity is computed in units of standard deviation.
#' @param y Gaze y coordinate, _arbitrary units_ as threshold velocity is computed in units of standard deviation.
#' @param vel Velocity \code{data.frame} with columns \code{x}, \code{y}, \code{amp}.
#' @param acc Acceleration \code{data.frame} with columns \code{x}, \code{y}, \code{amp}.
#' @param sample_rate Sample rate in Hz.
#' @param trial Trial id, so that trial borders are respected when computing velocity and saccades.
#' @param options Named list with method options. See \emph{details} for  further information.
#' @return logical vector marking samples that belong to saccades
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by mutate n
#' @importFrom tidyr nest unnest
#' @importFrom stats sd
#' @importFrom rlang .data
#' @seealso \code{\link{extract_saccades}}
#' @examples 
#' # Do not run this function directly, use extract_saccades() instead
method_nh <- function(x,
                      y,
                      vel,
                      acc,
                      sample_rate,
                      trial,
                      options){

  # extracting options or using defaults
  max_velocity <- option_or_default(options, "nh_max_velocity", 1000) 
  max_acceleration <- option_or_default(options, "nh_max_acceleration", 100000)
  initial_velocity_threshold <- option_or_default(options, "nh_initial_velocity_threshold", 100)
  
  # computing frame time step 
  delta_t_s <- 1 / sample_rate
  
  # --- combine compute and filter velocity and acceleration
  samples <- 
    data.frame(trial = trial,
               vel = vel[['amp']],
               acc = acc[['amp']]) %>%

    # nest, so we can process each trial separately
    dplyr::group_by(.data$trial) %>%
    tidyr::nest()

  # --- identify physiologically unrealistic velocity and acceleration
  for(itrial in 1:nrow(samples)){
    # identifying clearly unrealistic peaks
    i_bad_peak <- which(samples$data[[itrial]]$vel > max_velocity | samples$data[[itrial]]$acc > max_acceleration)
    is_bad <- rep(FALSE, nrow(samples$data[[itrial]]))
    is_bad[i_bad_peak] <- TRUE
    is_bad[is.na(samples$data[[itrial]]$vel)] <- TRUE
  
    # marking them out starting from peak and until median velocity is reached
    median_v <- median(samples$data[[itrial]]$vel, na.rm = TRUE)
    
    # forward sweep till offset
    for(i_peak in i_bad_peak){
      isample <- i_peak + 1
      while(isample <= nrow(samples$data[[itrial]]) &        # check limits
            is.finite(samples$data[[itrial]]$vel[isample]) & # finite velocity
            !is_bad[isample] &                               # not marked already
            samples$data[[itrial]]$vel[isample] > median_v){ # velocity above median
        is_bad[isample] <- TRUE
        isample <- isample + 1;
      }
    }
    # backward sweep till onset
    for(i_peak in rev(i_bad_peak)){
      isample <- i_peak - 1
      while(isample > 0 &                                    # check limits
            is.finite(samples$data[[itrial]]$vel[isample]) & # finite velocity
            !is_bad[isample] &                               # not marked already
            samples$data[[itrial]]$vel[isample] > median_v){ # velocity above median
        is_bad[isample] <- TRUE
        isample <- isample - 1;
      }
    }
    samples$data[[itrial]]$is_good <- !is_bad
  }
  
  # --- identifying velocity threshold via iterative adjustment
  all_samples <- unnest(samples, cols = c("data"))
  newPT <- initial_velocity_threshold
  PT <- 2 * newPT
  while(abs(newPT - PT) > 1){
    PT <- newPT
    
    ibelow <- (all_samples$is_good) & (all_samples$vel < PT)
    mu <- mean(all_samples$vel[ibelow])
    sigma <- sd(all_samples$vel[ibelow])
    newPT <- mu + 6 * sigma
  }
  onset_threshold <- mu + 3 * sigma
  
  # --- identifying saccades as peaks above PT threshold and onset_threshold
  is_saccade <- all_samples$is_good & all_samples$vel > PT
  i_saccade_peaks <- which(is_saccade)
  # forward sweep
  for(i_peak in i_saccade_peaks){
    isample <- i_peak + 1
    while(isample <= nrow(all_samples) &                            # check indexes
          all_samples$trial[isample] == all_samples$trial[i_peak] & # still same trial
          !is_saccade[isample] &                                    # did we already check?
          is.finite(all_samples$vel[isample]) &                     # finite velocity
          all_samples$vel[isample] > onset_threshold) {             # above onset/offset threshold
      is_saccade[isample] <- TRUE
      isample <- isample + 1
    }
  }
  # backward sweep
  for(i_peak in rev(i_saccade_peaks)){
    isample <- i_peak - 1
    while(isample >= 1 &                                            # check indexes
          all_samples$trial[isample] == all_samples$trial[i_peak] & # still same trial
          !is_saccade[isample] &                                    # did we already check?
          is.finite(all_samples$vel[isample]) &                     # finite velocity
          all_samples$vel[isample] > onset_threshold) {             # above onset/offset threshold
      is_saccade[isample] <- TRUE
      isample <- isample + 1
    }
  }

 is_saccade
}



