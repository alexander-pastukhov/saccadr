#' Extract microsaccades using an algorithm proposed by Nyström and Holmqvist (2010)
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by mutate n
#' @importFrom tidyr nest unnest
#' @export
extract_ms_nh <- function(x,
                          y,
                          vel,
                          acc,
                          sample_rate,
                          trial,
                          sg_filter_order = 2,
                          max_velocity = 1000,
                          max_acceleration = 100000,
                          initial_velocity_threshold = 100,
                          velocity_threshold = 6,
                          sd_fun = sd_via_median_estimator,
                          minimal_duration_ms = 12,
                          minimal_separation_ms = 12){
  delta_t_s <- 1 / sample_rate
  
  # --- compute and filter velocity and acceleration
  samples <- data.frame(trial = trial,
                      x = x,
                      y = y) %>%
    # adding overall index
    dplyr::mutate(irow = 1:n()) %>%
    
    # compute velocity and acceleration for each trial
    dplyr::group_by(trial) %>%
    dplyr::mutate(velx = (x - lag(x)) / delta_t_s,
                  vely = (y - lag(y)) / delta_t_s,
                  vel = sqrt(velx^2 + vely^2),
                  accx = (velx - lag(velx)) / delta_t_s,
                  accy = (vely - lag(vely)) / delta_t_s,
                  acc = sqrt(accx^2 + accy^2)) %>%
    
    # filter velocity
    dplyr::mutate(vel = filter_via_savitzky_golay(vel, sg_filter_order),
                  acc = filter_via_savitzky_golay(acc, sg_filter_order)) %>%
    
    # nest, so we can process each trial separately
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
  all_samples <- unnest(samples, cols = c(data))
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



#' Smooths signal using Savitzky-Golay and then shifts the filtered signal back
#'
#' @param x 
#' @param sgOrder 
#'
#' @return
#' @importFrom signal sgolayfilt
#' @export
#' @keywords internal
#'
#' @examples
filter_via_savitzky_golay <- function(x, sg_order){
  sg_length <- sg_order + 3 - sg_order %% 2
  x_shifted <- signal::sgolayfilt(x, p = sg_order, n = sg_length)
  x_filtered <- rep(NA, length(x))
  x_filtered[seq(1 + sg_length, length(x))] <- x_shifted[seq(1, length(x)-sg_length)]
  x_filtered
}