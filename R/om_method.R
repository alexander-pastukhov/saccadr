#' Extract microsaccades using an algorithm proposed by Otero-Millan et al. (2014)
#' 
#' @param x Gaze x coordinate, _arbitrary units_ as threshold velocity is computed in units of standard deviation.
#' @param y Gaze x coordinate, _arbitrary units_ as threshold velocity is computed in units of standard deviation.
#' @param vel Velocity data.frame with columns \code{x}, \code{y}, \code{amp}.
#' @param acc Acceleration data.frame with columns \code{x}, \code{y}, \code{amp}.
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by mutate lag lead pull filter select
#' @importFrom tidyr nest
#' @examples
#' # do not run this function directly, use extract_microsaccades
extract_ms_om <- function(x,
                          y,
                          vel,
                          acc,
                          sample_rate,
                          trial,
                          minimal_inter_peak_time_ms = 30,
                          maximal_peaks_per_second = 5,
                          velocity_threshold = 6,
                          sd_fun = sd_via_median_estimator,
                          minimal_duration_ms = 12,
                          minimal_separation_ms = 12){
  
  # identify local peaks within each trial
  peaks <-
    data.frame(trial = trial, vel = vel[['amp']]) %>%
    
    # peak computation must be trial border aware
    dplyr::group_by(trial) %>%
    
    # create time variable
    dplyr::mutate(t = (0:(n()-1)) * 1000 / sample_rate) %>%
    
    
    # maximum is a sample that is larger than immediate surround
    dplyr::mutate(IsMaxima = vel > dplyr::lag(vel, n = 1L) & vel > dplyr::lead(vel, n = 1L)) %>%
    
    # retain only peaks
    dplyr::filter(IsMaxima) %>%
    dplyr::select(!IsMaxima) %>%
    
    # nest them, so we get a separate table for each trial
    tidyr::nest()
  
  # prune local peaks so that they are 
  # 1) separated by at least `minimal_inter_peak_time_ms` ms
  # 2) no more than `maximal_peaks_per_second`
  for(itrial in 1:nrow(peaks)){
    # get peak order from highest to lowest
    ipeak <- 1:nrow(peaks$data[[itrial]])
    ipeak <- ipeak[order(peaks$data[[itrial]]$vel, decreasing = TRUE)]
    
    # highest peak is always selected
    iselected <- ipeak[1]

    # the rest have to fight it out
    for(ip in ipeak[2:length(ipeak)]){
      # compute the smallest time interval to already selected peaks
      time_to_selected <- peaks$data[[itrial]]$t[iselected] - peaks$data[[itrial]]$t[ip]
      
      # 1) anything within `minimal_inter_peak_time_ms` is no good
      if (min(abs(time_to_selected)) < minimal_inter_peak_time_ms) next
      
      # 2) do we have already too many within 1 second?
      peaks_within_one_second <- sum(abs(time_to_selected) < 0.5)
      if (peaks_within_one_second >= maximal_peaks_per_second) next
      
      iselected <- c(iselected, ip)
    }
  }
  
  
  
  # get peak Order from highest to lowest
  ipeak <- which(peaks$IsMaxima)
  ipeak <- ipeak[order(peaks$vel[ipeak], decreasing = TRUE)]
  
  
  peaks[ipeak, ]
  
  
  # retain only highest peaks that are 
  # 1) separated by at least `minimal_inter_peak_time_ms` ms
  # 2) no more that
    
    
  
}