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
                          velocity_threshold = 6,
                          sd_fun = sd_via_median_estimator,
                          minimal_duration_ms = 12,
                          minimal_separation_ms = 12){
  delta_t_s <- 1 / sample_rate
  
  # --- compute and filter velocity and acceleration
  trace <- data.frame(trial = trial,
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
  itrial <- 1
  i_bad_peak <- which(trace$data[[itrial]]$vel > max_velocity | trace$data[[itrial]]$acc > max_acceleration)
  
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