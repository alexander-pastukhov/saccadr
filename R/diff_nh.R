#' Smooths signal using Savitzky-Golay and then shifts the filtered signal back
#'
#' @param x vector of float
#' @param sgOrder integer, order of the filter
#'
#' @return vector of float
#' @importFrom signal sgolayfilt
#' @export
#' @keywords internal
#'
#' @examples
#' filter_via_savitzky_golay(rnorm(1000), 2)
filter_via_savitzky_golay <- function(x, sg_order){
  sg_length <- sg_order + 3 - sg_order %% 2
  x_shifted <- signal::sgolayfilt(x, p = sg_order, n = sg_length)
  x_filtered <- rep(NA, length(x))
  x_filtered[seq(1 + sg_length, length(x))] <- x_shifted[seq(1, length(x)-sg_length)]
  x_filtered
}


#' Differentiate x and y and compute change amplitude via an algorithm proposed by Nyström and Holmqvist (2010) \doi{10.3758/BRM.42.1.188}
#' 
#' @description Differentiate x and y and compute change amplitude via an algorithm proposed by Nyström 
#' and Holmqvist (2010) \doi{10.3758/BRM.42.1.188}. Note that both components and the amplitude are smoothed
#' \emph{independently} via a Savitzky-Golay filter, so the components may not (probably won't) add up to
#' the amplitude. Note that filtering is sensitive to the presence of \code{NA}.
#' 
#' @details Method options, please refer to Nyström and Holmqvist (2010) for details on parameters and the rationale for default values.
#' \itemize{
#' \item{\code{nh_sg_filter_order}} {Order of Savitzky-Golay filter. Defaults to \code{2}.}
#' }
#' @param x vector with x coordinates in \emph{degrees of visual angle}
#' @param y vector with y coordinates in \emph{degrees of visual angle}
#' @param trial vector with trial index
#' @param sample_rate sample rate in Hz
#' @param options List with method specific options, see Details.
#'
#' @return \code{data.frame} with columns \code{x}, \code{y}, and \code{amp}
#' @importFrom dplyr %>% group_by ungroup mutate lag select
#' @export
#' @seealso filter_via_savitzky_golay
#'
#' @examples
#' diff_nh(rnorm(1000), rnorm(1000), rep(1, 1000), 250, list("nh_sg_filter_order" = 2))
diff_nh <- function(x, y,  trial, sample_rate, options=NULL){
  # extracting filter order
  sg_filter_order <- option_or_default(options, "nh_sg_filter_order", 2)

  # computing frame time step 
  delta_t_s <- 1 / sample_rate
  
  # --- differentiate (compute velocity or acceleration) and filter
  data.frame(trial = trial,
             x = x,
             y = y) %>%

    # compute velocity and acceleration for each trial
    dplyr::group_by(.data$trial) %>%
    dplyr::mutate(x = (.data$x - dplyr::lag(x)) / delta_t_s,
                  y = (.data$y - dplyr::lag(y)) / delta_t_s,
                  amp = sqrt(.data$x^2 + .data$y^2)) %>%
    
    # filter
    dplyr::mutate(x = filter_via_savitzky_golay(.data$x, sg_filter_order),
                  y = filter_via_savitzky_golay(.data$y, sg_filter_order),
                  amp = filter_via_savitzky_golay(.data$amp, sg_filter_order)) %>%
    
    # dropping trial column
    dplyr::ungroup() %>%
    dplyr::select(-c("trial"))
}
