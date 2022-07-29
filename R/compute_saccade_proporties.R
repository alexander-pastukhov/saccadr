#' Compute saccade properties
#'
#' @return
#' @export
#'
#' @examples
compute_saccade_proporties <- function(x, y, trial, vel, acc){
  
  # computing saccades' properties
  saccades %>%
    rowwise() %>%
    dplyr::mutate(vPeak =  max(v[.data$OnsetSample[1]:.data$OffsetSample[1]]),
                  DeltaX = x[.data$OffsetSample[1]] - x[.data$OnsetSample[1]],
                  DeltaY = y[.data$OffsetSample[1]] - y[.data$OnsetSample[1]],
                  DeltaPhi = atan2(.data$DeltaY, .data$DeltaX),
                  AmpX = sign(which.max(x[.data$OnsetSample[1]:.data$OffsetSample[1]]) - which.min(x[.data$OnsetSample[1]:.data$OffsetSample[1]])) * 
                    (max(x[.data$OnsetSample[1]:.data$OffsetSample[1]]) - min(x[.data$OnsetSample[1]:.data$OffsetSample[1]])),
                  AmpY = sign(which.max(y[.data$OnsetSample[1]:.data$OffsetSample[1]]) - which.min(y[.data$OnsetSample[1]:.data$OffsetSample[1]])) *
                    (max(y[.data$OnsetSample[1]:.data$OffsetSample[1]]) - min(y[.data$OnsetSample[1]:.data$OffsetSample[1]])),
                  Amplitude = sqrt(.data$AmpX^2 + .data$AmpY^2),
                  AmpPhi = atan2(.data$AmpY, .data$AmpX)
    ) %>%
    dplyr::select(-c("IsAboveThreshold")) %>%
    dplyr::relocate(c("OnsetSample", "OffsetSample"))
}