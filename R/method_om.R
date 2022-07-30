#' Extract saccades using an algorithm proposed by Otero-Millan et al. (2014) \doi{10.1167/14.2.18.}
#' 
#' @details Method options, please refer to Otero-Millan et al. (2014) for details on parameters and the rationale for default values.
#' \itemize{
#' \item{\code{om_minimal_inter_peak_time_ms}} {Minimal inter-peak interval in milliseconds. Defaults to \code{30}.}
#' \item{\code{om_maximal_peaks_per_second}} {Maximal allowed number of peaks per second. Defaults to \code{5}.}
#' \item{\code{om_velocity_threshold_deg_per_sec}} {Threshold saccade velocity in °/s.  Defaults to \code{3}.}
#' \item{\code{om_pca_variance_threshold}} {Minimal variance explained by retained rotated components. Defaults to \code{0.05}.}
#' }
#' @param x Gaze x coordinate, _arbitrary units_ as threshold velocity is computed in units of standard deviation.
#' @param y Gaze x coordinate, _arbitrary units_ as threshold velocity is computed in units of standard deviation.
#' @param vel Velocity \code{data.frame} with columns \code{x}, \code{y}, \code{amp}.
#' @param acc Acceleration \code{data.frame} with columns \code{x}, \code{y}, \code{amp}.
#' @param sample_rate Sample rate in Hz.
#' @param trial Trial id, so that trial borders are respected when computing velocity and saccades.
#' @param options Names list with method options. See \emph{details} for further information.
#' @return logical vector marking samples that belong to saccades
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom dplyr group_by mutate lag lead pull filter select arrange
#' @importFrom tidyr nest unnest
#' @importFrom stats prcomp kmeans
#' @importFrom cluster silhouette
#' @importFrom rlang .data
#' @seealso \code{\link{extract_saccades}}
#' @examples
#' # Do not run this function directly, use extract_saccades() instead
method_om <- function(x,
                      y,
                      vel,
                      acc,
                      sample_rate,
                      trial,
                      options){
  # get options
  minimal_inter_peak_time_ms <- option_or_default(options, "om_minimal_inter_peak_time_ms", 30)
  maximal_peaks_per_second <- option_or_default(options, "om_maximal_peaks_per_second", 5)
  velocity_threshold_deg_per_sec <- option_or_default(options, "om_velocity_threshold_deg_per_sec", 3)
  pca_variance_threshold <-  option_or_default(options, "om_pca_variance_threshold", 0.05)
  
  
  # --- identify local peaks within each trial
  peaks <-
    data.frame(trial = trial,
               vel = vel[['amp']],
               irow = 1:nrow(vel)) %>%
    
    # peak computation must be trial border aware;
    dplyr::group_by(.data$trial) %>%
    
    # create within trial time variable
    dplyr::mutate(t = (0:(n()-1)) * 1000 / sample_rate) %>%
    
    
    # maximum is a sample that is above velocity_threshold_deg_per_sec and is larger than immediate surround
    dplyr::mutate(IsMaxima = .data$vel > velocity_threshold_deg_per_sec &
                             .data$vel > dplyr::lag(vel, n = 1L) & 
                             .data$vel > dplyr::lead(vel, n = 1L)) %>%
    # retain only peaks
    dplyr::filter(.data$IsMaxima) %>%
    dplyr::select(-c("IsMaxima")) %>%
    
    # nest them, so we get a separate table for each trial
    tidyr::nest()
  
  # --- prune local peaks so that they are
  # 1) separated by at least `minimal_inter_peak_time_ms` ms
  # 2) no more than `maximal_peaks_per_second`
  for(itrial in 1:nrow(peaks)){
    # get peak order from highest to lowest
    ipeak <- 1:nrow(peaks$data[[itrial]])
    ipeak <- ipeak[order(peaks$data[[itrial]]$vel, decreasing = TRUE)]
    
    # highest peak with the trial is always selected
    iselected <- ipeak[1]

    # the rest have to fight it out
    for(ip in ipeak[2:length(ipeak)]){
      # compute  time intervals to already selected peaks
      time_to_selected <- peaks$data[[itrial]]$t[iselected] - peaks$data[[itrial]]$t[ip]
      
      # 1) anything within `minimal_inter_peak_time_ms` is no good
      if (min(abs(time_to_selected)) < minimal_inter_peak_time_ms) next
      
      # 2) do we have already too many within 1 second?
      peaks_within_one_second <- sum(abs(time_to_selected) < 0.5)
      if (peaks_within_one_second >= maximal_peaks_per_second) next
      
      iselected <- c(iselected, ip)
    }
    
    # retain only selected peaks
    peaks$data[[itrial]] <- peaks$data[[itrial]][iselected, ]
  }
  
  # back to single simple table
  selected_peaks <- 
    peaks %>%
    tidyr::unnest(cols = c("data")) %>%
    dplyr::arrange(.data$irow)
  
  
  # --- mark out sample that belong to peaks by starting at each peak
  # and selecting all adjacent samples above `velocity_threshold_deg_per_sec`
  # (will be optimized via Rcpp)
  is_peak <- rep(FALSE, nrow(vel))
  # forward sweep
  for(ip in 1:nrow(selected_peaks)) {
    isample <- selected_peaks$irow[ip]
    while ((isample < nrow(vel)) &  # within range (yes, R will return NA but let's be thorough)
           (trial[isample] == selected_peaks$trial[ip]) & # same trial
           (is.finite(vel$amp[isample])) & # valid velocity value
           (vel$amp[isample] > velocity_threshold_deg_per_sec)) { # still above velocity threshold
      is_peak[isample] <- TRUE
      isample <- isample + 1
    }
  }
  # backwards sweep
  for(ip in nrow(selected_peaks):1) {
    isample <- selected_peaks$irow[ip] - 1
    while ((isample > 0) &  # within range (yes, R will return NULL for index 0 but let's be thorough)
           (!is_peak[isample]) & # have not been already marked out by the forward sweep
           (is.finite(vel$amp[isample])) & # valid velocity value
           (trial[isample] == selected_peaks$trial[ip]) & # same trial
           (vel$amp[isample] > velocity_threshold_deg_per_sec)) { # still above velocity threshold
      is_peak[isample] <- TRUE
      isample <- isample - 1
    }
  }
  
  ## --- getting potential saccades from consecutive periods marked as peaks
  peak_samples <- rle(is_peak)
  potential_saccades <-
    data.frame(IsPeak = peak_samples$values,
               Length = peak_samples$lengths) %>%
    
    # compute onset and offset for each period
    dplyr::mutate(Onset = cumsum(c(1, .data$Length[1:(n()-1)])),
                  Offset = .data$Onset + .data$Length - 1) %>%
    
    # drop non-peaks
    dplyr::filter(.data$IsPeak) %>%
    dplyr::select(-c("IsPeak"))
    
  # --- computing properties of potential saccades
  saccade_properties <- matrix(0, nrow = nrow(potential_saccades), ncol = 3)
  colnames(saccade_properties) <- c("zlogvel", "zlogaccstart", "zlogaccstop")
  for(iS in 1:nrow(potential_saccades)) {
    isaccade <- potential_saccades$Onset[iS]:potential_saccades$Offset[iS]
    ipeak_velocity <- which.max(vel$amp[isaccade])
    saccade_properties[iS, "zlogvel"] <- vel$amp[isaccade[ipeak_velocity]]
    saccade_properties[iS, "zlogaccstart"] <- max(acc$amp[isaccade[1:ipeak_velocity]])
    saccade_properties[iS, "zlogaccstop"] <- max(acc$amp[isaccade[ipeak_velocity:length(isaccade)]])
  }
  # logging and z-scoring them
  saccade_properties <- log(saccade_properties)
  saccade_properties <- apply(saccade_properties, MARGIN = 2, scale)
  ivalid <-  which(rowSums(is.na(saccade_properties)) == 0)
  
  # --- performing PCA
  saccade_pca <- prcomp(saccade_properties[ivalid, ])
  important_components <- 1:2 # which(summary(saccade_pca)$importance[2,]>pca_variance_threshold)
  saccade_rotated <- as.matrix(saccade_pca$x[, important_components])
  
  # --- performing cluster analysis for 2, 3, and 4 clusters
  kmeans_cluster <- list()
  silhouette_size <- c()

  for(groups_n in 2:4){
    # splitting into groups based on peak velocity
    velocity_cut <- cut(saccade_properties[ivalid, "zlogvel"], groups_n)
    # computing average component values for each group
    group_avg <- matrix(0, nrow = groups_n, ncol = length(important_components))
    for(i_cut_group in 1:length(levels(velocity_cut))){
      i_group <- velocity_cut == levels(velocity_cut)[i_cut_group]
      if (sum(i_group) == 1) {
        # special case, single entry
        group_avg[i_cut_group, ] <- saccade_rotated[i_group, ]
      } else {
        # averaging over columns
        group_avg[i_cut_group, ] <- colMeans(saccade_rotated[velocity_cut == levels(velocity_cut)[i_cut_group], ])
      }
    }
    
    # actual k-means and silhouette
    kmeans_cluster[[groups_n]] <- stats::kmeans(saccade_rotated, group_avg)
    group_silhouette <- cluster::silhouette(kmeans_cluster[[groups_n]]$cluster, stats::dist(saccade_rotated))
    silhouette_size[groups_n] <- mean(group_silhouette[, 3])
  }
  # picking clustering with the smallest silhouette
  i_smallest_silhouette <- which.min(silhouette_size)
  
  # computing average peak velocity for each group
  group_zlogvel <- c()
  for(group_label in 1:max(kmeans_cluster[[i_smallest_silhouette]]$cluster)){
    igroup <- kmeans_cluster[[i_smallest_silhouette]]$cluster == group_label
    group_zlogvel[group_label] <- mean(saccade_properties[ivalid[igroup], "zlogvel"])
  }
  
  # maximal peak velocity group is our saccades
  i_max_vel_group <- kmeans_cluster[[i_smallest_silhouette]]$cluster == which.max(group_zlogvel)
  saccades <- potential_saccades[ivalid[i_max_vel_group], ]

  # marking out samples
  is_saccade <- rep(FALSE, nrow(vel))
  for(iS in 1:nrow(saccades)) {
    is_saccade[saccades$Onset[iS] : saccades$Offset[iS]] <- TRUE
  }
  is_saccade
}
