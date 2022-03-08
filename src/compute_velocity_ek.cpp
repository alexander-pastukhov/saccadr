
#include <Rcpp.h>
using namespace Rcpp;


//' Compute velocity via Engbert & Kliegl (2003) algorithm.
//'
//' @description Compute velocity via Engbert & Kliegl (2003) algorithm.
//' See formula #1 in the manuscript.
//' 
//' @param x Vector of coordinates
//' @param trial Vector with trial labels, so that velocity is computed only within trials.
//' @param vel_window_in_samples Width of velocity computation in samples.
//' @param frame_duration Duration of a single frame (1 / sampling rate).
//' 
//' @return Velocity vector
// [[Rcpp::export]]
NumericVector compute_velocity_ek(NumericVector x, IntegerVector trial, int vel_window_in_samples, float frame_duration){
  int window_middle = vel_window_in_samples / 2;
  // Compute total weight for velocity normalization.
  // It is equal to 6 in the original formula #1 because the window is 5 samples wide.
  int total_width = 0;
  for(int distance = 1; distance <= vel_window_in_samples / 2; distance ++) {
    total_width += distance * 2;
  }
  
  // Create summation weights
  IntegerVector indexes = seq(0, vel_window_in_samples - 1);
  NumericVector weights(vel_window_in_samples, 0.0);
  weights[indexes < window_middle] = -1.0;
  weights[indexes > window_middle] = 1.0;
  
  // Allocate velocity vector
  NumericVector v(x.size(), NA_REAL);
  
  // Loop over the all samples, respecting trial boundaries
  for(int iend = 0; iend < x.size(); iend++){
    int istart = iend - vel_window_in_samples + 1;
    
    // computing speed only within a trial
    if ((istart > 0) && (trial[iend] == trial[istart])) {
      v[istart + window_middle] = sum(weights * x[seq(istart, iend)]);
    }
  }
  
  // return normalized v
  return v / (total_width * frame_duration);
}
