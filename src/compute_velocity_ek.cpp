
#include <Rcpp.h>
using namespace Rcpp;


//' Compute velocity via Engbert & Kliegl (2003) algorithm.
//'
//' @description Compute velocity via Engbert & Kliegl (2003) algorithm.
//' See formula #1 in the manuscript.
//' 
//' @param x Vector of coordinates
//' @param trial Vector with trial labels, so that velocity is computed only within trials.
//' @param time_window_in_samples Width of window for velocity computation in samples.
//' @param delta_t Duration of a single frame (1 / sampling rate).
//' 
//' @return Velocity vector
//' @export
// [[Rcpp::export]]
NumericVector compute_velocity_ek(NumericVector x, IntegerVector trial, int time_window_in_samples, float delta_t){
  // Compute total weight for velocity normalization.
  // It is equal to 6 in the original formula #1 because the window is 5 samples wide.
  float distance_weight = sum(seq_len(time_window_in_samples / 2)) * 2 * delta_t;

  // Create summation weights
  int window_middle = time_window_in_samples / 2;
  IntegerVector indexes = seq(0, time_window_in_samples - 1);
  NumericVector sample_weight(time_window_in_samples, 0.0);
  sample_weight[indexes < window_middle] = -1;
  sample_weight[indexes > window_middle] =  1;
  
  // Allocate velocity vector
  NumericVector v(x.size(), NA_REAL);
  
  // Loop over the all samples, respecting trial boundaries
  for(int iend = 0; iend < x.size(); iend++){
    int istart = iend - time_window_in_samples + 1;
    
    // computing speed only within a trial
    if ((istart > 0) && (trial[iend] == trial[istart])) {
      v[istart + window_middle] = sum(sample_weight * x[seq(istart, iend)]);
    }
  }
  
  // return normalized v
  return v / distance_weight;
}
