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
  // maximal half span of window, so it goes from imiddle +- half_span
  int max_half_span = (time_window_in_samples - 1) / 2;
  
  // marking out finite values of x
  LogicalVector x_is_finite = is_finite(x);

  // Allocate velocity vector
  NumericVector v(x.size(), NA_REAL);
  
  for(int imiddle = 1; imiddle < x.size() - 1; imiddle++){
  
    // adding weighted difference and each span
    float total_diff = 0;
    float total_weight = 0;
    for(int half_span = max_half_span; half_span > 0; half_span--){
      int istart = imiddle - half_span;
      int iend = imiddle + half_span;
      
      if ((istart >= 0) &&                         // start must not be outside of the vector
          (iend < x.size()) &&                     // end must not be outside of the vector
          (trial[istart] == trial[imiddle]) &&     // start is in the same trial as the middle
          (trial[imiddle] == trial[iend]) &&       // end is in the same trial as the middle
          (x_is_finite[istart]) &&                 // start sample is a valid value
          (x_is_finite[iend])) {                   // end sample is a valid value
        total_diff += (x[iend] - x[istart]);
        total_weight += half_span * 2;
      }
    }
    
    // compute velocity if at least one pair of samples was usable
    if (total_weight > 0) {
      v[imiddle] = total_diff / (total_weight * delta_t);
    }
  }

  // return v
  return v;
}
