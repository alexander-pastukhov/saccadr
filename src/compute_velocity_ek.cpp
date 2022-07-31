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
NumericVector compute_velocity_via_ek(NumericVector x, IntegerVector trial, int time_window_in_samples, float delta_t){
  // maximal half span of window, so it goes from imiddle +- half_span
  int max_half_span = (time_window_in_samples - 1) / 2;

  // Compute total weight for velocity normalization.
  NumericVector weight(max_half_span, 0.0);
  int span_sum = 0;
  for(int span = 1; span <= max_half_span; span ++){
    span_sum += span;
    weight[span - 1] = span_sum * 2 * delta_t;
  }

  // Allocate velocity vector
  NumericVector v(x.size(), 0.0);

  for(int imiddle = 1; imiddle < x.size() - 1; imiddle++){
    // finding applicable filter width starting with the widest one
    int half_span = max_half_span;
    int istart = imiddle - half_span;
    int iend = imiddle + half_span;

    // shrink until window is valid or it is zero
    while(((istart < 0) ||                      // start must not be outside of the vector
           (iend >= x.size()) ||                // end must not be outside of the vector
           (trial[istart] != trial[imiddle]) || // start is in the same trial as the middle
           (trial[imiddle] != trial[iend])) &&  // end is in the same trial as the middle
           (half_span > 0)) {                   // half span of zero means we cannot apply the filter
      half_span --;
      istart = imiddle - half_span;
      iend  = imiddle + half_span;
    }

    if (half_span > 0){
      float current_v = 0;

      // negative side
      for(int i = istart; i < imiddle; i++) current_v -= x[i];

      // positive side
      for(int i = imiddle + 1; i <= iend; i++) current_v += x[i];

      // normalize
      v[imiddle] = current_v / weight[half_span - 1];
    }
  }

  // return v
  return v;
}
