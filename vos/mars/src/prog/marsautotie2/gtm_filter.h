// This files (.h and .cc) implements the GTM filtering presented in the
// following paper:
// "A robust Graph Transformation Matching for non-rigid registration"
// Image and Vision Computing 27(7):897-910. June 2009 


#include <vector>
#include "mars_tiepoints.h"

using namespace std; 

// Computes all the relative distance, in pixel, between the left-image points
// of a tie points list
vector<vector<float> > compute_tiepoints_distances1(
                                               const vector<TiePoint> tieVect);

// Computes all the relative distance, in pixel, between the right-image points
// of a tie points list
vector<vector<float> > compute_tiepoints_distances2(
                                               const vector<TiePoint> tieVect);

// Return the median value of a distance matrix (2D array containing the 
// relative distance between points)
float compute_median_distance(const vector<vector<float> > distances);

// Compute the matrix O, per notation of reference paper.
vector<vector<int> > compute_matrix_O(const vector<vector<float> > distances, 
                                      const float median, const int K);

// Compute the matrix I, per notation of reference paper.
vector<vector<int> > compute_matrix_I(const vector<vector<int> > O, int K);

// Compute the vector C, per notation of reference paper.
vector<int> compute_array_C(const vector<vector<int> > O, int K);

// Sort a vector of value and return the vector of indexes of the sorted
// value. The vector is not actually sorted.
template <typename T1, typename T2> 
void sort_indexes(const vector<T1> &dists, vector<T2> &index) ;

// Main function applying the GTM filtering
void gtm_filter(vector<TiePoint> &tieVect, int K);
