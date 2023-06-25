// This files (.h and .cc) implements the GTM filtering presented in the
// following paper:
// "A robust Graph Transformation Matching for non-rigid registration"
// Image and Vision Computing 27(7):897-910. June 2009 
#include <set>
#include <algorithm>
#include <numeric>
#include "gtm_filter.h"

using namespace std; 

// Computes all the relative distance, in pixel, between the left-image points
// of a tie points list
vector<vector<float> > compute_tiepoints_distances1(const vector<TiePoint> tieVect) 
{
   long int i, j, nb;
   float d;

   // Get the number of points to cross compare for relative distance
   nb = tieVect.size();

   // Initialize a square matrix that will store the relative distances between
   // points. Distance is expressed in pixel   
   vector<vector<float> > distances = vector<vector<float> >(nb, vector<float>(nb));

   for (int i=0; i<(nb-1); i++) {
      // Initialize the diagonal matrix element Set them to zero
      distances[i][i]=0.0;
      // Compute distance with the other points
      for (int j=i+1; j<nb; j++) {
         d = sqrt(pow(tieVect[i].left_sample - tieVect[j].left_sample,2) +
                  pow(tieVect[i].left_line - tieVect[j].left_line,2));
         distances[i][j] = d;
         distances[j][i] = d;
      }
   }

   return distances;
}





// Computes all the relative distance, in pixel, between the right-image points
// of a tie points list
vector<vector<float> > compute_tiepoints_distances2(const vector<TiePoint> tieVect) 
{
   long int i, j, nb;
   float d;

   // Get the number of points to cross compare for relative distance
   nb = tieVect.size();

   // Initialize a square matrix that will store the relative distances between
   // points. Distance is expressed in pixel   
   vector<vector<float> > distances = vector<vector<float> >(nb, vector<float>(nb));

   for (int i=0; i<(nb-1); i++) {
      // Initialize the diagonal matrix element Set them to zero
      distances[i][i]=0.0;
      // Compute distance with the other points
      for (int j=i+1; j<nb; j++) {
         d = sqrt(pow(tieVect[i].corrected_sample - tieVect[j].corrected_sample,2) +
                  pow(tieVect[i].corrected_line - tieVect[j].corrected_line,2));
         distances[i][j] = d;
         distances[j][i] = d;
      }
   }

   return distances;
}




// Return the median value of a distance matrix (2D array containing the 
// relative distance between points)
// Note that only the upper (or lower) triangle part of the 2D square matrix
// is needed. Input is supposedly a square 2D matrix containaing the relative
// distance between points:
// distance[i][i] = 0 (distance of the point with himself)
// distance[i][j] = distace[j][i]
float compute_median_distance(const vector<vector<float> > distances) 
{
   long int i, nb;

   nb = distances.size();

   // Reformat from 2D vector to 1D vector
   vector<float> distMed;
   distMed.reserve(nb * (nb-1) / 2); // Number of pairwise combination 
   for (i=0; i<nb ; i++)
      distMed.insert(distMed.end(), distances[i].begin()+(i+1), distances[i].end());

   // Sort in ascending order
   std::sort(distMed.begin(), distMed.end());

   // Return the value in the middle of the vector, i.e., the median
   return distMed[distMed.size()/2];

}




// Compute the matrix O, per notation of reference paper.
// Each line i of O contains the indexes of points ordered in closest to 
// farthest distance to point i. O[i][0] contains index of closest point to i, 
// O[i][1] second-clsoest point to i, etc... Farthest point is less or equal 
// than median distance.
// If less than K points are closest than median, the point is deemed to 
// isolated and removed (first value set to -1)
vector<vector<int> > compute_matrix_O(const vector<vector<float> > distances, 
                                      const float median, const int K)
{
   int i, j, nb, ind;

   // Get the number of distances measured for each point
   nb = distances.size();

   // Intialize the 2D array that will contain the indexes of the nearest points
   vector<vector<int> > O;
   O.resize(nb);

   vector<int> indexes(nb);
   
   for (i=0; i<nb; i++) {
      // Get the indexes of the sorted distances. We want to get a ordered list
      // of points from closest to farthest
      sort_indexes(distances[i], indexes);

      ind = 0;
      for (j=0; j<nb; j++) {
         if (distances[i][indexes[j]] > median)
            break;
         ind++;
      }

      if (ind > K)
         // begin()+1 for 1st element which is 0 (distance with self)
         O[i].assign(indexes.begin()+1, indexes.begin()+ind); 
      else
         O[i].push_back(-1); //point removed

   } 

   return O;

}




// Compute the matrix I, per notation of reference paper.
// I[i] contains a vector of points for which point i is one of their K closest
// neighboor
vector<vector<int> > compute_matrix_I(const vector<vector<int> > O, int K)
{
   int i, j, k, nb;

   // Get the number of points
   nb = O.size();

   // Initialize the I container
   vector<vector<int> > I(nb); 

   // Loop over all the points and gather all points indexes for which the 
   // current point is part of their K nearest neighboor
   for (i=0; i<nb; i++) {
      for (j=0; j<nb; j++) {
         if (i==j)
            continue;
 
         if (O[j][0] == -1)
            continue;  
         
         for (k=0; k<K; k++) {
            if (O[j][k] == i){
               I[i].push_back(j);
               break;
            }
         }
      }
   }

   return I;
}


// Creating the C array (per paper notation)
// Contains, for each point, the index in O of the next available nearest
// point beyond the K initial firsts.
vector<int> compute_array_C(const vector<vector<int> > O, int K)
{
   int i, nb;

   nb = O.size();
 
   vector<int> C;
   C.assign(nb, K); // Default value

   for (i=0; i<nb; i++) {
      if (O[i][0] == -1)
         C[i] = -1; //No next point available
   }

   return C;
}



// Sorting function. The input vector is actually not sorted, only the indexes
// of the sorted values are returned
template <typename T1, typename T2>
void sort_indexes(const vector<T1> &dists, vector<T2> &indexes) {

   // initialize original index locations
   std::iota(indexes.begin(), indexes.end(), 0);
  
   // sort indexes based on comparing values in v
   std::sort(indexes.begin(), indexes.end(),
   [&dists](size_t i1, size_t i2) {return dists[i1] < dists[i2];});
 
}


// GTM filtering main.
// Takes a vector of tiepoints and a graph size (K) and will remove outliers
// from the input vector
void gtm_filter(vector<TiePoint> &tieVect, int K) 
{
   long int i, j, k, nb, index, end;
   float median1, median2, d, maxd;
   const size_t msgLen = 150;
   char msg[msgLen];

   // If K <= 1, nothing to do, return
   if (K <= 1) {
      snprintf(msg, msgLen, "K <= 1 for GTM filtering. Nothing to do...");
      zvmessage(msg, "");
      return;
   }

   // Get number of tiepoints
   nb = tieVect.size();

   // If number of points is less or equals than K, nothing to filter
   if (nb <= K) {
      snprintf(msg, msgLen, "Number of tiepoints <= K. Nothing to do...");
      zvmessage(msg, "");
      return;
   }


   // Initialize a distance matrices which will record the inter pixel distance 
   // between points in the left and right images
   vector<vector<float> > dists1, dists2;


   // Compute the relative inter-distance between points of the first image
   // and second image
   dists1 = compute_tiepoints_distances1(tieVect);
   dists2 = compute_tiepoints_distances2(tieVect);

   // Get the median distance
   median1 = compute_median_distance(dists1);
   median2 = compute_median_distance(dists2);


   // Creating the O matrice (per paper notation).
   // That is for each point (each line of O), each element of that line will be
   // the point index ordered from closest to farthest, up to median dist.
   // This is done from three steps:
   // - Order each line by the distance and keep the indexes of the sorting
   // - Locate the index at which distance is greater than median
   // - Replace the N first distances by their point index
   // Note, if a point has less than K neighboor closer than median, consider 
   // the point outlier. In practice there is no reason why that should be the
   // case, but for now it's easier to implement this way. 
   // TO DO: deal more gently with these last cases
   
   vector<vector<int> > O1, O2;

   O1 = compute_matrix_O(dists1, median1, K);
   O2 = compute_matrix_O(dists2, median2, K);

   // At this stage, dists matrices are not needed anymore. Free memory
   vector<vector<float> >().swap(dists1);
   vector<vector<float> >().swap(dists2);



   // If a point first element is -1, that means that this points is largely 
   // isolated from the rest of the points (distance to closest point is larger
   // than median distance, or it has less than K neighboor closer to median 
   // distance). In that case it cannot be part of the graph structure 
   // verification. We need to exclude its corresponding point in the other 
   // image. Normally that should be the case, but it's not guaranteed. 
   // WARNING, doing so, we potentially remove valid, isolated, tiepoints,
   // TO DO: handle theses cases
   for (i=0; i<nb; i++) {
      if (O1[i][0] == -1 || O2[i][0] == -1) {
         O1[i][0] = -1;
         O2[i][0] = -1;
      }
   }



   // Creating the I arrays (per paper notation)
   // That is, for each point, list the points indexes for which the current 
   // point is part of their K nearest neighboors
   vector<vector<int> > I1, I2;
   I1 = compute_matrix_I(O1, K);
   I2 = compute_matrix_I(O2, K);


   // Creating the C array (per paper notation)
   // Contains, for each point, the index in O of the next available nearest
   // point beyond the K initial firsts.
   vector<int> C1, C2;
   C1 = compute_array_C(O1, K);
   C2 = compute_array_C(O2, K);




   // Now that the data necessary for the filtering has been created and 
   // formated, the actual filtering can take place

   // Variable to avoid infinite loop -  shouldn't happen but for security.
   // Max number of loop is equals to number of points
   int iterLeft = nb;
   int maxDiff, maxIndex;
   set<int> set1, set2;
   vector<int> v;
   vector<int>::iterator it;


   while (iterLeft) {

      maxDiff = -1;
      maxIndex = 0;

      // Locate the next "grossest" outlier
      for (i=0; i<nb; i++) {

         // Case if point is already invalid/removed. Move on to next point.
         if (O1[i][0] == -1)  
            continue;

         // Fill the sets of image1 that will be compared with the sets of 
         // image2
         set1.insert(O1[i].begin(), O1[i].begin()+K);
         set1.insert(I1[i].begin(), I1[i].end());

         set2.insert(O2[i].begin(), O2[i].begin()+K);
         set2.insert(I2[i].begin(), I2[i].end());

         // Get the complement of the two sets
         v.resize(set1.size() + set2.size());
         it = std::set_symmetric_difference (set1.begin(), set1.end(),
                                      set2.begin(), set2.end(), v.begin());
         
         // Get the number of element in the sets' complement
         // Keep track of the largest one (largest outlier)
         int diff = it-v.begin();
         if (maxDiff < diff) {
            maxDiff = diff;
            maxIndex = i;
         }

         set1.clear();
         set2.clear();
         v.clear();
      }


      // If maximum difference is 0, then no more outliers, we're done.
      if (maxDiff == 0) 
         break;


      // The grossest outlier is found and is recorded in maxIndex
      // Now we are removing all trace of that outlier in O and I and
      // update them accordingly along with C

      // remove all occurence of outlier in Is
      for (i=0; i<nb; i++) {
         for(j=0; j<I1[i].size(); j++) {
            if (I1[i][j] == maxIndex){
               I1[i].erase(I1[i].begin()+j);
               break;
            }
         }
      }

      // Do the same in other I
      for (i=0; i<nb; i++) {
         for(j=0; j<I2[i].size(); j++) {
            if (I2[i][j] == maxIndex) {
               I2[i].erase(I2[i].begin()+j);
               break;
            }
         }
      }

      // Remove the outliers from the Os matrices 
      // We simply sets first element of the line to -1
      O1[maxIndex][0] = -1;
      O2[maxIndex][0] = -1;

      // Remove occurences of the outlier in the other lines of O. The search is
      // limited to the first K elements of each line.
      // If outlier is found, it is replaced with the next available point in 
      // the line using the C matrix (and verifying that it is not a
      // point that has already been flagged as outlier, i.e., -1)
      for (i=0; i<nb; i++) {

         if (O1[i][0] == -1) // already a deactivated point, move on...
            continue;

         for(j=0; j<K; j++) {
            if (O1[i][j] == maxIndex) {
               
               // Get the next point available

               // Case no point are available. Then valid neighborood falls 
               // below K. Remove point. 
               // TO DO: This is too aggressive. May be set to -1 and when 
               // computing complement account for it on next loop
               if (C1[i] == -1) {
                  O1[i][0] = -1;
                  O2[i][0] = -1;
               }
               // Case where there are points available for replacement of the
               // outlier. Verify that the point is valid, meaning it has not
               // previously been flagged as outlier (O first element = -1)
               // Otherwise, move to next available point
               else {
                  int p = C1[i];                // get index of next avail point
                  it = O1[i].begin() + p;
                  while (it != O1[i].end()) {  
                     if (O1[*it][0] == -1) {    // check that point is valid
                        it++;                   // otherwise move to next one
                        p++;
                     }
                     else
                        break;
                  }

                  if (it == O1[i].end()) {      // if end of avail point reached
                     O1[i][0] = -1;             // flag current point as invalid
                     O2[i][0] = -1;
                     C1[i] = -1;
                  }
                  else {
                     O1[i][j] = *it;            // otherwise, replace outlier
                     I1[*it].push_back(i);      // with the valid point
                     C1[i] = ++p;
                  }                    
               }
               break;
            }
         }
      }


      // Same thing for the other image
      for (i=0; i<nb; i++) {

         if (O2[i][0] == -1) //invalid point, move on...
            continue;

         for(j=0; j<K; j++) {
            if (O2[i][j] == maxIndex) {
               // Get the next point available

               // Case no point are available. Then valid neighborood falls 
               // below K. Remove point. 
               // TO DO: This is too aggressive. May be set to -1 and when 
               // computing complement account for it on next loop
               if (C2[i] == -1) {
                  O1[i][0] = -1;
                  O2[i][0] = -1;
               }
               // Case where there are points available for replacement of the
               // outlier. Verify that the point is valid, meaning it has not
               // previously been flagged as outlier (O first element = -1)
               // Otherwise, move to next available point
               else {
                  int p = C2[i];
                  it = O2[i].begin() + p;
                  while (it != O2[i].end()) {
                     if (O2[*it][0] == -1) {
                        it++;
                        p++;
                     }
                     else
                        break;
                  }

                  if (it == O2[i].end()) {
                     O1[i][0] = -1;
                     O2[i][0] = -1;
                     C2[i] = -1;
                  }
                  else {
                     O2[i][j] = *it;
                     I2[*it].push_back(i);
                     C2[i] = ++p;
                  }                    
               }
               break;
            }
         }
      }

      iterLeft--; // Max number of iteration left

   }


   // Remove outliers from tiepoint list
   for (i=nb-1; i>=0; i--)
      if (O1[i][0] == -1)
         tieVect.erase(tieVect.begin()+i);

}


