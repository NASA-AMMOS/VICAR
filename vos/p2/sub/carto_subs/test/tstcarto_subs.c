#include <stdio.h>
#include <assert.h>

#include "cartoSortUtils.h"

int main(int argc, char* argv[]) {
  printf("Running carto_subs tests ...\n");

  int n = 5;
  float nums[5] = {3.14, -42.0, 17.0, 186000, 42.0};
  int indices[5] = {0, 0, 0, 0, 0};
  int i;
  
  printf("*** Module cartoSortUtils ***\n");

  printf("    getSelectionSortIndices ...\n");
  getSelectionSortIndices(nums, indices, n, 4 /* float */);
  for (i = 0; i < n-1; ++i)
    assert(nums[indices[i]] < nums[indices[i+1]]);

  printf("... carto_subs tests done.\n");

  return 0;
}
