#include <map>
#include <vector>


  // Old GCC (< 4.6) doesn't support range-based for loop.
#define FOR_ITERATOR(IT, X) \
  for (auto IT = X.begin(); IT != X.end(); ++IT)

  template <typename T, typename U>
  inline std::vector<T> keys(const std::map<T, U> &input)
  {
    std::vector<T> output;

    FOR_ITERATOR(it, input)
    {
      output.push_back(it->first);
    }

    return output;
  }

