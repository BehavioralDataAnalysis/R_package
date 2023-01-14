
#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
std::vector<int> n_smallest_indices(const std::vector<int>& vec, int n) {
  std::vector<int> indices(vec.size());
  std::iota(indices.begin(), indices.end(), 0);

  std::sort(indices.begin(), indices.end(), [&vec](int i, int j) {
    return vec[i] < vec[j];
  });

  std::vector<int> smallest_indices(indices.begin(), indices.begin() + n);
  return smallest_indices;
}