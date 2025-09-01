#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix mC1_matrix(NumericMatrix x) {
  const R_xlen_t nrow = x.nrow();
  const R_xlen_t ncol = x.ncol();
  NumericMatrix out = clone(x);
  NumericVector center = colMeans(x, true);

  for(R_xlen_t j = 0; j < ncol; ++j) {
    double* col = &out(0, j);
    const double c = center[j];
    for(R_xlen_t i = 0; i < nrow; ++i) col[i] -= c;
  }
  return out;
}

// [[Rcpp::export]]
NumericVector mC1_vector(NumericVector x) {
  NumericVector out = clone(x);
  const double mu = mean(out, true);

  double* p = out.begint();
  const R_xlen_t n = out.size();
  for (R_xlen_t i = 0; i < n; ++i) p[i] -= mu;

  return out;
}
