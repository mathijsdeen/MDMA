#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name = ".meanCenter_Matrix")]]
NumericMatrix mC_matrix(NumericMatrix x) {
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

// [[Rcpp::export(name = ".meanCenter_Vector")]]
NumericVector mC_vector(NumericVector x) {
  NumericVector out = clone(x);
  double s = 0.0;
  R_xlen_t cnt = 0;
  const R_xlen_t n = out.size();

  for(R_xlen_t i = 0; i < n; ++i) {
    double v = out[i];
    if(!NumericVector::is_na(v)) {
      s += v;
      ++cnt;
    }
  }

  const double mu = (cnt > 0) ? (s / cnt) : R_NaN;

  double* p = out.begin();
  for (R_xlen_t i = 0; i < n; ++i) p[i] -= mu;

  return out;
}
