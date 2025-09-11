#include <Rcpp.h>
using namespace Rcpp;

IntegerVector firstRow(int n);

//' @title Latin square
//' @description Create a latin square of size \eqn{n \times n}.
//'
//' `r lifecycle::badge("stable")`
//' @param n positive integer
//' @return \code{latinSquare} returns a matrix of size \eqn{n \times n} that is a latin square.
//' @details The latin square that is produced is a (generalized) latin square that is also balanced for first order carryover effects.
//' @author Mathijs Deen
//' @export
//' @examples
//' # 5 x 5 latin square
//' latinSquare(5)
//' @references
//' * Williams, E.J. (1949). Experimental designs balanced for the estimation of residual effects of treatments. *Australian Journal of Scientific Research 2, 2*, 149-168. https://doi.org/10.1071/CH9490149
// [[Rcpp::export]]
IntegerMatrix latinSquare(const R_len_t n) {
  IntegerMatrix L(n * (1 + (n % 2)*(n != 1)),n);
  IntegerVector r1 = firstRow(n);
  for(R_len_t j = 0; j < n; ++j) {
    int v = r1[j];
    for(R_len_t i = 0; i < n; ++i) {
      L(i, j) = ((v + i - 1) % n) + 1;
    }
  }
  if(((n % 2) == 1) & (n != 1)) {
    for(R_len_t i = n; i < 2*n; ++i) {
      L(i, _) = rev(L(i - n, _));
    }
  }
  return L;
}

// [[Rcpp::export]]
IntegerVector firstRow(R_len_t n) {
  if(n < 1) stop("n must be >= 1");
  IntegerVector out(n);
  out[0] = 1;
  if(n == 1) return out;
  out[1] = 2;
  if(n == 2) return out;

  int lo = 3, hi = n, k = 2;
  while (k < n) {
    if (k < n) out[k++] = hi--;
    if (k < n) out[k++] = lo++;
  }
  return out;
}


