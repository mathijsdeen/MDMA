#include <Rcpp.h>
using namespace Rcpp;

IntegerVector firstRow(int m);

// [[Rcpp::export]]
void /*IntegerMatrix*/ latinSquare(const int n) {
  IntegerMatrix L(n,n);
  L(1, _) = firstRow(n);
  if(n % 2 == 0) {
    //return 1;
  }
  //return L;
}

// [[Rcpp::export]]
IntegerVector firstRow(int m) {
  if(m < 1) stop("m must be >= 1");
  IntegerVector out(m);
  out[0] = 1;
  if(m == 1) return out;
  out[1] = 2;
  if(m == 2) return out;

  int lo = 3, hi = m, k = 2;
  while (k < m) {
    if (k < m) out[k++] = hi--;
    if (k < m) out[k++] = lo++;
  }
  return out;
}
