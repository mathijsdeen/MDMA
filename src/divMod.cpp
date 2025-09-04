#include <Rcpp.h>
using namespace Rcpp;

//' @title divMod
//' @description Retrieve quotient and remainder from Euclidian division.
//'
//' `r lifecycle::badge("stable")`
//' @param x numeric value or vector
//' @param d numeric value or vector
//' @param shortForm boolean (defaults to \code{TRUE}), indicating whether the output should contain \code{x} and \code{d}
//'
//' @return \code{divMod} returns an object of class \code{divMod} (which is essentially a \code{data.frame}). It consists of the following values/variables:
//' \item{quotient}{The quotient of the integer division \code{x}/\code{n}}
//' \item{remainder}{The remainder of the division above}
//' Dependent upon the argument \code{shortForm}, the following values are present:
//' \item{x}{The argument \code{x}}
//' \item{d}{The argument \code{d}. Returning \code{x} and \code{d} is especially useful with the use of recycling (see example below).}
//' @details \code{divMod} is equivalent to Python's \code{divmod}, but extends it with support for vector recycling.
//' @author Mathijs Deen
//' @export
//' @examples
//' divMod(x = 15, d = 4)
//' divMod(x = c(15,23,42,58), d = c(4,7), shortForm = FALSE) #recycling
//'
//' renderTime <- function(milliseconds) {
//'   out <- list("hours"=NA, "minutes"=NA, "seconds"=NA, "milliseconds"=NA)
//'   out[c("seconds","milliseconds")] <- divMod(milliseconds,1000)
//'   out[c("minutes","seconds")] <- divMod(out$seconds,60)
//'   out[c("hours","minutes")] <- divMod(out$minutes, 60)
//'   with(out, sprintf("%02d:%02d:%02d.%03d", hours,minutes,seconds,milliseconds))
//'   }
//'
//' renderTime(1000)
//' renderTime(68000)
//' renderTime(3683121)
//'
// [[Rcpp::export]]
DataFrame divMod(NumericVector x, NumericVector d, bool shortForm = true) {
  if(is_true(any(d == 0))) stop("d must not contain 0.");
  int nx = x.size(), nd = d.size();

  int n = std::max(nx, nd);
  if((n % nx) || (n % nd))
    warning("longer object length is not a multiple of shorter object length");

  IntegerVector q(n);
  NumericVector r(n), xout(n), dout(n);

  for(int i = 0; i < n; ++i) {
    double xi = x[i % nx];
    double di = d[i % nd];

    if(NumericVector::is_na(xi) || NumericVector::is_na(di)) {
      q[i] = NA_INTEGER;
      r[i] = NA_REAL;
      continue;
    }

    xout[i] = xi;
    dout[i] = di;
    q[i] = floor(xi / di);
    r[i] = xi - di * q[i];
  }

  DataFrame outFrame = (shortForm) ? DataFrame::create(_["quotient"] = q,
                                                       _["remainder"] = r)
                                   : DataFrame::create(_["x"] = xout,
                                                       _["d"] = dout,
                                                       _["quotient"] = q,
                                                       _["remainder"] = r);
  outFrame.attr("class") = CharacterVector::create("divMod", "data.frame");
  return outFrame;
}
