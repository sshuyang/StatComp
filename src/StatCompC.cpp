#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler
//' @description A Gibbs sampler using Rcpp in Chapter 9 exercise 9.8
//' @param N the number of samples
//' @param thin the number of between-sample random numbers
//' @param a the parameter of bivariate density
//' @param b the parameter of bivariate density
//' @param n the number of experiments
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' rnC <- gibbsC(100,10,2,1,10)
//' par(mfrow=c(2,1));
//' plot(rnC[,1],type='l')
//' plot(rnC[,2],type='l')
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix gibbsC(int N, int thin, int a, int b, int n) {
  NumericMatrix mat(N, 2);
  double x = 5, y = 0.5;
  for(int i = 0; i < N; i++) {
    for(int j = 0; j < thin; j++) {
      x = rbinom(1, n, y )[0];
      y = rbeta(1, x+a,n-x+b)[0];
    }
    mat(i, 0) = x;
    mat(i, 1) = y;
  }
  return(mat);
}
