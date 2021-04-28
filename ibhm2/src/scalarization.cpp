#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix radialScal(const NumericMatrix& x, const NumericVector& dd) {
  int rows = x.nrow();
  int cols = x.ncol();
  
  NumericMatrix d(1, cols+1, dd.begin());
  NumericMatrix result(x.rows(), 1);
  
  double d0 = d[0];
  for(int i=0; i < rows; i++){
    double sum = 0.0;
    for(int j=0; j < cols; j++){
      double v = x(i,j) - d(0,j+1);
      sum += v*v;
    }
    result(i,0) = d0*(sum);
  }
  
  return result;
}
 
// [[Rcpp::export]]
NumericMatrix rootRadialScal(const NumericMatrix& x, const NumericVector& dd) {
 int rows = x.nrow();
 int cols = x.ncol();
 
 NumericMatrix d(1, cols+1, dd.begin());
 NumericMatrix result(x.rows(),1);
 
 double d0 = d[0];
 for(int i=0; i < rows; i++){
   double sum = 0.0;
   for(int j=0; j < cols; j++){
     double v = x(i,j) - d(0,j+1);
     sum += v*v;
   }
   result(i,0) = d0*sqrt(sum);
 }
 
 return result;
}


// [[Rcpp::export]]
NumericMatrix dotprScal(const NumericMatrix& x, const NumericVector& dd) {
  int rows = x.nrow();
  int cols = x.ncol();
  
  NumericMatrix d(1, cols+1, dd.begin());
  NumericMatrix result(x.rows(),1);
  
  double d0 = d[0];
  for(int i=0; i < rows; i++){
    double sum = 0.0;
    for(int j=0; j < cols; j++){
      sum += x(i,j)*d(0,j+1);;
    }
    result(i,0) = d0+sum;
  }
  
  return result;
}

