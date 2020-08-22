#include <Rcpp.h>
#include <RcppEigen.h>
#include <cmath>

// [[Rcpp::depends(RcppEigen)]]

using Eigen::Map;                       // 'maps' rather than copies
using Eigen::VectorXd;                  // variable size vector, double precision
using Eigen::RowVectorXd;               // double precision row vector
using Rcpp::NumericVector;


// Bias correction function (Chen 1999)
double rho(double x,  double b) {
  double c1 = 2.0*pow(b,2.0) + 2.5;
  double c2 = sqrt(4.0*pow(b, 4.0) + 6.0*pow(b, 2.0) + 2.25 - pow(x,2.0) - x/b);
  return c1 + c2;
}

// Helper function to generate dbeta value vector 
VectorXd vec_dbeta(Map<VectorXd> x, double a, double b){
  int n = x.size(); 
  RowVectorXd d(n); // Allocate memory
  
  for (int j = 0; j < n; j++){
    d(j) = R::dbeta(x(j), a, b, 0);  // scalar output 
  }
  return d;
}

// Piecewise kernel value estimation (Chen 1999)
 double bc_kde_calc(double xs, double b,  NumericVector pvals){
  
  // Memory allocation
  double d;
  NumericVector out;

  if ((xs >= 2*b) && (xs <= (1 - 2*b))){
    out = Rcpp::dbeta(pvals, xs/b, (1 - xs)/b, 0.0);
    Map<VectorXd>  tmp(Rcpp::as< Map<VectorXd> >(out));
    d = tmp.sum() / tmp.size();               // vector mean
  } else if ((xs >= 0) && (xs < 2*b)) {
    out = Rcpp::dbeta(pvals, rho(xs, b), (1 - xs)/b, 0.0);
    Map<VectorXd>  tmp(Rcpp::as< Map<VectorXd> >(out));
    d = tmp.sum() / tmp.size();
  } else if ((xs > (1 - 2*b)) && (xs <= 1)) {
    out = Rcpp::dbeta(pvals, xs/b, rho(1 - xs, b), 0.0);
    Map<VectorXd> tmp(Rcpp::as< Map<VectorXd> >(out));
    d = tmp.sum() / tmp.size();
  } else {
    d = 0.0;
  }
  return d;
}


// [[Rcpp::export]]
RowVectorXd bc_dunif(Map<VectorXd> xs, NumericVector pvals, 
                     double b, double xmax = 1.0){
  // Function based on biased-corrected (modified) beta kernel 
  //Chen, Song Xi. "Beta kernel estimators for density functions." 
  //Computational Statistics & Data Analysis 31.2 (1999): 131-145.

  xs = xs / xmax;
  pvals = pvals / xmax;
  b = b/xmax;           // smoothing parameter (i.e. bw)
  
  int n = xs.size();
  RowVectorXd d(n);     // Allocate memory
  double out_calc;
  
  // Elementwise boundary corrected values
  for (int i = 0; i < n; i++){
    out_calc = bc_kde_calc(xs(i), b, pvals);
    d(i) = out_calc;
  }
  
  return d / xmax;
  
}

