#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// To perform convolutions to a matrix I have to extract a 3x3 submatrix 
// around each element of it. This function allows to do convolutions on 
// the edge elements of the matrix since it extracts elements from the opposite
// rows or columns to avoid losing dimensionality after convolution
int subm_index(int M, int x)
{
  if (x<0)
    return x+M;
  if(x >= M)
    return x-M;
  return x;
}

// This function performs a single iteration of Gray-Scott algorithm
arma::cube update_concentrations(arma::cube X, 
                                 arma::mat L, 
                                 double DA, 
                                 double DB,
                                 double f, 
                                 double k){
  
  float sum_a, sum_b, x1, y1;
  int m = X.n_rows;
  int n = X.n_cols;
  
  arma::cube X_new(m, n, 2);
  
  for(int x = 0; x < m; x++) {
    for(int y = 0; y < n; y++){
      sum_a = 0.0;
      sum_b = 0.0;
      for(int i = -1; i <= 1; i++){
        for(int j = -1; j <= 1; j++){
          x1 = subm_index(m, x - i);
          y1 = subm_index(n, y - j);
          sum_a = sum_a + L(i+1, j+1) * X(x1, y1, 0);
          sum_b = sum_b + L(i+1, j+1) * X(x1, y1, 1);
        }
      }
      X_new(x,y,0) = X(x,y,0)+DA*sum_a-X(x,y,0)*pow(X(x,y,1),2)+f*(1-X(x,y,0));
      X_new(x,y,1) = X(x,y,1)+DB*sum_b+X(x,y,0)*pow(X(x,y,1),2)-(k+f)*X(x,y,1);
    }
  }
  return X_new;
}

// This function performs Gray-Scott a number of times n
// [[Rcpp::export]]
arma::cube iterate_Gray_Scott(arma::cube X, 
                              arma::mat L, 
                              double DA, 
                              double DB,
                              double f, 
                              double k,
                              int n){
  for(int i = 0; i < n; i++){
    X = update_concentrations(X, L, DA, DB, f, k); 
  }
  return X;
}
