// [[Rcpp::depends(RcppArmadillo)]]
#define ARMA_64BIT_WORD 1
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
int distlist(const arma::mat& X, int plot, char dataset_name) {
  int n = X.n_rows;
  int nb_rows = ((n*(n-1))/2);
  // Initialize with zeros the matrix to store results
  arma::mat my_matrix; my_matrix.zeros(nb_rows, 3);
  
  arma::vec vec_of_num{0.0, 1.0/10.0, 1.0/5.0, 1.0/4.0, 1.0/3.0, 1.0/2, 1.0/1.5, 1.0};
  
  for(int k = 0; k < vec_of_num.size()-1; k++) { //define divisors in order to have smaller datasets
    float l = vec_of_num(k);
    float m = vec_of_num(k+1);
    
    // loop and fill rows
    for(int i = round(n*l) ; i < round(n*m); i++) {
      
      arma::vec p0 = X.row(i).t(); // fix a point 
      for(int j = i+1; j < n; j++) {
        arma::vec p1 = X.row(j).t();
        arma::vec diff = p0 - p1; // (x0-x1,y0-y1)
        double dist = sqrt(as_scalar(diff.t() * diff)); // sqrt((x0-x1)² + (y0-y1)²)
        if (dist <= 100 && dist>0) {
          double i2 = (double)i;
          double j2 = (double)j;
          my_matrix.row(n*i-((i+1)*(i+2))/2+j) = {i2, j2, dist};
        }//end of condition to save the line
      }//end of loop on j
    }//end of loop on i
    //remove lines with null distance
    arma::uvec row_keep = find(my_matrix.col(2) != 0);
    my_matrix = my_matrix.rows(row_keep);
    char filename[40];
    sprintf(filename, "file%d_%d_%c.txt", k, plot, dataset_name);  
    my_matrix.save(filename, raw_ascii);
    
    my_matrix.zeros(nb_rows, 3);
    
  }//end of loop on dataset divisors
  
  return 0;
}//end of function

//data <- cbind(runif(100,0,1), runif(100,0,1))
//start <- Sys.time()
//distlist(data, 1, "Test")
//time <- Sys.time()-start