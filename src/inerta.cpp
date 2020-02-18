#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//' inertia
//'
//' A function to compute the inertia effect.
//'
//' @param evls 2-column edgelist (event, time) in relevent::rem format.
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//'
//' @return matrix (time, dyad)

//[[Rcpp::export]]
arma::mat inertia(arma::mat evls, arma::mat riskset) {
    // Storage space and fill with zeros
    arma::mat stat(evls.n_rows, riskset.n_rows, fill::zeros);

    // For loop over the sequence
    for(arma::uword i = 1; i < evls.n_rows; ++i) {
        //Copy the previous row
        arma::rowvec thisrow = stat.row(i-1);
        //Get the (position of the) previous event
        arma::uword event = evls(i-1, 0) - 1.0;
        //Add one to the previous event
        thisrow(event) += 1;
        //Change the row in the statistic
        stat.row(i) = thisrow;
    }

    // Output
    return stat;
}