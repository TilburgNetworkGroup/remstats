#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//' reciprocity
//'
//' A function to compute the indegree, outdegree and total degree effects.
//' 
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//'
//' @return matrix (time, dyad)

//[[Rcpp::export]]
arma::mat reciprocity(arma::mat edgelist, arma::mat riskset) {
    // Storage space and fill with zeros
    arma::mat stat(edgelist.n_rows, riskset.n_rows, fill::zeros);

    // For loop over the sequence
    for(arma::uword i = 1; i < edgelist.n_rows; ++i) {
        //Copy the previous row
        arma::rowvec thisrow = stat.row(i-1);

        //Sender of the previous event
        arma::uword sender = edgelist(i-1, 1);
        //Receiver of the previous event
        arma::uword receiver = edgelist(i-1, 2); 

        //Find the reciprocal event
        arma::uvec indices = find(riskset.col(0) == receiver && riskset.col(1) == sender);
        
        //Add one to the reciprocal event
        thisrow(indices) +=1;

        //Change the row in the statistic
        stat.row(i) = thisrow;
    }

    // Output
    return stat;
}