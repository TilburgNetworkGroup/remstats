#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//' degree
//'
//' A function to compute the indegree, outdegree and total degree effects.
//' 
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//' @param type (1 = indegree_sender, 2 = indegree_receiver, 3 = outdegree_sender, 
//' 4 = outdegree_receiver, 5 = totaldegree_sender, 6 = totaldegree_receiver) 
//'
//' @return matrix (time, dyad)

//[[Rcpp::export]]
arma::mat degree(arma::mat edgelist, arma::mat riskset, arma::uword type) {
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

        //indegree_sender and totaldegree_sender: Add one to the events that involve the previous receiver as sender      
        if((type == 1) || (type == 5)) {
            // Positions in the riskset
            arma::uvec indices = find(riskset.col(0) == receiver);
            // Add one
            thisrow(indices) +=1;
        }

        //indegree_receiver and totaldegree_receiver: Add one to the events that involve the previous receiver as receiver      
        if((type == 2) || (type == 6)) {
            // Positions in the riskset
            arma::uvec indices = find(riskset.col(1) == receiver);
            // Add one
            thisrow(indices) +=1;
        }

        //outdegree_sender and totaldegree_sender: Add one to the events that involve the previous sender as sender      
        if((type == 3) || (type == 5)) {
            // Positions in the riskset
            arma::uvec indices = find(riskset.col(0) == sender);
            // Add one
            thisrow(indices) +=1;
        }

        //outdegree_receiver and totaldegree_receiver: Add one to the events that involve the previous sender as receiver      
        if((type == 4) || (type == 6)) {
            // Positions in the riskset
            arma::uvec indices = find(riskset.col(1) == sender);
            // Add one
            thisrow(indices) +=1;
        }

        //Change the row in the statistic
        stat.row(i) = thisrow;
    }

    // Output
    return stat;
}