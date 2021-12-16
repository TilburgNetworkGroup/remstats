#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]

// compute_adjmat
//
// Helper function that computes or updates the adjacency matrix. 
//
// Computes at each time point for every potential relational event, i.e., 
// potential edge, in the risk set its weight based on the past. Can account 
// for memory effects by setting the `memory` argument together with the 
// `memory_value` argument.  
//
// *param [edgelist] matrix with the observed relational event history. Rows 
// refers to the observed relational events. The first column refers to the 
// time, the second column to the events and the third column to the event 
// weight. 
// *param [adjmat] matrix with the adjacency counts that need to be updated 
// (timepoint t gives the event weight until t-1), 0x0 matrix if the adjacency
// count has to be computed from scratch 
// *param [N] integer number refering to the total number of actors. 
// *param [D] integer number refering to the total number of potential 
// relational events in the risk set. 
// *param [directed] boolean whether the events are directed
// *param [memory] integer number indicating the type of memory effect, i.e., 
// how past events influence future events/should be accounted for in the 
// adjacency matrix count. 0 = full history, 1 = windowed memory, 2 = 
// Brandes-type memory, i.e., exponential decay with a half-life parameter 
// (see also memory_value). 
// *param [memory_value] numeric value indicating the memory parameter, i.e., 
// the window width if memory = 1, and the half-life time if memory = 2
//
// *return [adjmat] adjacency matrix with per row the number of past events for
// the respective dyads in the columns
//
// [[Rcpp::export]]
arma::mat compute_adjmat_update(const arma::mat& edgelist, arma::mat adjmat, 
    int N, int D, bool directed, int memory, double memory_value, 
    int start, int stop) {

    // Does the adjacency matrix need to be computed from scratch or updated?
    bool update = adjmat.n_cols != 0;

    // Full memory
    if(memory == 1) {
        if(!update) {
            // Slice the edgelist according to "start" and "stop"
            arma::mat slice = edgelist.rows(start, stop);
            
            // Initialize memory and fill with zeros
            adjmat.set_size(slice.n_rows, D);
            adjmat.fill(0);  

            // (1) Initialize adjacency matrix
            // Select the past
            double time = slice(0,0);
            arma::uvec pastkey = arma::find(edgelist.col(0) < time);
            arma::mat past = edgelist.rows(pastkey);

            // For loop over the past
            for(arma::uword j = 0; j < past.n_rows; ++j) {
                // Add event weight to adjacency matrix
                adjmat(0, past(j, 1)) += past(j, 2);
            }

            // (2) For loop over timepoints
            for(arma::uword i = 1; i < slice.n_rows; ++i) {
                // Copy previous row
                adjmat.row(i) = adjmat.row(i-1);
                // Add event weight previous event to adjacency matrix
                adjmat(i, slice(i-1, 1)) += slice(i-1, 2);
            }
        } else {
            // Add the new events to the adjacency matrix 
            Rcpp::Range index_new = Rcpp::seq(adjmat.n_rows - 1, 
                edgelist.n_rows - 2);

            // Initialize memory and fill with zeros
            arma::mat new_rows(index_new.size(), D, arma::fill::zeros);  
          
            // For loop over index
            for(arma::uword i = 0; i < index_new.size(); ++i) {
                // Copy previous row
                if(i == 0) {
                    new_rows.row(0) = adjmat.row(adjmat.n_rows - 1);
                } else {
                    new_rows.row(i) = new_rows.row(i-1);
                }
                
                // Add event weight previous event to adjacency matrix
                new_rows(i, edgelist(index_new[i], 1)) += 
                    edgelist(index_new[i], 2);
            }

            // Add new rows to the adjacency matrix 
            adjmat = join_cols(adjmat, new_rows);
        }
    }

    // Output
    return adjmat;
}
