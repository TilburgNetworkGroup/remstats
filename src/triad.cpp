#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//' triad
//'
//' A function to compute the triadic effects.
//' 
//' @param actors vector with numeric actor IDs (correspod to edgelist, riskset)
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//' @param type (1 = outgoing two-path)
//'
//' @return matrix (time, dyad)

//[[Rcpp::export]]
arma::mat triad(arma::vec actors, arma::mat edgelist, arma::mat riskset, arma::uword type) {

    //Storage space
    //(1) Adjacency matrix 
    arma::mat adj(max(actors), max(actors), fill::zeros);
    //(2) Statistic matrix (output)
    arma::mat stat(edgelist.n_rows, riskset.n_rows, fill::zeros);

    //For loop over the sequence 
    for(arma::uword i = 1; i < edgelist.n_rows; ++i) {
        //Sender of the previous event
        arma::uword sender = edgelist(i-1, 1);
        //Receiver of the previous event
        arma::uword receiver = edgelist(i-1, 2); 

        // Update the adjacency matrix
        adj(sender - 1, receiver - 1) += 1;

        // Statistic row
        arma::rowvec thisrow(riskset.n_rows, fill::zeros);

        //For loop over dyads
        for(arma::uword d = 0; d < riskset.n_rows; ++d) {
            //Sender of the dyad
            arma::uword senderD = riskset(d, 0);
            //Receiver of the dyad
            arma::uword receiverD = riskset(d, 1);

            //Outgoing communication senderD
            arma::rowvec outSenderD = adj.row(senderD-1);
            //Incoming communication senderD
            arma::colvec inSenderD = adj.col(senderD-1);
            //Outgoing communication receiverD
            arma::rowvec outReceiverD = adj.row(receiverD-1);
            //Incoming communication receiverD
            arma::colvec inReceiverD = adj.col(receiverD-1);

            //Saving space
            arma::uword stat = 0;

            // Outbound two-path: i -> h -> j    
            if(type == 1) {
                //For loop over actors
                for(arma::uword h = 0; h < max(actors); ++h) {
                    if((h != (senderD-1)) && (h!= (receiverD-1))) {
                        arma::vec thisactor = {outSenderD(h), inReceiverD(h)};
                        stat += min(thisactor);                       
                    }
                }              
            }

            // Inbound two-path: i <- h <- j    
            if(type == 2) {
                //For loop over actors
                for(arma::uword h = 0; h < max(actors); ++h) {
                    if((h != (senderD-1)) && (h!= (receiverD-1))) {
                        arma::vec thisactor = {inSenderD(h), outReceiverD(h)};
                        stat += min(thisactor);                       
                    }
                }              
            }

            // Outbound shared partners: i -> h <- j    
            if(type == 3) {
                //For loop over actors
                for(arma::uword h = 0; h < max(actors); ++h) {
                    if((h != (senderD-1)) && (h!= (receiverD-1))) {
                        arma::vec thisactor = {outSenderD(h), outReceiverD(h)};
                        stat += min(thisactor);                       
                    }
                }              
            }

            // Inbound shared partners: i <- h -> j    
            if(type == 4) {
                //For loop over actors
                for(arma::uword h = 0; h < max(actors); ++h) {
                    if((h != (senderD-1)) && (h!= (receiverD-1))) {
                        arma::vec thisactor = {inSenderD(h), inReceiverD(h)};
                        stat += min(thisactor);                       
                    }
                }              
            }

            // Save the statistic for this dyad
            thisrow(d) = stat;         
        }
        // Save the statistic for this timepoint
        stat.row(i) = thisrow;     
    }
    // Output
    return(stat);
}