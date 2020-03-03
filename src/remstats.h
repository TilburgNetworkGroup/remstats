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
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistD)
//' out <- prepER(edgelist = edgelistD, directed = TRUE, type = FALSE, 
//'     riskset = NULL, actors = NULL)
//' el <- out$edgelist
//' rs <- out$riskset
//' evls <- prepEvls(el, rs, type = FALSE)
//' stat <- inertia(evls, rs)
//'
//' @export
//'
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

//' reciprocity
//'
//' A function to compute the indegree, outdegree and total degree effects.
//' 
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistD)
//' out <- prepER(edgelist = edgelistD, directed = TRUE, type = FALSE, 
//'     riskset = NULL, actors = NULL)
//' el <- out$edgelist
//' rs <- out$riskset
//' stat <- reciprocity(el, rs)
//'
//' @export
//'
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

//' degree
//'
//' A function to compute the indegree, outdegree and total degree effects.
//' 
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//' @param type 1 = indegree_sender, 2 = indegree_receiver, 3 = 
//' outdegree_sender, 4 = outdegree_receiver, 5 = totaldegree_sender, 6 = 
//' totaldegree_receiver 
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistD)
//' out <- prepER(edgelist = edgelistD, directed = TRUE, type = FALSE, 
//'     riskset = NULL, actors = NULL)
//' el <- out$edgelist
//' rs <- out$riskset
//' indegree_sender <- degree(el, rs, type = 1)
//'
//' @export
//'
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

//' triad
//'
//' A function to compute the triadic effects.
//' 
//' @param actors vector with numeric actor IDs (correspod to edgelist, riskset)
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//' @param type (1 = outgoing two-path, 2 = incoming two-path, 3 = outbound 
//' shared partners, 4 = inbound shared partners)
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistD)
//' out <- prepER(edgelist = edgelistD, directed = TRUE, type = FALSE, 
//'     riskset = NULL, actors = NULL)
//' el <- out$edgelist
//' rs <- out$riskset
//' ac <- sort(unique(c(rs[,1], rs[,2])))
//' otp <- triad(ac, el, rs, type = 1)
//'
//' @export
//'
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

        //Update the adjacency matrix
        adj(sender - 1, receiver - 1) += 1;

        //Actors that have a relation with the sender/receiver of the previous 
        //event
        arma::uvec relActors1 = find(adj.row(sender-1) != 0);
        arma::uvec relActors2 = find(adj.col(sender-1) != 0);
        arma::uvec relActors3 = find(adj.row(receiver-1) != 0);
        arma::uvec relActors4 = find(adj.col(receiver-1) != 0);
        arma::uvec relActors = join_cols(relActors1, relActors2);
        relActors = join_cols(relActors, relActors3);
        relActors = join_cols(relActors, relActors4);
        relActors = sort(unique(relActors));

        //Copy the previous row
        arma::rowvec thisrow = stat.row(i-1);

        //For loop over dyads
        for(arma::uword d = 0; d < riskset.n_rows; ++d) {
            //Sender of the dyad
            arma::uword senderD = riskset(d, 0);
            //Receiver of the dyad
            arma::uword receiverD = riskset(d, 1);
            
            //Only change their statistic if both actors have a relation to the 
            //sender/receiver of the previous event
            if(any(relActors == (senderD - 1)) && 
                any(relActors == (receiverD - 1))) {

                //Outgoing communication senderD
                arma::rowvec outSenderD = adj.row(senderD-1);
                //Incoming communication senderD
                arma::colvec inSenderD = adj.col(senderD-1);
                //Outgoing communication receiverD
                arma::rowvec outReceiverD = adj.row(receiverD-1);
                //Incoming communication receiverD
                arma::colvec inReceiverD = adj.col(receiverD-1);

                //Saving space
                arma::uword dyadstat = 0;

                // Outbound two-path: i -> h -> j    
                if(type == 1) {
                    //For loop over actors
                    for(arma::uword h = 0; h < max(actors); ++h) {
                        if((h != (senderD-1)) && (h!= (receiverD-1))) {
                            arma::vec thisactor = {outSenderD(h), inReceiverD(h)};
                            dyadstat += min(thisactor);                       
                        }
                    }              
                }

                // Inbound two-path: i <- h <- j    
                if(type == 2) {
                    //For loop over actors
                    for(arma::uword h = 0; h < max(actors); ++h) {
                        if((h != (senderD-1)) && (h!= (receiverD-1))) {
                            arma::vec thisactor = {inSenderD(h), outReceiverD(h)};
                            dyadstat += min(thisactor);                       
                        }
                    }              
                }

                // Outbound shared partners: i -> h <- j    
                if(type == 3) {
                    //For loop over actors
                    for(arma::uword h = 0; h < max(actors); ++h) {
                        if((h != (senderD-1)) && (h!= (receiverD-1))) {
                            arma::vec thisactor = {outSenderD(h), outReceiverD(h)};
                            dyadstat += min(thisactor);                       
                        }
                    }              
                }

                // Inbound shared partners: i <- h -> j    
                if(type == 4) {
                    //For loop over actors
                    for(arma::uword h = 0; h < max(actors); ++h) {
                        if((h != (senderD-1)) && (h!= (receiverD-1))) {
                            arma::vec thisactor = {inSenderD(h), inReceiverD(h)};
                            dyadstat += min(thisactor);                       
                        }
                    }              
                }

                // Save the statistic for this dyad
                thisrow(d) = dyadstat;  
            }     
        }
        // Save the statistic for this timepoint
        stat.row(i) = thisrow;     
    }
    // Output
    return(stat);
}