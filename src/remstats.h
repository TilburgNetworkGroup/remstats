#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//' actorStat
//'
//' A function to transform exogenous actor covariate variables in the format ..
//' requested for estimation with relevent::rem(). 
//'
//' @param values 3-column matrix (id, change time, value). Actor ids should 
//' correspond to actor ids in the edgelist and riskset. Change time is zero 
//' for all entries if the covariate is time invariate. 
//' @param type 1 = sender effect, 2 = receiver effect
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2).
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistD)
//' data(covar)
//' out <- prepER(edgelist = edgelistD)
//' el <- out$edgelist
//' rs <- out$rs
//' ac <- out$ac
//' covar$id <- ac$id[match(covar$id, ac$name)]
//' stat <- actorStat(values = covar[,c(1:3)], type = 1, edgelist = el, 
//'     riskset = rs)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat actorStat(arma::mat values, arma::uword type, arma::mat edgelist, 
    arma::mat riskset) {
    // Storage space and fill with zeros
    arma::mat stat(edgelist.n_rows, riskset.n_rows, fill::zeros);

    // Set the first row 
    // For loop over dyads
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        // Find the relevant actor
        arma::uword actor = 0;
        if(type == 1) {actor = riskset(i, 0);} // Sender
        if(type == 2) {actor = riskset(i, 1);} // Receiver

        // Find the value for this actor
        arma::uvec index = find(values.col(0) == actor && values.col(1) == 0);
        double value = values(index(0), 2);

        // Save the value
        stat(0, i) = value;
    }

    // Find the unique change timepoints
    arma::vec changetimes = sort(unique(values.col(1)));
    changetimes = changetimes(find(changetimes!=0));
    arma::uword counter = 0;

    // For loop over the sequence
    for(arma::uword m = 1; m < edgelist.n_rows; ++m) {
        // Copy the previous row
        arma::rowvec thisrow = stat.row(m-1);

        // Update the statistic if required
        // Do not update after the last changetime
        if(counter < changetimes.n_elem) {
            // Update if the time of the event is larger than the current 
            // changetime
            if(edgelist(m, 0) > changetimes(counter)) {
                // Check whether a changetime needs to be skipped (i.e., when a 
                // multiple changes occur between events)
                while((counter < (changetimes.n_elem - 1)) && 
                    (edgelist(m, 0) > changetimes(counter+1))) {counter+=1;}  
                
                // For loop over dyads
                for(arma::uword i = 0; i < riskset.n_rows; ++i) {
                    // Find the relevant actor
                    arma::uword actor = 0;
                    if(type == 1) {actor = riskset(i, 0);} // Sender
                    if(type == 2) {actor = riskset(i, 1);} // Receiver

                    // Find the value for this actor 
                    arma::uvec index = find((values.col(0) == actor) && 
                        (values.col(1) == changetimes(counter)));
                    // Update if a new value exists
                    if(index.n_elem == 1) {
                        double value = values(index(0), 2);
                        thisrow(i) = value;
                    }                 
                }

                //Update the counter
                counter+=1;
            }
        }
            
        // Save the row
        stat.row(m) = thisrow;
    }

    // Output
    return stat; 
}

/*
TO DO: R wrapper function so that a default value for weights can be set?
*/

//' inertia
//'
//' A function to compute the inertia effect.
//'
//' @param evls 2-column edgelist (event, time) in relevent::rem format.
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2).
//' @param weights vector (length evls) 
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
//' stat <- inertia(evls, rs, weights = rep(1, nrow(el)))
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat inertia(arma::mat evls, arma::mat riskset, arma::vec weights) {
    // Storage space and fill with zeros
    arma::mat stat(evls.n_rows, riskset.n_rows, fill::zeros);

    // For loop over the sequence
    for(arma::uword i = 1; i < evls.n_rows; ++i) {
        //Copy the previous row
        arma::rowvec thisrow = stat.row(i-1);
        //Get the (position of the) previous event
        arma::uword event = evls(i-1, 0) - 1.0;
        //Add the weight of the previous event
        thisrow(event) += weights(i-1);
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
//' ac <- out$actors[,1]
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

/* 
TO DO: Merge with triad()??
TO DO: R wrapper function so a default value for unique can be set?
*/

//' triadU
//'
//' A function to compute the (unique) shared partners effect for undirected 
//' relational events.
//'
//' @param actors vector with numeric actor IDs (correspond to edgelist,
//' riskset)
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//' @param uinque_sp logical value
//'
//' @return matrix (time x dyad)
//' 
//' @examples
//' data(edgelistU)
//' out <- prepER(edgelist = edgelistU, directed = FALSE, type = FALSE, 
//'     riskset = NULL, actors = NULL)
//' el <- out$edgelist
//' rs <- out$riskset
//' ac <- out$actors[,1]
//' stat <- triadU(ac, el, rs, unique_sp = FALSE)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat triadU(arma::vec actors, arma::mat edgelist, arma::mat riskset, 
    bool unique_sp) {

    //Storage space
    //(1) Adjacency matrix 
    arma::mat adj(max(actors), max(actors), fill::zeros);
    //(2) Statistic matrix (output))
    arma::mat stat(edgelist.n_rows, riskset.n_rows, fill::zeros);

    //For loop over the sequence
    for(arma::uword i = 1; i < edgelist.n_rows; ++i) {
        //Actors of the previous event
        arma::uword actor1 = edgelist(i-1, 1);  
        arma::uword actor2 = edgelist(i-1, 2);  
        
        //Update the adjacency matrix
        adj(actor1 - 1, actor2 - 1) += 1;  
        
        //If "unique_sp" is requested, dichotomize the adjacency matrix
        if(unique_sp) {adj.replace(2, 1);}

        //Actors that have a relation with the actors involved in the previous 
        //event
        arma::uvec relActors1 = find(adj.row(actor1-1) != 0);  
        arma::uvec relActors2 = find(adj.col(actor1-1) != 0);  
        arma::uvec relActors3 = find(adj.row(actor2-1) != 0);  
        arma::uvec relActors4 = find(adj.col(actor2-1) != 0);  
        arma::uvec relActors = join_cols(relActors1, relActors2);
        relActors = join_cols(relActors, relActors3); 
        relActors = join_cols(relActors, relActors4); 
        relActors = sort(unique(relActors));          

        //Copy the previous row
        arma::rowvec thisrow = stat.row(i-1);

        //For loop over dyads
        for(arma::uword d = 0; d < riskset.n_rows; ++d) {
            //Actors in the dyad
            arma::uword actor1D = riskset(d, 0);
            arma::uword actor2D = riskset(d, 1);

            //Only change their statistic if both actors have a relation to the 
            //actors involved in the previous event
            if(any(relActors == (actor1D - 1)) &&
                any(relActors == (actor2D - 1))) {

                    //Communications actor1D
                    arma::rowvec com1Actor1Dtemp = adj.row(actor1D-1);
                    arma::colvec com1Actor1D = conv_to<colvec>::from(com1Actor1Dtemp);
                    arma::colvec com2Actor1D = adj.col(actor1D-1);
                    arma::vec comActor1D = com1Actor1D + com2Actor1D;
                    //Communications actor2D
                    arma::rowvec com1Actor2Dtemp = adj.row(actor2D-1);
                    arma::colvec com1Actor2D = conv_to<colvec>::from(com1Actor2Dtemp);
                    arma::colvec com2Actor2D = adj.col(actor2D-1);
                    arma::vec comActor2D = com1Actor2D + com2Actor2D;

                    //Saving space
                    arma::uword dyadstat = 0;

                    //For loop over actors 
                    for(arma::uword h = 0; h < max(actors); ++h) {
                        if((h != (actor1D-1)) && (h != (actor2D-1))) {
                            arma::vec thisactor = {comActor1D(h), comActor2D(h)};
                            dyadstat += min(thisactor);        
                        }
                    }

                    //Save the statistic for this dyad
                    thisrow(d) = dyadstat;
                }
        }

        //Save the statistic for this timepoint
        stat.row(i) = thisrow;
    }
    //Output
    return(stat);
}