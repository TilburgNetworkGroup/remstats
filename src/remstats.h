#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// TO DO: Create R wrapper functions for these cpp functions and export those instead of the ones below.

//' actorstat
//'
//' A function to transform exogenous actor covariate variables in the format 
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
//' rs <- out$riskset
//' ac <- out$actors
//' covar$id <- ac$id[match(covar$id, ac$name)]
//' covar <- as.matrix(covar)
//' stat <- actorstat(values = covar[,c(1:3)], type = 1, edgelist = el, 
//'     riskset = rs)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat actorstat(arma::mat values, arma::uword type, arma::mat edgelist, 
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
    for(arma::uword m = 0; m < edgelist.n_rows; ++m) {
        // Saving space
        arma::rowvec thisrow(riskset.n_rows);
        
        if(m == 0) {
            // Copy the current row
            thisrow = stat.row(m);
        } else {
            // Copy the previous row
            thisrow = stat.row(m-1);
        }

        // Update the statistic if required
        // Do not update after the last changetime
        if(counter < changetimes.n_elem) {
            // Update if the time of the event is larger than the current 
            // changetime
            if(edgelist(m, 0) > changetimes(counter)) {
                // Update all changes in between
                while((counter < changetimes.n_elem) && 
                    (edgelist(m, 0) > changetimes(counter))) {
                        // For loop over dyads
                        for(arma::uword i = 0; i < riskset.n_rows; ++i) {
                            // Find the relevant actor
                            arma::uword actor = 0;
                            if(type == 1) {actor = riskset(i, 0);} // Sender
                            if(type == 2) {actor = riskset(i, 1);} // Receiver

                            // Find the value for this actor 
                            arma::uvec index = find((values.col(0) == actor) 
                                && (values.col(1) == changetimes(counter)));
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
        }
            
        // Save the row
        stat.row(m) = thisrow;
    }

    // Output
    return stat; 
}

//' dyadstat
//'
//' A function to transform exogenous actor covariate variables in dyad 
//' statistics.
//'
//' @param values 3-column matrix (id, change time, value). Actor ids should 
//' correspond to actor ids in the edgelist and riskset. Change time is zero 
//' for all entries if the covariate is time invariate. 
//' @param type 1 = same, 2 = difference, 3 = mean, 4 = min, 5 = max, 
//' 6 = both equal to
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2).
//' @param equal_val value
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistU)
//' data(covar)
//' out <- prepER(edgelist = edgelistU, directed = FALSE)
//' el <- out$edgelist
//' rs <- out$riskset
//' ac <- out$actors    
//' covar$id <- ac$id[match(covar$id, ac$name)]
//' covar <- as.matrix(covar)
//' stat <- dyadstat(values = covar[,c(1:3)], type = 1, edgelist = el, 
//'     riskset = rs, NA)
//'
//' @export
//'
//[[Rcpp::export]]
arma::dmat dyadstat(arma::dmat values, arma::uword type, arma::mat edgelist, 
    arma::mat riskset, arma::uword equal_val) {
    // Storage space for the statistic 
    arma::dmat stat(edgelist.n_rows, riskset.n_rows, fill::zeros);

    // Storage space for the current covariate values
    arma::dvec current_ac1(riskset.n_rows, fill::zeros);
    arma::dvec current_ac2(riskset.n_rows, fill::zeros);
    
    // Set the first row 
    // For loop over dyads
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        // Find the relevant actors
        arma::uword actor1 = riskset(i, 0);
        arma::uword actor2 = riskset(i, 1);

        // Find the values for these actor
        arma::uvec index1 = find(values.col(0) == actor1 && values.col(1) == 0);
        current_ac1(i) = values(index1(0), 2);
        arma::uvec index2 = find(values.col(0) == actor2 && values.col(1) == 0);
        current_ac2(i) = values(index2(0), 2);

        // Are these values equal?
        if(type == 1) {stat(0, i) = (current_ac1(i)==current_ac2(i));}
        // What is the difference between these values?
        if(type == 2) {stat(0, i) = current_ac1(i)-current_ac2(i);}
       
        arma::dvec both = {current_ac1(i), current_ac2(i)};
        //What is the mean value?
        if(type == 3) {stat(0, i) = mean(both);}
        // What is the minimum value?
        if(type == 4) {stat(0, i) = min(both);}
        // What is the maximum value?
        if(type == 5) {stat(0, i) = max(both);}
        // Are both equal to this value?
        if(type == 6) {stat(0, i) = ((current_ac1(i) == equal_val) && 
            (current_ac2(i) == equal_val));}
    }

    // Find the unique change timepoints
    arma::vec changetimes = sort(unique(values.col(1)));
    changetimes = changetimes(find(changetimes!=0));
    arma::uword counter = 0;

    // For loop over the sequence
    for(arma::uword m = 0; m < edgelist.n_rows; ++m) {
        // Saving space
        arma::rowvec thisrow(riskset.n_rows);
        
        if(m == 0) {
            // Copy the current row
            thisrow = stat.row(m);
        } else {
            // Copy the previous row
            thisrow = stat.row(m-1);
        }

        // Update the statistic if required
        // Do not update after the last changetime
        if(counter < changetimes.n_elem) {
            // Update if the time of the event is larger than the current 
            // changetime
            if(edgelist(m, 0) > changetimes(counter)) {
                // Update all changes in between
                while((counter < changetimes.n_elem) && 
                    (edgelist(m, 0) > changetimes(counter))) {
                        
                        // For loop over dyads
                        for(arma::uword i = 0; i < riskset.n_rows; ++i) {
                            // Find the relevant actor
                            arma::uword actor1 = riskset(i, 0);
                            arma::uword actor2 = riskset(i, 1);

                            // Find the values for these actor
                            arma::uvec index1 = find((values.col(0) == actor1) 
                                && (values.col(1) == changetimes(counter)));
                            arma::uvec index2 = find((values.col(0) == actor2) 
                                && (values.col(1) == changetimes(counter)));
                            
                            // Update if a new value exists
                            if((index1.n_elem == 1) || (index2.n_elem == 1)) {
                                if(index1.n_elem == 1) {
                                    current_ac1(i) = values(index1(0), 2);
                                } 
                                if(index2.n_elem == 1) {
                                    current_ac2(i) = values(index2(0), 2);
                                } 
                           
                                // Are these values equal?
                                if(type == 1) {thisrow(i) = 
                                    (current_ac1(i)==current_ac2(i));}
                                // What is the difference between 
                                // these values?
                                if(type == 2) {thisrow(i) = 
                                    current_ac1(i)-current_ac2(i);}
                            
                                arma::dvec both = {current_ac1(i), 
                                    current_ac2(i)};
                                //What is the mean value?
                                if(type == 3) {thisrow(i) = mean(both);}
                                // What is the minimum value?
                                if(type == 4) {thisrow(i) = min(both);}
                                // What is the maximum value?
                                if(type == 5) {thisrow(i) = max(both);}
                                // Are both equal to this value?
                                if(type == 6) {thisrow(i) = 
                                    ((current_ac1(i) == equal_val) && 
                                        (current_ac2(i) == equal_val));}
                            }               
                        }
                    
                    //Update the counter
                    counter+=1;
                }  
            }
        }
            
        // Save the row
        stat.row(m) = thisrow;
    }

    // Transform difference to absolute difference
    if(type == 2) {stat = abs(stat);}

    //Output
    return(stat);
}

//' inertia
//'
//' A function to compute the inertia effect.
//' 
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//' @param weights vector (length edgelist) 
//' @param standardize logical 
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistD)
//' out <- prepER(edgelist = edgelistD)
//' edgelist <- out$edgelist
//' riskset <- out$riskset
//' equal_weights <- rep(1, nrow(edgelist))
//' stat <- inertia(edgelist, riskset, equal_weights, standardize = FALSE)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat inertia(arma::mat edgelist, arma::mat riskset, arma::vec weights, 
    bool standardize) {
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

        //Find the dyad's position in the riskset
        arma::uvec dyad = find(riskset.col(0) == sender && 
            riskset.col(1) == receiver);
        
        //Add the intensity to the dyad's intensity count
        thisrow(dyad) += weights(i-1);

        //Change the row in the statistic
        stat.row(i) = thisrow;
    }

    // Standardize effect if requested
    if(standardize) {
        for(arma::uword i = 0; i < edgelist.n_rows; ++ i) {
            if(stddev(stat.row(i)) > 0) {
                stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
                    stddev(stat.row(i));
            }
        }
    }

    // Output
    return stat;
}

//' reciprocity
//'
//' A function to compute reciprocity.
//' 
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//' @param weights vector (length edgelist) 
//' @param standardize logical 
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistD)
//' out <- prepER(edgelist = edgelistD)
//' edgelist <- out$edgelist
//' riskset <- out$riskset
//' equal_weights <- rep(1, nrow(edgelist))
//' stat <- reciprocity(edgelist, riskset, equal_weights, standardize = FALSE)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat reciprocity(arma::mat edgelist, arma::mat riskset, arma::vec weights,
    bool standardize) {
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

        //Find the reciprocal dyad
        arma::uvec dyad = find(riskset.col(0) == receiver && riskset.col(1) == sender);
        
        //Add the intensity to the reciprocal dyad
        thisrow(dyad) += weights(i-1);

        //Change the row in the statistic
        stat.row(i) = thisrow;
    }

    // Standardize effect if requested
    if(standardize) {
        for(arma::uword i = 0; i < edgelist.n_rows; ++ i) {
            if(stddev(stat.row(i)) > 0) {
                stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
                    stddev(stat.row(i));
            }
        }
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
//' @param standardize logical 
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistD)
//' out <- prepER(edgelist = edgelistD, directed = TRUE, type = FALSE, 
//'     riskset = NULL, actors = NULL)
//' el <- out$edgelist
//' rs <- out$riskset
//' indegree_sender <- degree(edgelist = el, riskset = rs, type = 1, 
//'     standardize = FALSE)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat degree(arma::mat edgelist, arma::mat riskset, arma::uword type, 
    bool standardize) {
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

    // Standardize effect if requested
    if(standardize) {
        for(arma::uword i = 0; i < edgelist.n_rows; ++ i) {
            if(stddev(stat.row(i)) > 0) {
                stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
                    stddev(stat.row(i));
            }
        }
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
//' @param standardize logical 
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
//' otp <- triad(ac, el, rs, type = 1, standardize = FALSE)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat triad(arma::vec actors, arma::mat edgelist, arma::mat riskset, arma::uword type, bool standardize) {

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

    // Standardize effect if requested
    if(standardize) {
        for(arma::uword i = 0; i < edgelist.n_rows; ++ i) {
            if(stddev(stat.row(i)) > 0) {
                stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
                    stddev(stat.row(i));
            }
        }
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
//' @param unique_sp logical value
//' @param standardize logical value
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
//' stat <- triadU(ac, el, rs, unique_sp = FALSE, standardize = FALSE)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat triadU(arma::vec actors, arma::mat edgelist, arma::mat riskset, 
    bool unique_sp, bool standardize) {

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

    // Standardize effect if requested
    if(standardize) {
        for(arma::uword i = 0; i < edgelist.n_rows; ++ i) {
            if(stddev(stat.row(i)) > 0) {
                stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
                    stddev(stat.row(i));
            }
        }
    }

    //Output
    return(stat);
}

//' pshift
//'
//' A function to compute Gibson's (2003) dyadic participation shifts. 
//'
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//' @param type (1 = AB-BA, 2 = AB-BY, 3 = AB-XA, 4 = AB-XB, 5 = AB-XY, 
//' 6 = AB-AY)
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistD)
//' out <- prepER(edgelistD)
//' ABBA <- pshift(edgelist = out$edgelist, riskset = out$riskest, type = 1)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat pshift(arma::mat edgelist, arma::mat riskset, int type) {
    // Storage space and fill with zeros
    arma::mat stat(edgelist.n_rows, riskset.n_rows, fill::zeros);

    // For loop over events
    for(arma::uword i = 0; i < (edgelist.n_rows - 1); ++i) {
        //Sender of the current event
        arma::uword sender = edgelist(i, 1);
        //Receiver of the current event
        arma::uword receiver = edgelist(i, 2); 

        // Storage space
        arma::uvec psdyads = {0};

        // Find the dyads that would create the respective p-shift 
        switch(type) {
            // AB-BA
            case 1 :
                // Find the reverse dyad (BA)
                psdyads = find(riskset.col(0) == receiver && 
                    riskset.col(1) == sender);
                break;
            
            // AB-BY
            case 2 :
                // Find all BY dyads 
                psdyads = find(riskset.col(0) == receiver && 
                    riskset.col(1) != sender && riskset.col(1) != receiver);
                break;

            // AB-XA
            case 3 :
                // Find all XA dyads 
                psdyads = find(riskset.col(1) == sender && 
                    riskset.col(0) != receiver && riskset.col(0) != sender);
                break;
            
            // AB-XB
            case 4 :
                // Find all XB dyads 
                psdyads = find(riskset.col(1) == receiver && 
                    riskset.col(0) != sender && riskset.col(0) != receiver);
                break;

            // AB-XY
            case 5 :
                // Find all XY dyads 
                psdyads = find(riskset.col(0) != sender && 
                    riskset.col(0) != receiver && riskset.col(1) != sender && 
                    riskset.col(1) != receiver);
                break;
            
            // AB-AY
            case 6 :
                // Find all AY dyads 
                psdyads = find(riskset.col(0) == sender &&                  
                    riskset.col(1) != receiver && riskset.col(1) != sender);
                break;              
        }

        // Set the statistic to one at the next event for those dyads that 
        // create the respective p-shift
        for(arma::uword d = 0; d < psdyads.n_elem; ++d) {
            stat(i+1, psdyads(d)) = 1.0;
        }
    }

    // Output
    return stat;
}

//' inertiaMW
//'
//' A function to compute the inertia effect.
//'
//' @param full_edgelist 3-column edgelist (time, sender, receiver)
//' @param window_edgelist 3-column edgelist (time, sender, receiver)
//' @param window_length numeric value.
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2).
//' @param full_weights vector (length full_edgelist). 
//' @param standardize logical. 
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' windows <- data.frame(start = seq(0, 900, 75), end = seq(100, 1000, 75))
//' data(edgelistU)
//' window_edgelist <- edgelistU[edgelistU$time > windows$start[2] & 
//'     edgelistU$time <= windows$end[2],]
//' out <- prepER(edgelist = edgelistU, directed = FALSE)
//' full_el <- out$edgelist
//' rs <- out$riskset
//' ac <- out$actors
//' out <- prepER(window_edgelist, directed = FALSE, actors = ac[,2])
//' window_el <- out$edgelist
//' stat <- inertiaMW(full_edgelist = full_el, window_edgelist = window_el, 
//'     window_length = 100, riskset = rs, 
//'     full_weights = rep(1, nrow(full_el)), standardize = FALSE)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat inertiaMW(arma::mat full_edgelist, arma::mat window_edgelist, 
    double window_length, arma::mat riskset, arma::vec full_weights, 
    bool standardize) {
    // Storage space and fill with zeros
    arma::mat stat(window_edgelist.n_rows, riskset.n_rows, fill::zeros);

    // For loop over the events that fall within the window
    for(arma::uword i = 0; i < window_edgelist.n_rows; ++i) {
        // Determine the past 
        double time = window_edgelist(i,0);
        arma::uvec indices = 
            find((full_edgelist.col(0) > (time - window_length)) && 
            (full_edgelist.col(0) < time));
        arma::mat past = full_edgelist.rows(indices);
        arma::mat past_weights = full_weights(indices);

        // Create storage space 
        arma::rowvec thisrow(riskset.n_rows, fill::zeros);

        // For loop over dyads
        for(arma::uword r = 0; r < riskset.n_rows; ++r) {
            //Sender and receiver of the dyad
            arma::uword sender = riskset(r, 0);
            arma::uword receiver = riskset(r, 1); 

            // Determine the intensity of past interactions
            arma::uvec indices2 = find(past.col(1) == sender && 
                past.col(2) == receiver);
            arma::vec past_intensity = past_weights(indices2);
            thisrow(r) = sum(past_intensity);
        }

        //Change the row in the statistic
        stat.row(i) = thisrow;
    }

    // Standardize effect if requested
    if(standardize) {
        for(arma::uword i = 0; i < window_edgelist.n_rows; ++ i) {
            if(stddev(stat.row(i)) > 0) {
                stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
                    stddev(stat.row(i));
            }
        }
    }

    // Output
    return stat;
}

//' triadUMW
//'
//' A function to compute the (unique) shared partners effect for undirected 
//' relational events when fitting a moving window REM.
//'
//' @param actors vector with numeric actor IDs (correspond to edgelist,
//' riskset)
//' @param full_edgelist 3-column edgelist (time, sender, receiver)
//' @param window_edgelist 3-column edgelist (time, sender, receiver)
//' @param window_length numeric value.
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//' @param unique_sp logical value
//' @param standardize logical value
//'
//' @return matrix (time x dyad)
//' 
//' @examples
//' data(edgelistU)
//' windows <- data.frame(start = seq(0, 900, 75), end = seq(100, 1000, 75))
//' window_edgelist <- edgelistU[edgelistU$time > windows$start[2] & 
//'     edgelistU$time <= windows$end[2],]
//' out <- prepER(edgelist = edgelistU, directed = FALSE)
//' full_el <- out$edgelist
//' rs <- out$riskset
//' ac <- out$actors
//' out <- prepER(window_edgelist, directed = FALSE, actors = ac[,2])
//' window_el <- out$edgelist
//' stat <- triadUMW(actors = ac[,1], full_edgelist = full_el, 
//'     window_edgelist = window_el, window_length = 100, riskset = rs, 
//'     unique_sp = FALSE, standardize = FALSE)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat triadUMW(arma::vec actors, arma::mat full_edgelist, 
    arma::mat window_edgelist, double window_length, arma::mat riskset, 
    bool unique_sp, bool standardize) {

    //Storage space
    //Statistic matrix (output))
    arma::mat stat(window_edgelist.n_rows, riskset.n_rows, fill::zeros);

    // For loop over the events that fall within the window
    for(arma::uword i = 0; i < window_edgelist.n_rows; ++i) {
        // Determine the past 
        double time = window_edgelist(i,0);
        arma::uvec indices = 
            find((full_edgelist.col(0) > (time - window_length)) && (full_edgelist.col(0) < time));
        arma::mat past = full_edgelist.rows(indices);

        // Create storage space
        arma::rowvec thisrow(riskset.n_rows, fill::zeros);
        arma::mat adj(max(actors), max(actors), fill::zeros);

        // Fill adjacency matrix based on the past
        for(arma::uword j = 0; j < past.n_rows; ++ j) {
            //Actors of the event
            arma::uword actor1 = past(j, 1);  
            arma::uword actor2 = past(j, 2);  
            //Update the adjacency matrix
            adj(actor1 - 1, actor2 - 1) += 1; 

            //If "unique_sp" is requested, dichotomize the adjacency matrix
            if(unique_sp) {adj.replace(2, 1);}
        }
        
        //For loop over dyads
        for(arma::uword d = 0; d < riskset.n_rows; ++d) {
            //Actors in the dyad
            arma::uword actor1D = riskset(d, 0);
            arma::uword actor2D = riskset(d, 1);

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
        //Save the statistic for this timepoint
        stat.row(i) = thisrow;
    }

    // Standardize effect if requested
    if(standardize) {
        for(arma::uword i = 0; i < window_edgelist.n_rows; ++ i) {
            if(stddev(stat.row(i)) > 0) {
                stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
                    stddev(stat.row(i));
            }
        }
    }

    //Output
    return(stat);
}

//' typestat
//'
//' A function to compute a type effect/type dummy. 
//' @param edgelist 4-column edgelist (time, sender, receiver, type)
//' @param riskset 3-column riskset (sender/actor 1, receiver/actor 2, type)
//' @param type numeric value
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistDT)
//' out <- prepER(edgelist = edgelistDT, type = TRUE)
//' edgelist <- out$edgelist
//' riskset <- out$riskset
//' types <- unique(edgelist[,4])
//' stat <- typestat(edgelist, riskset, types[1])
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat typestat(arma::mat edgelist, arma::mat riskset, arma::uword type) {
    // Storage space and fill with zeros
    arma::mat stat(edgelist.n_rows, riskset.n_rows, fill::zeros);
    arma::rowvec onerow(riskset.n_rows, fill::zeros);

    // Find event types in the riskset
    arma::uvec indices = find(riskset.col(2) == type);
    // Replace the zeros for these events with ones
    onerow(indices).replace(0,1);
    // Copy the row for each event
    stat.each_row() = onerow;

    // Output
    return(stat);
}


//' inertia_type
//'
//' A function to compute the inertia_type effect.
//' 
//' @param edgelist 4-column edgelist (time, sender, receiver, type)
//' @param riskset 3-column riskset (sender/actor 1, receiver/actor 2, type)
//' @param weights vector (length edgelist) 
//' @param standardize logical 
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistDT)
//' out <- prepER(edgelist = edgelistDT, type = TRUE)
//' edgelist <- out$edgelist
//' riskset <- out$riskset
//' equal_weights <- rep(1, nrow(edgelist))
//' stat <- inertia_type(edgelist, riskset, equal_weights, standardize = FALSE)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat inertia_type(arma::mat edgelist, arma::mat riskset, 
    arma::vec weights, bool standardize) {
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
        //Type of the previous event
        arma::uword type = edgelist(i-1, 3);

        //Find the dyad's position in the riskset
        arma::uvec dyad = find(riskset.col(0) == sender && 
            riskset.col(1) == receiver && riskset.col(2) == type);
        
        //Add the intensity to the dyad's intensity count
        thisrow(dyad) += weights(i-1);

        //Change the row in the statistic
        stat.row(i) = thisrow;
    }

    // Standardize effect if requested
    if(standardize) {
        for(arma::uword i = 0; i < edgelist.n_rows; ++ i) {
            if(stddev(stat.row(i)) > 0) {
                stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
                    stddev(stat.row(i));
            }
        }
    }

    // Output
    return stat;
}

//' inertia_typeMW
//'
//' A function to compute the inertia_type effect for a moving window REM.
//'
//' @param full_edgelist 4-column edgelist (time, sender, receiver, type).
//' @param window_edgelist 4-column edgelist (time, sender, receiver, type).
//' @param window_length numeric value.
//' @param riskset 3-column riskset (sender/actor 1, receiver/actor 2, type).
//' @param full_weights vector (length full_edgelist). 
//' @param standardize logical. 
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' windows <- data.frame(start = seq(0, 900, 75), end = seq(100, 1000, 75))
//' data(edgelistUT)
//' window_edgelist <- edgelistUT[edgelistUT$time > windows$start[2] & 
//'     edgelistUT$time <= windows$end[2],]
//' out <- prepER(edgelist = edgelistUT, directed = FALSE, type = TRUE)
//' full_el <- out$edgelist
//' rs <- out$riskset
//' ac <- out$actors
//' out <- prepER(window_edgelist, directed = FALSE, type = TRUE, 
//'    actors = ac[,2])
//' window_el <- out$edgelist
//' equal_weights <- rep(1, nrow(full_el))
//' stat <- inertia_typeMW(full_edgelist = full_el, window_edgelist = 
//'     window_el, window_length = 100, riskset = rs, 
//'     full_weights = equal_weights, standardize = FALSE)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat inertia_typeMW(arma::mat full_edgelist, arma::mat window_edgelist, 
    double window_length, arma::mat riskset, arma::vec full_weights, 
    bool standardize) {
    // Storage space and fill with zeros
    arma::mat stat(window_edgelist.n_rows, riskset.n_rows, fill::zeros);

    // For loop over the events that fall within the window
    for(arma::uword i = 0; i < window_edgelist.n_rows; ++i) {
        // Determine the past 
        double time = window_edgelist(i,0);
        arma::uvec indices = 
            find((full_edgelist.col(0) > (time - window_length)) && 
            (full_edgelist.col(0) < time));
        arma::mat past = full_edgelist.rows(indices);
        arma::mat past_weights = full_weights(indices);

        // Create storage space 
        arma::rowvec thisrow(riskset.n_rows, fill::zeros);

        // For loop over dyads
        for(arma::uword r = 0; r < riskset.n_rows; ++r) {
            //Sender, receiver and dyad type
            arma::uword sender = riskset(r, 0);
            arma::uword receiver = riskset(r, 1); 
            arma::uword type = riskset(r, 2);

            // Determine the intensity of past interactions
            arma::uvec indices2 = find(past.col(1) == sender && 
                past.col(2) == receiver && past.col(3) == type);
            arma::vec past_intensity = past_weights(indices2);
            thisrow(r) = sum(past_intensity);
        }

        //Change the row in the statistic
        stat.row(i) = thisrow;
    }

    // Standardize effect if requested
    if(standardize) {
        for(arma::uword i = 0; i < window_edgelist.n_rows; ++ i) {
            if(stddev(stat.row(i)) > 0) {
                stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
                    stddev(stat.row(i));
            }
        }
    }

    // Output
    return stat;
}

//' triadU_type
//'
//' A function to compute the (unique) shared partners effect for undirected 
//' relational events with types. 
//'
//' @param actors vector with numeric actor IDs (correspond to edgelist,
//' riskset)
//' @param edgelist 4-column edgelist (time, sender, receiver, type)
//' @param riskset 3-column riskset (sender/actor 1, receiver/actor 2, type)
//' @param unique_sp logical value
//' @param standardize logical value
//'
//' @return matrix (time x dyad)
//' 
//' @examples
//' data(edgelistUT)
//' out <- prepER(edgelist = edgelistUT, directed = FALSE, type = TRUE)
//' el <- out$edgelist
//' rs <- out$riskset
//' ac <- out$actors[,1]
//' stat <- triadU_type(ac, el, rs, unique_sp = FALSE, standardize = FALSE)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat triadU_type(arma::vec actors, arma::mat edgelist, arma::mat riskset, 
    bool unique_sp, bool standardize) {

    //Storage space
    //(1) List with adjacency matrices per type
    arma::vec types = unique(riskset.col(2));
    arma::mat adj(max(actors), max(actors), fill::zeros);
    Rcpp::List adj_list = List::create(adj, adj);
    for(arma::uword i = 2; i < types.n_elem; ++i) {
        adj_list.push_back(adj);
    }
    
    //(2) Statistic matrix (output))
    arma::mat stat(edgelist.n_rows, riskset.n_rows, fill::zeros);
   
    //For loop over the sequence
    for(arma::uword i = 1; i < edgelist.n_rows; ++i) {
        //Actors of the previous event
        arma::uword actor1 = edgelist(i-1, 1);  
        arma::uword actor2 = edgelist(i-1, 2);  
        //Type of the previous event
        arma::uword type = edgelist(i-1, 3);
        
        //Update the adjacency matrix
        arma::uvec indextype = find(types==type);
        arma::mat update_adj = adj_list[indextype(0)];
        update_adj(actor1 - 1, actor2 - 1) += 1; 
        if(unique_sp) {update_adj.replace(2, 1);} 
        adj_list[indextype(0)] = update_adj; 
        
        //Actors that have a relation with the actors involved in the previous 
        //event
        arma::uvec relActors1 = find(update_adj.row(actor1-1) != 0);  
        arma::uvec relActors2 = find(update_adj.col(actor1-1) != 0);  
        arma::uvec relActors3 = find(update_adj.row(actor2-1) != 0);  
        arma::uvec relActors4 = find(update_adj.col(actor2-1) != 0);  
        arma::uvec relActors = join_cols(relActors1, relActors2);
        relActors = join_cols(relActors, relActors3); 
        relActors = join_cols(relActors, relActors4); 
        relActors = sort(unique(relActors));          

        //Copy the previous row
        arma::rowvec thisrow = stat.row(i-1);

        //For loop over dyads
        for(arma::uword d = 0; d < riskset.n_rows; ++d) {
            //Actors and type
            arma::uword actor1D = riskset(d, 0);
            arma::uword actor2D = riskset(d, 1);
            arma::uword typeD = riskset(d, 2);

            arma::uvec indextype2 = find(types==typeD);
            arma::mat adjD = adj_list[indextype2(0)];

            //Only change their statistic if both actors have a relation to the 
            //actors involved in the previous event
            if(any(relActors == (actor1D - 1)) &&
                any(relActors == (actor2D - 1))) {

                    //Communications actor1D
                    arma::rowvec com1Actor1Dtemp = adjD.row(actor1D-1);
                    arma::colvec com1Actor1D = conv_to<colvec>::from(com1Actor1Dtemp);
                    arma::colvec com2Actor1D = adjD.col(actor1D-1);
                    arma::vec comActor1D = com1Actor1D + com2Actor1D;
                    //Communications actor2D
                    arma::rowvec com1Actor2Dtemp = adjD.row(actor2D-1);
                    arma::colvec com1Actor2D = conv_to<colvec>::from(com1Actor2Dtemp);
                    arma::colvec com2Actor2D = adjD.col(actor2D-1);
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

    // Standardize effect if requested
    if(standardize) {
        for(arma::uword i = 0; i < edgelist.n_rows; ++ i) {
            if(stddev(stat.row(i)) > 0) {
                stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
                    stddev(stat.row(i));
            }
        }
    }

    //Output
    return(stat);
}

//' triadU_typeMW
//'
//' A function to compute the (unique) shared partners effect for undirected 
//' relational events with types for a moving window REM. 
//'
//' @param actors vector with numeric actor IDs (correspond to edgelist,
//' riskset)
//' @param full_edgelist 4-column edgelist (time, sender, receiver, type)
//' @param window_edgelist 4-column edgelist (time, sender, receiver, type)
//' @param window_length numeric value.
//' @param riskset 3-column riskset (sender/actor 1, receiver/actor 2, type)
//' @param unique_sp logical value
//' @param standardize logical value
//'
//' @return matrix (time x dyad)
//' 
//' @examples
//' data(edgelistUT)
//' windows <- data.frame(start = seq(0, 900, 75), end = seq(100, 1000, 75))
//' window_edgelist <- edgelistUT[edgelistUT$time > windows$start[2] & 
//'     edgelistUT$time <= windows$end[2],]
//' out <- prepER(edgelist = edgelistUT, directed = FALSE, type = TRUE)
//' full_el <- out$edgelist
//' rs <- out$riskset
//' ac <- out$actors
//' out <- prepER(window_edgelist, directed = FALSE, type = TRUE, 
//'     actors = ac[,2])
//' window_el <- out$edgelist
//' stat <- triadU_typeMW(ac[,1], full_edgelist = full_el, 
//'     window_edgelist = window_el, window_length = 100, riskset = rs, 
//'     unique_sp = FALSE, standardize = FALSE)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat triadU_typeMW(arma::vec actors, arma::mat full_edgelist, 
    arma::mat window_edgelist, double window_length, arma::mat riskset, 
    bool unique_sp, bool standardize) {

    //Storage space
    //Statistic matrix (output))
    arma::mat stat(window_edgelist.n_rows, riskset.n_rows, fill::zeros);

    // Event types
    arma::vec types = unique(riskset.col(2));
   
    // For loop over the events that fall within the window
    for(arma::uword i = 1; i < window_edgelist.n_rows; ++i) {
        // Determine the past 
        double time = window_edgelist(i,0);
        arma::uvec indices = 
            find((full_edgelist.col(0) > (time - window_length)) && (full_edgelist.col(0) < time));
        arma::mat past = full_edgelist.rows(indices);

        // Create storage space
        arma::rowvec thisrow(riskset.n_rows, fill::zeros);
        arma::mat adj(max(actors), max(actors), fill::zeros);
        Rcpp::List adj_list = List::create(adj, adj);
        for(arma::uword j = 2; j < types.n_elem; ++j) {
            adj_list.push_back(adj);
        }

        // Fill adjacency matrices based on the past
        for(arma::uword j = 0; j < past.n_rows; ++ j) {
            //Actors and type of the event
            arma::uword actor1 = past(j, 1);  
            arma::uword actor2 = past(j, 2);  
            arma::uword type = past(j, 3);
            
            //Update the adjacency matrix
            arma::uvec index = find(types == type);
            arma::mat update_adj = adj_list[index(0)];
            update_adj(actor1 - 1, actor2 - 1) += 1; 

            //If "unique_sp" is requested, dichotomize the adjacency matrix
            if(unique_sp) {update_adj.replace(2, 1);}

            //Save the updated adjacency matrix
            adj_list[index(0)] = update_adj; 
        }

        //For loop over dyads
        for(arma::uword d = 0; d < riskset.n_rows; ++d) {
            //Actors and type in the dyad
            arma::uword actor1D = riskset(d, 0);
            arma::uword actor2D = riskset(d, 1);
            arma::uword typeD = riskset(d, 2);

            //Get the adjacency matrix for this type
            arma::uvec index2 = find(types==typeD);
            arma::mat adjD = adj_list[index2(0)];

            //Communications actor1D
            arma::rowvec com1Actor1Dtemp = adjD.row(actor1D-1);
            arma::colvec com1Actor1D = conv_to<colvec>::from(com1Actor1Dtemp);
            arma::colvec com2Actor1D = adjD.col(actor1D-1);
            arma::vec comActor1D = com1Actor1D + com2Actor1D;
            //Communications actor2D
            arma::rowvec com1Actor2Dtemp = adjD.row(actor2D-1);
            arma::colvec com1Actor2D = conv_to<colvec>::from(com1Actor2Dtemp);
            arma::colvec com2Actor2D = adjD.col(actor2D-1);
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

        //Save the statistic for this timepoint
        stat.row(i) = thisrow;
    }

    // Standardize effect if requested
    if(standardize) {
        for(arma::uword i = 0; i < window_edgelist.n_rows; ++ i) {
            if(stddev(stat.row(i)) > 0) {
                stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
                    stddev(stat.row(i));
            }
        }
    }

    //Output
    return(stat);
}


//' recency
//'
//' A function to compute the rank-based recency effect, as in section 2.2.5 of Butts (2008).
//' 
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param actors vector of integers indicating the identity of the actors in the network
//' @param type equals 1 for outgoing and 2 for incoming recency effect
//'
//' @return matrix (time x dyad)
//'
//' @examples
//' data(edgelistD)
//' out <- prepER(edgelist = edgelistD)
//' edgelist <- out$edgelist
//' actors <- out$actors$id
//' type <- 1
//' stat <- recency(edgelist, actors, type)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat recency(arma::mat edgelist, arma::vec actors, double type){
  //this cube will be filled with the ranks at each time
  arma::cube output(max(actors), max(actors), edgelist.n_rows, fill::zeros);
  
  //these are just auxiliary structures that'll be used inside the loop
  arma::mat aux(max(actors), max(actors), fill::zeros);
  arma::vec auxCol(max(actors));
  
  for(arma::uword k = 0; k < edgelist.n_rows; ++k){
    
      if(k == 0){continue;} //the statistic has to be zero for everybody at time t = 1
      else {
        
        arma::uword j = edgelist(k-1,1); //sender index
        arma::uword i = edgelist(k-1,2); //receiver index
        
        if(k == 1){ //case where time t = 1
          aux(i-1,j-1) = 1;
          output.slice(k) = aux;
        } else {
          aux = output.slice(k-1);
          auxCol = aux.col(j-1);
          arma::uword rank = auxCol(i-1);
          
          //this block takes care of the 4 situations needed in order to correctly get the ranks in the network
          if(rank == 1){ //case where the current interaction was the same as the previous one
            output.slice(k) = aux;  
            continue;
          } else if(sum(auxCol) == 0){//case where it's first interaction of an actor
            aux(i-1,j-1) = 1;
          } else if(rank == 0 && sum(auxCol) >= 1){//case where it's the first time an actor interacts with another
            arma::uvec mostRecent = find(auxCol > 0);
            auxCol(mostRecent) = auxCol(mostRecent) + 1;
            auxCol(i-1) = 1;
            aux.col(j-1) = auxCol;
          } else if(rank > 1){//case where an actor is not interacting with another for the first time
            arma::uvec mostRecent = find(auxCol > 0 && auxCol < rank);
            auxCol(mostRecent) = auxCol(mostRecent) + 1;
            auxCol(i-1) = 1;
            aux.col(j-1) = auxCol;
          }
        }
      }
    //updating the ranks  
    output.slice(k) = aux;  
  }
  
  //this is the number of edges in the network
  arma::uword permutations = max(actors)*(max(actors) - 1);
  //the output of the function, it will be transposed in the end
  arma::mat recency(permutations, edgelist.n_rows);
  //another auxiliary structure
  arma::vec auxRows(permutations);
  
  //Only one of this will be the output of the function, this was made to prevent having to create two functions
  
  for(arma::uword i = 0; i < output.n_slices; ++i){
    double value = -1;
    arma::vec V;
    output.slice(i).diag(0).fill(value);
    if(type == 1){//calculates the outgoing recency
      V = vectorise(output.slice(i).t());
    }
    if(type == 2){//calculates the incoming recency
      V = vectorise(output.slice(i));
    }  
    arma::uvec ind1 = find(V >= 0);
    auxRows = V(ind1); //correcting the fact that rank = Inf was not set for actors who haven't interacted
    arma::uvec ind2 = find(auxRows);
    auxRows(ind2) = 1/auxRows(ind2);
    recency.col(i) = auxRows;
  }

  //transposing the output to be in the same form as the other functions in the package
  return(recency.t());
}  