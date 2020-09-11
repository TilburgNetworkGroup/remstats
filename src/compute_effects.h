#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// standardize
// 
// Can be used to standardize a statistic matrix per row (=timepoint). 
// 
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat standardize(arma::mat stat) {
    for(arma::uword i = 0; i < stat.n_rows; ++i) {
            if(stddev(stat.row(i)) > 0) {
                stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
                    stddev(stat.row(i));
            }
    }

    return stat;
}

// divide_by_past
// 
// Can be used to scale a statistic matrix by dividing by the number of past 
// events.  
// 
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat divide_by_past(arma::mat stat) {
    for(arma::uword i = 1; i < stat.n_rows; ++i) {
        stat.row(i) = stat.row(i)/i;
    }

    return stat;
}

// divide_by_2past
// 
// Can be used to scale a statistic matrix by dividing by two times the number 
// of past events.  
// 
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat divide_by_2past(arma::mat stat) {
    for(arma::uword i = 1; i < stat.n_rows; ++i) {
        stat.row(i) = stat.row(i)/(2*i);
    }

    return stat;
}

// compute_actorEffect
// 
// Computes exogenous actor effects (= send and receive)  
// 
//  values: matrix with an exogenous actor attribute (id, time, value)
//  type: type of actor effect that needs to be computed (1 = send, 2 = 
// receive)
//  edgelist: matrix with edgelist (time, sender, receiver, (type), 
// riskset position)
//  riskset: matrix with riskset (sender, receiver, (type))
//  start: integer value referring to the first row + 1 in the edgelist 
// for which effects have to be computed 
//  stop: integer value referring to the last row + 1 in the edgelist 
// for which effects have to be computed 
//
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat compute_actorEffect(const arma::mat& values, int type, 
    const arma::mat& edgelist, const arma::mat& riskset, int start, int stop) {

    // Extract small edgelist
    arma::mat small_edgelist = edgelist.rows((start-1), (stop-1));

    // Storage space and fill with zeros
    arma::mat stat(small_edgelist.n_rows, riskset.n_rows, arma::fill::zeros); 

    // First time point
    double time = small_edgelist(0,0);

    // Set the first row
    // For loop over dyads
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        // Find the relevant actor
        arma::uword actor = 0;
        if(type == 1) {actor = riskset(i, 0);} // Sender
        if(type == 2) {actor = riskset(i, 1);} // Receiver
        
        // Find the first value for this actor
        arma::uvec index = find(values.col(0) == actor && 
            values.col(1) <= time);
        arma::mat actor_values = values.rows(index);
        arma::uword max_index = index_max(actor_values.col(1));
        stat(0,i) = actor_values(max_index, 2);
    }

    // Find the unique change timepoints
    arma::vec changetimes = sort(unique(values.col(1)));
    changetimes = changetimes(find(changetimes!=0));
    arma::uword counter = 0;
    
    // For loop over the sequence
    for(arma::uword m = 0; m < small_edgelist.n_rows; ++m) {
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
            if(small_edgelist(m, 0) > changetimes(counter)) {
                // Update all changes in between
                while((counter < changetimes.n_elem) && 
                    (small_edgelist(m, 0) > changetimes(counter))) {
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

// compute_dyadEffect
// 
// Computes exogenous dyad effects (= same, difference, mean, minimum, 
// maximum, equate)  
// 
//  values: matrix with an exogenous actor attribute (id, time, value)
//  type: type of dyad effect that needs to be computed (1 = same, 2 = 
// difference, 3 = mean, 4 = minimum, 5 = maximum, 6 = equate)
//  edgelist: matrix with edgelist (time, sender/actor1, receiver/
// actor2, (type), riskset position)
//  riskset: matrix with riskset (sender/actor1, receiver/actor2, (type))
//  start: integer value referring to the first row + 1 in the edgelist 
// for which effects have to be computed 
//  stop: integer value referring to the last row + 1 in the edgelist 
// for which effects have to be computed 
//  equal_val: value when type is equate
//
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat compute_dyadEffect(const arma::mat& values, int type, 
    const arma::mat& edgelist, const arma::mat& riskset, int start, int stop, double equal_val) {

    // Extract small edgelist
    arma::mat small_edgelist = edgelist.rows((start-1), (stop-1));

    // Storage space and fill with zeros
    arma::mat stat(small_edgelist.n_rows, riskset.n_rows, arma::fill::zeros); 

    // Storage space for the current covariate values
    arma::vec current_ac1(riskset.n_rows, arma::fill::zeros);
    arma::vec current_ac2(riskset.n_rows, arma::fill::zeros);

    // First time point
    double time = small_edgelist(0,0);
    
    // Set the first row 
    // For loop over dyads
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        // Find the relevant actors
        arma::uword actor1 = riskset(i, 0);
        arma::uword actor2 = riskset(i, 1);

        // Find the values for actor1
        arma::uvec index1 = find(values.col(0) == actor1 && 
            values.col(1) <= time);
        arma::mat actor1_values = values.rows(index1);
        arma::uword max_index1 = index_max(actor1_values.col(1));
        current_ac1(i) = actor1_values(max_index1, 2);

        // Find the values for actor2
        arma::uvec index2 = find(values.col(0) == actor2 && 
            values.col(1) <= time);
        arma::mat actor2_values = values.rows(index2);
        arma::uword max_index2 = index_max(actor2_values.col(1));
        current_ac2(i) = actor2_values(max_index2, 2);

        // Are these values equal?
        if(type == 1) {stat(0, i) = (current_ac1(i)==current_ac2(i));}
        // What is the difference between these values?
        if(type == 2) {stat(0, i) = current_ac1(i)-current_ac2(i);}
       
        arma::vec both = {current_ac1(i), current_ac2(i)};
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
    for(arma::uword m = 0; m < small_edgelist.n_rows; ++m) {
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
            if(small_edgelist(m, 0) > changetimes(counter)) {
                // Update all changes in between
                while((counter < changetimes.n_elem) && 
                    (small_edgelist(m, 0) > changetimes(counter))) {
                        
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

// compute_inertia
// 
// Computes statistic for an inertia effect. 
// 
//  edgelist: matrix with edgelist (time, sender/actor1, receiver/
// actor2, (type), riskset position)
//  riskset: matrix with riskset (sender/actor1, receiver/actor2, (type))
//  memory_value: numeric value indicating the time after which events 
// are 'forgotten'
//  with_type: logical value indicating whether events of different 
// types should be counted separately
//  event_weights: vector of length edgelist with event weights
//  start: integer value referring to the first row + 1 in the edgelist 
// for which effects have to be computed 
//  stop: integer value referring to the last row + 1 in the edgelist 
// for which effects have to be computed 
//
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat compute_inertia(const arma::mat& edgelist, const arma::mat& riskset, 
    double memory_value, bool with_type, const arma::vec& event_weights, 
    int start, int stop) {

    // Get the position of the riskset position column
    int rp = edgelist.n_cols-1;

    // Extract small edgelist
    arma::mat small_edgelist = edgelist.rows((start-1), (stop-1));

    // Storage space and fill with zeros
    arma::mat stat(small_edgelist.n_rows, riskset.n_rows, arma::fill::zeros);  

    // For loop over the small edgelist
    for(arma::uword i = 0; i < small_edgelist.n_rows; ++i) {
        //Storage space and fill with zeros
        arma::rowvec thisrow(riskset.n_rows, arma::fill::zeros);

        //Determine the past
        double time = small_edgelist(i,0);
        double bound = time-memory_value;
        arma::uvec past = find(edgelist.col(0) < time && 
            edgelist.col(0) > bound);

        //Sum the intensity of events in the past
        for(arma::uword j = 0; j < past.n_elem; ++j) {
            if(with_type || (edgelist.n_cols == 4)) {
                int dyad = edgelist(past(j), rp);
                thisrow(dyad-1) += event_weights(past(j));
            } else {
                int sender = edgelist(past(j), 1);
                int receiver = edgelist(past(j), 2);
                arma::uvec dyads = find(riskset.col(0) == sender && 
                    riskset.col(1) == receiver);
                thisrow(dyads) += event_weights(past(j));
            }
        }

        //Change the row in the statistic
        stat.row(i) = thisrow;
    }

    // Output
    return stat;
}

// compute_reciprocity
// 
// Computes statistic for a reciprocity effect. 
// 
//  edgelist: matrix with edgelist (time, sender/actor1, receiver/
// actor2, (type), riskset position)
//  riskset: matrix with riskset (sender/actor1, receiver/actor2, (type))
//  memory_value: numeric value indicating the time after which events 
// are 'forgotten'
//  with_type: logical value indicating whether events of different 
// types should be counted separately
//  event_weights: vector of length edgelist with event weights
//  start: integer value referring to the first row + 1 in the edgelist 
// for which effects have to be computed 
//  stop: integer value referring to the last row + 1 in the edgelist 
// for which effects have to be computed 
//
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat compute_reciprocity(const arma::mat& edgelist, 
    const arma::mat& riskset, double memory_value, bool with_type, 
    const arma::vec& event_weights, int start, int stop) {
    
    // Extract small edgelist
    arma::mat small_edgelist = edgelist.rows((start-1), (stop-1));

    // Storage space and fill with zeros
    arma::mat stat(small_edgelist.n_rows, riskset.n_rows, arma::fill::zeros);

    // For loop over the small edgelist
    for(arma::uword i = 0; i < small_edgelist.n_rows; ++i) {
        //Storage space and fill with zeros
        arma::rowvec thisrow(riskset.n_rows, arma::fill::zeros);

        //Determine the past
        double time = small_edgelist(i,0);
        double bound = time-memory_value;
        arma::uvec past = find(edgelist.col(0) < time && 
            edgelist.col(0) > bound);

        //Sum the intensity of events in the past
        for(arma::uword j = 0; j < past.n_elem; ++j) {
            int sender = edgelist(past(j), 1);
            int receiver = edgelist(past(j), 2);
            if(!with_type) {
                arma::uvec dyads = find(riskset.col(0) == receiver && 
                    riskset.col(1) == sender);
                thisrow(dyads) += event_weights(past(j));
            } else {
                int type = edgelist(past(j), 3);
                arma::uvec dyads = find(riskset.col(0) == receiver && 
                    riskset.col(1) == sender && riskset.col(2) == type);
                thisrow(dyads) += event_weights(past(j));
            }
        }

        //Change the row in the statistic
        stat.row(i) = thisrow;
    }

    // Output
    return stat;
}

// compute_degree
// 
// Computes statistic for a degree effect (indegreeSender, indegreeReceiver, 
// outdegreeSender, outdegreeReceiver, totaldegreeSender, totaldegreeReceiver)
// 
//  type: type of degree effect (1 = indegreeSender, 2 = 
// indegreeReceiver, 3 = outdegreeSender, 4 = outdegreeReceiver, 5 = 
// totaldegreeSender, 6 = totaldegreeReceiver)
//  edgelist: matrix with edgelist (time, sender/actor1, receiver/
// actor2, (type), riskset position)
//  riskset: matrix with riskset (sender/actor1, receiver/actor2, (type))
//  memory_value: numeric value indicating the time after which events 
// are 'forgotten'
//  with_type: logical value indicating whether events of different 
// types should be counted separately
//  event_weights: vector of length edgelist with event weights
//  start: integer value referring to the first row + 1 in the edgelist 
// for which effects have to be computed 
//  stop: integer value referring to the last row + 1 in the edgelist 
// for which effects have to be computed 
//
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat compute_degree(int type, const arma::mat& edgelist, 
    const arma::mat& riskset, double memory_value, bool with_type, 
    const arma::vec& event_weights, int start, int stop) {
	
	// Extract small edgelist
	arma::mat small_edgelist = edgelist.rows((start-1), (stop-1));
	
	// Storage space and fill with zeros
	arma::mat stat(small_edgelist.n_rows, riskset.n_rows, arma::fill::zeros);
	
	// For loop over the sequence
	for(arma::uword i = 0; i < small_edgelist.n_rows; ++i) {
		//Storage space and fill with zeros
		arma::rowvec thisrow(riskset.n_rows, arma::fill::zeros);
		
		// Determine the past
		double time = small_edgelist(i,0);
		double bound = time-memory_value;
		arma::uvec past = find(edgelist.col(0) < time && 
			edgelist.col(0) > bound);
		
		// For loop over events in the past
		for(arma::uword j = 0; j < past.n_elem; ++j) {
			int sender = edgelist(past(j), 1);
			int receiver = edgelist(past(j), 2);
			
			arma::uvec dyads = {0};
			
			if(!with_type) {
				// indegree sender
				if(type == 1) {dyads = find(riskset.col(0) == receiver);}
				// indegree receiver
				if(type == 2) {dyads = find(riskset.col(1) == receiver);}
				// outdegree sender
				if(type == 3) {dyads = find(riskset.col(0) == sender);}
				// outdegree receiver
				if(type == 4) {dyads = find(riskset.col(1) == sender);}
				// total degree sender
				if(type == 5) {dyads = find(riskset.col(0) == sender || 
                    riskset.col(0) == receiver);}
				// total degree receiver
				if(type == 6) {dyads = find(riskset.col(1) == sender || 
                    riskset.col(1) == receiver);}
			} else {
				int eventtype = edgelist(past(j), 3);
				if(type == 1) {dyads = find(riskset.col(0) == receiver && 
                    riskset.col(2) == eventtype);}
				if(type == 2) {dyads = find(riskset.col(1) == receiver && 
                    riskset.col(2) == eventtype);}
				if(type == 3) {dyads = find(riskset.col(0) == sender && 
                    riskset.col(2) == eventtype);}
				if(type == 4) {dyads = find(riskset.col(1) == sender && 
                    riskset.col(2) == eventtype);}
				if(type == 5) {dyads = find((riskset.col(0) == sender || 
                    riskset.col(0) == receiver) && 
                    riskset.col(2) == eventtype);}
				if(type == 6) {dyads = find((riskset.col(1) == sender || 
                    riskset.col(1) == receiver) && 
                    riskset.col(2) == eventtype);}
			}
			
			thisrow(dyads) += event_weights(past(j));
		}
		
		//Change the row in the statistic
		stat.row(i) = thisrow;       
	}
	
	// Output
	return stat;  
}

// compute_triad
// 
// Computes statistic for a triad effect (otp, itp, osp, isp, sp, spUnique)
// 
//  type: type of triad effect (1 = otp, 2 = itp, 3 = osp, 4 = isp, 5 = 
// sp, 6 = spUnique)
//  edgelist: matrix with edgelist (time, sender/actor1, receiver/
// actor2, (type), riskset position)
//  riskset: matrix with riskset (sender/actor1, receiver/actor2, (type))
//  memory_value: numeric value indicating the time after which events 
// are 'forgotten'
//  with_type: logical value indicating whether events of different 
// types should be counted separately
//  event_weights: vector of length edgelist with event weights
//  start: integer value referring to the first row + 1 in the edgelist 
// for which effects have to be computed 
//  stop: integer value referring to the last row + 1 in the edgelist 
// for which effects have to be computed 
//
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat compute_triad(int type, const arma::mat& edgelist, 
    const arma::mat& riskset, double memory_value, bool with_type, 
    const arma::vec& event_weights, int start, int stop) {

    // Extract actors and types
    arma::vec actors = sort(unique(join_cols(riskset.col(0), riskset.col(1))));
    arma::vec types = {1};
    if(with_type) {
        types = sort(unique(riskset.col(2)));
    }

    // Extract small edgelist
    arma::mat small_edgelist = edgelist.rows((start-1), (stop-1));

    // Storage space and fill with zeros
    // Statistic (output)
    arma::mat stat(small_edgelist.n_rows, riskset.n_rows, arma::fill::zeros);
    // Statistic row (helper storage space)
    arma::rowvec thisrow(riskset.n_rows, arma::fill::zeros); 
    // Array with adjacency matrices (actor x actor x event type; helper 
    // storage space: stores the adjacency matrices of the previous run)
    arma::cube prev_adj(actors.n_elem, actors.n_elem, types.n_elem, 
        arma::fill::zeros);     

    // For loop over the small edgelist
    for(arma::uword i = 0; i < small_edgelist.n_rows; ++i) {
        //Copy previous row
        if(i>0) {thisrow = stat.row(i-1);}

        // Determine the past
        double time = small_edgelist(i,0);
        double bound = time-memory_value;
        arma::uvec past = find(edgelist.col(0) < time && 
            edgelist.col(0) > bound);

        //Sum the intensity of events in the past
        arma::cube adj(actors.n_elem, actors.n_elem, types.n_elem, 
            arma::fill::zeros); 

        for(arma::uword j = 0; j < past.n_elem; ++j) {
            int eventsender = edgelist(past(j), 1);
            int eventreceiver = edgelist(past(j), 2);
            int eventtype = 1;
            if(with_type) {eventtype = edgelist(past(j), 3);}
            // otp, itp, osp, isp
            if((type == 1) || (type == 2) || (type == 3) || (type == 4)) {
                adj(eventsender-1, eventreceiver-1, eventtype-1) += 
                    event_weights(past(j));
            } 
            // sp and spUnique
            if((type == 5) || (type == 6)) {
                adj(eventsender-1, eventreceiver-1, eventtype-1) += 
                    event_weights(past(j));
                adj(eventreceiver-1, eventsender-1, eventtype-1) += 
                    event_weights(past(j));
                
                // spUnique
                if(type == 6) {
                   adj.replace(2, 1); 
                }
            }       
        }

        //Compare with previous adj
        arma::mat send_change(actors.n_elem, types.n_elem);
        arma::mat receive_change(actors.n_elem, types.n_elem);
        for(arma::uword t = 0; t < types.n_elem; ++t) {
            arma::mat adj_mat = adj.slice(t);
            arma::mat prev_adj_mat = prev_adj.slice(t);
            for(arma::uword h = 0; h < actors.n_elem; ++h) {
                send_change(h, t) = approx_equal(adj_mat.row(h), 
                    prev_adj_mat.row(h), "absdiff", 0.0001);
                receive_change(h, t) = approx_equal(adj_mat.col(h), 
                    prev_adj_mat.col(h), "absdiff", 0.0001);
            }
        }
        // Note: approx_equal turns a one if equal (thus NO change) and a zero if 
        // not equal (thus CHANGE)

        //Update prev_adj
        prev_adj = adj;

        //For loop over dyads
        for(arma::uword d = 0; d < riskset.n_rows; ++d) {
            int dyadsender = riskset(d, 0);
            int dyadreceiver = riskset(d, 1);
            int dyadtype = 1;
            if(with_type) {dyadtype = riskset(d,2);}

            //Does the statistic change for this dyad?
            if((send_change(dyadsender-1, dyadtype-1) == 0) || 
                    (send_change(dyadreceiver-1, dyadtype-1) == 0) || 
                    (receive_change(dyadsender-1, dyadtype-1) == 0) || 
                    (receive_change(dyadreceiver-1, dyadtype-1) == 0)) {

                // Recompute the statistic based on the past
                double dyadstat = 0.0;
                
                arma::mat adj_mat = adj.slice(dyadtype-1);
                arma::rowvec send_by_sender = adj_mat.row(dyadsender-1);
                arma::colvec received_by_receiver = adj_mat.col(dyadreceiver-1);
                arma::rowvec send_by_receiver = adj_mat.row(dyadreceiver-1);
                arma::colvec received_by_sender = adj_mat.col(dyadsender-1);

                // For loop over actors
                for(arma::uword h = 0; h < actors.n_elem; ++h) {
                    arma::vec temp = {0};
                        
                    // otp
                    if(type == 1) {
                        temp = {send_by_sender(h), received_by_receiver(h)};
                    }
                        
                    // itp
                    if(type == 2) {
                        temp = {received_by_sender(h), send_by_receiver(h)};
                    }
                        
                    // osp/sp/spUnique
                    if(type == 3 || type == 5 || type == 6) {
                        temp = {send_by_sender(h), send_by_receiver(h)};
                    }
                        
                    // isp
                    if(type == 4) {
                        temp = {received_by_sender(h), received_by_receiver(h)};
                    }
                        
                    dyadstat += min(temp);
                }

                thisrow(d) = dyadstat;  
            }
        }

        //Change the row in the statistic
        stat.row(i) = thisrow; 
    }   

    return stat;   
}

// compute_pshift
// 
// Computes statistic for a p-shift effect (AB-BA, AB-BY, AB-XA, AB-XB, AB-XY, 
// AB-AY)
// 
//  type: type of p-shift effect (1 = AB-BA, 2 = AB-BY, 3 = AB-XA, 4 = 
// AB-XB, 5 = AB-XY, 6 = AB-AY)
//  edgelist: matrix with edgelist (time, sender/actor1, receiver/
// actor2, (type), riskset position)
//  riskset: matrix with riskset (sender/actor1, receiver/actor2, (type))
//  with_type: logical value indicating whether events of different 
// types should be counted separately
//  start: integer value referring to the first row + 1 in the edgelist 
// for which effects have to be computed 
//  stop: integer value referring to the last row + 1 in the edgelist 
// for which effects have to be computed 
//
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat compute_pshift(int type, const arma::mat& edgelist, 
    const arma::mat& riskset, bool with_type, int start, int stop) {

    // Extract small edgelist
    arma::mat small_edgelist = edgelist.rows((start-1), (stop-1));

    // Storage space and arma::fill with zeros
    arma::mat stat(small_edgelist.n_rows, riskset.n_rows, arma::fill::zeros);

    // For loop over events
    for(arma::uword i = 0; i < small_edgelist.n_rows; ++i) {
        //Time of the current event
        double time = small_edgelist(i, 0); 
        //Position of the current event in the full edgelist
        arma::uvec current = arma::find(edgelist.col(0) == time);
        // Position of the last event in the full edgelist
        int last = current(0) - 1;

        // If the current event is the first event in the edgelist, continue to 
        // the next iteration
        if(last < 0) {continue;}  

        //Sender of the last event
        int sender = edgelist(last, 1);
        //Receiver of the last event
        int receiver = edgelist(last, 2); 
        //Type of the last event
        int eventtype = 1;
        if(with_type) {eventtype = edgelist(last,3);}

        // Storage space
        arma::uvec psdyads = {0};

        // Find the dyads that would create the respective p-shift 
        switch(type) {
            // AB-BA
            case 1 :
                // Find the reverse dyad (BA)
                psdyads = find(riskset.col(0) == receiver && 
                    riskset.col(1) == sender);
                if(with_type) {
                    psdyads = find(riskset.col(0) == receiver && 
                        riskset.col(1) == sender &&
                        riskset.col(2) == eventtype);
                }
                break;
            
            // AB-BY
            case 2 :
                // Find all BY dyads 
                psdyads = find(riskset.col(0) == receiver && 
                    riskset.col(1) != sender && riskset.col(1) != receiver);
                if(with_type) {
                    psdyads = find(riskset.col(0) == receiver && 
                        riskset.col(1) != sender && 
                        riskset.col(1) != receiver &&
                        riskset.col(2) == eventtype);
                }
                break;

            // AB-XA
            case 3 :
                // Find all XA dyads 
                psdyads = find(riskset.col(1) == sender && 
                    riskset.col(0) != receiver && riskset.col(0) != sender);
                if(with_type) {
                    psdyads = find(riskset.col(1) == sender && 
                        riskset.col(0) != receiver && 
                        riskset.col(0) != sender &&
                        riskset.col(2) == eventtype);
                }
                break;
            
            // AB-XB
            case 4 :
                // Find all XB dyads 
                psdyads = find(riskset.col(1) == receiver && 
                    riskset.col(0) != sender && riskset.col(0) != receiver);
                if(with_type) {
                    psdyads = find(riskset.col(1) == receiver && 
                        riskset.col(0) != sender && 
                        riskset.col(0) != receiver &&
                        riskset.col(2) == eventtype);
                }
                break;

            // AB-XY
            case 5 :
                // Find all XY dyads 
                psdyads = find(riskset.col(0) != sender && 
                    riskset.col(0) != receiver && riskset.col(1) != sender && 
                    riskset.col(1) != receiver);
                if(with_type) {
                    psdyads = find(riskset.col(0) != sender && 
                        riskset.col(0) != receiver && 
                        riskset.col(1) != sender && 
                        riskset.col(1) != receiver &&
                        riskset.col(2) == eventtype);
                }
                break;
            
            // AB-AY
            case 6 :
                // Find all AY dyads 
                psdyads = find(riskset.col(0) == sender &&                  
                    riskset.col(1) != receiver && riskset.col(1) != sender);
                if(with_type) {
                    psdyads = find(riskset.col(0) == sender &&                  
                        riskset.col(1) != receiver && 
                        riskset.col(1) != sender &&
                        riskset.col(2) == eventtype);
                }
                break;              
        }

        // Set the statistic to one for those dyads that create the respective 
        // p-shift
        for(arma::uword d = 0; d < psdyads.n_elem; ++d) {
            stat(i, psdyads(d)) = 1.0;
        }
    }

    // Output
    return stat;
}

// compute_rrank
// 
// Computes statistic for a recency-rank effect (rrankSend, rrankReceive)
// 
//  type: type of recency-rank effect (1 = rrankSend, 2 = rrankReceive)
//  edgelist: matrix with edgelist (time, sender/actor1, receiver/
// actor2, (type), riskset position)
//  riskset: matrix with riskset (sender/actor1, receiver/actor2, (type))
//  with_type: logical value indicating whether events of different 
// types should be counted separately
//  start: integer value referring to the first row + 1 in the edgelist 
// for which effects have to be computed 
//  stop: integer value referring to the last row + 1 in the edgelist 
// for which effects have to be computed 
//
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat compute_rrank(int type, const arma::mat& edgelist, 
    const arma::mat& riskset, bool with_type, int start, int stop) {

    // Extract actors
    arma::vec actors = sort(unique(join_cols(riskset.col(0), riskset.col(1))));

    // Extract event types
    arma::vec types = {1};
    if(with_type) {
        types = sort(unique(riskset.col(2)));
    }

    // Extract small edgelist
    arma::mat small_edgelist = edgelist.rows((start-1), (stop-1));

    // Storage space statistic and fill with zeros
    arma::mat stat(small_edgelist.n_rows, riskset.n_rows, arma::fill::zeros);
    // Storage space ranks
    arma::cube ranks(actors.n_elem, actors.n_elem, types.n_elem, 
        arma::fill::zeros);

    // For loop over the sequence
    for(arma::uword i = 0; i < small_edgelist.n_rows; ++i) {
        
        // Determine the past
	    double time = small_edgelist(i,0);
	    arma::uvec past = find(edgelist.col(0) < time);

        // Initialize the ranks at the first timepoint
        if(i == 0) {        
            // For loop over events in the past
            for(arma::uword j = 0; j < past.n_elem; ++j) {
                // Sender and receiver of the event
                int sender = edgelist(past(j), 1);
                int receiver = edgelist(past(j), 2);
                // Type of the event
                int eventtype = 1;
                if(with_type) {
                    eventtype = edgelist(past(j), 3);
                }
                
                if(type == 1) {
                    // To whom the sender has most recently send events
                    int rank = ranks(sender-1, receiver-1, eventtype-1);
                    if(rank == 1) {
                        // If the current actor is the most recent actor: 
                        // nothing changes
                        continue;
                    } else {
                        arma::mat typeranks = ranks.slice(eventtype-1);
                        // Find all elements that should be changed
                        arma::uvec change = {0};
                        if(rank == 0) {
                            // All non-zero elements
                            change = find(typeranks.row(sender-1) > 0);
                        } else {
                            // All non-zero elements that are smaller than the 
                            // rank of the current actor
                            change = find(typeranks.row(sender-1) > 0 && 
                                typeranks.row(sender-1) < rank);
                        }
                        // Add one to all elements that should be changed
                        arma::rowvec rowranks = typeranks.row(sender-1);
                        rowranks(change) += 1;
                        // Set the rank of the current actor to one 
                        rowranks(receiver-1) = 1;
                        // Update ranks 
                        typeranks.row(sender-1) = rowranks;
                        ranks.slice(eventtype-1) = typeranks;
                    }
                }
                if(type == 2) {
                    // From whom the sender has most recently received events
                    int rank = ranks(receiver-1, sender-1, eventtype-1);
                    if(rank == 1) {
                        // If the current actor is the most recent actor: 
                        // nothing changes
                        continue;
                    } else {
                        arma::mat typeranks = ranks.slice(eventtype-1);
                        // Find all elements that should be changed
                        arma::uvec change = {0};
                        if(rank == 0) {
                            // All non-zero elements
                            change = find(typeranks.row(receiver-1) > 0);
                        } else {
                            // All non-zero elements that are smaller than the 
                            // rank of the current actor
                            change = find(typeranks.row(receiver-1) > 0 && 
                                typeranks.row(receiver-1) < rank);
                        }
                        // Add one to all elements that should be changed
                        arma::rowvec rowranks = typeranks.row(receiver-1);
                        rowranks(change) += 1;
                        // Set the rank of the current actor to one 
                        rowranks(sender-1) = 1;
                        // Update ranks 
                        typeranks.row(receiver-1) = rowranks;
                        ranks.slice(eventtype-1) = typeranks;
                    }
                }
            }
        }

        // Compute the statistic based on the current ranks
        for(arma::uword d = 0; d < riskset.n_rows; ++d) {
            int dyadsender = riskset(d,0);
            int dyadreceiver = riskset(d,1);
            int dyadtype = 1;
            if(with_type) {
                dyadtype = riskset(d,2);
            }

            stat(i,d) = 1/ranks(dyadsender-1, dyadreceiver-1, dyadtype-1);
            stat.replace(arma::datum::inf, 0);
        }

        // Update the ranks 
        // Sender and receiver of the event
        int sender = small_edgelist(i, 1);
        int receiver = small_edgelist(i, 2);
        // Type of the event
        int eventtype = 1;
        if(with_type) {
            eventtype = small_edgelist(i, 3);
        }
        
        if(type == 1) {
            // To whom the sender has most recently send events
            int rank = ranks(sender-1, receiver-1, eventtype-1);
            if(rank == 1) {
                // If the current actor is the most recent actor: nothing 
                // changes
                continue;
            } else {
                arma::mat typeranks = ranks.slice(eventtype-1);
                // Find all elements that should be changed
                arma::uvec change = {0};
                if(rank == 0) {
                    // All non-zero elements
                    change = find(typeranks.row(sender-1) > 0);
                } else {
                    // All non-zero elements that are smaller than the rank of 
                    // the current actor
                    change = find(typeranks.row(sender-1) > 0 && 
                        typeranks.row(sender-1) < rank);
                }
                // Add one to all elements that should be changed
                arma::rowvec rowranks = typeranks.row(sender-1);
                rowranks(change) += 1;
                // Set the rank of the current actor to one 
                rowranks(receiver-1) = 1;
                // Update ranks 
                typeranks.row(sender-1) = rowranks;
                ranks.slice(eventtype-1) = typeranks;
            }
        }
        if(type == 2) {
            // From whom the sender has most recently received events
            int rank = ranks(receiver-1, sender-1, eventtype-1);
            if(rank == 1) {
                // If the current actor is the most recent actor: nothing 
                // changes
                continue;
            } else {
                arma::mat typeranks = ranks.slice(eventtype-1);
                // Find all elements that should be changed
                arma::uvec change = {0};
                if(rank == 0) {
                    // All non-zero elements
                    change = find(typeranks.row(receiver-1) > 0);
                } else {
                    // All non-zero elements that are smaller than the rank of 
                    // the current actor
                    change = find(typeranks.row(receiver-1) > 0 && 
                        typeranks.row(receiver-1) < rank);
                }
                // Add one to all elements that should be changed
                arma::rowvec rowranks = typeranks.row(receiver-1);
                rowranks(change) += 1;
                // Set the rank of the current actor to one 
                rowranks(sender-1) = 1;
                // Update ranks 
                typeranks.row(receiver-1) = rowranks;
                ranks.slice(eventtype-1) = typeranks;
            }
        }
    }

    return stat;
}

// compute_baselineType
// 
// Computes (one of the dummy) statistic(s) for a baselineType effect.
// 
//  values: matrix (1,1) with the type for which the statistic should be 
// set to one 
//  edgelist: matrix with edgelist (time, sender/actor1, receiver/
// actor2, (type), riskset position)
//  riskset: matrix with riskset (sender/actor1, receiver/actor2, (type))
//  start: integer value referring to the first row + 1 in the edgelist 
// for which effects have to be computed 
//  stop: integer value referring to the last row + 1 in the edgelist 
// for which effects have to be computed 
//
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat compute_baselineType(const arma::mat&values, 
    const arma::mat& edgelist, const arma::mat& riskset, int start, int stop) {

    // Extract small edgelist
    arma::mat small_edgelist = edgelist.rows((start-1), (stop-1));

    // Storage space and fill with zeros
    arma::mat stat(small_edgelist.n_rows, riskset.n_rows, arma::fill::zeros); 

    // Riskset indices
    arma::uvec indices = find(riskset.col(2) == values(0,0));

    // Set the stat for those indices to 1
    arma::vec oneVec(small_edgelist.n_rows, arma::fill::ones);
    stat.each_col(indices) = oneVec;

    return stat;
}

// compute_interact
// 
// Multiplies two previously computed statistics in slices of the statistics 
// object to create the statistic for an interaction effect. Note: the 
// interaction effects have to be computed after(!) all main effects are 
// computed.
// 
//  x: matrix (2x1) with the two slices + 1 that contain the main effects
//  statistics: array (timepoint x riskset position x statistic) with 
// previously computed statistics
//
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat compute_interact(arma::mat x, arma::cube statistics) {

    // Storage space and fill with zeros
    arma::mat stat(statistics.n_rows, statistics.n_cols, arma::fill::zeros);

    // Get the indices of the statistics slices (+1) with the statistics for 
    // which an interaction needs to be computed.
    int main1 = x(0);
    int main2 = x(1);

    // Interact
    stat = statistics.slice(main1-1)%statistics.slice(main2-1);

    // Return
    return stat;
}

// compute_eventEffect
// 
// Computes an event effect. 
// 
//  x: matrix (timepointx1) with for each event the attribute value
//  statistics: array (timepoint x riskset position x statistic) with 
// previously computed statistics (required for stat dimensions)
//
//  stat: matrix (timepoint x riskset position)
// 
//[[Rcpp::export]]
arma::mat compute_eventEffect(arma::mat x, arma::cube statistics, int start, 
    int stop) {
    // Storage space and fill with zeros
    arma::mat stat(statistics.n_rows, statistics.n_cols, arma::fill::zeros);

    // Copy the column of x for each column in stat
    arma::colvec y = x.col(0);
    y = y.subvec(start - 1, stop - 1);
    stat.each_col() = y;

    return stat;
}


// recency
//
// A function for computing the recency statistics, as in  Vu et al. (2017) and 
// Mulder and Leenders (2019).
// (1) Recency sender (type=1) including how long ago the sender was active as 
// sender.
// (2) Recency receiver (type=2) including how long ago the receiver was active 
// as receiver.
// (3) Recency sender-receiver(continue) (type=3) including how long ago the 
// sender send something to the receiver.    
//    
// @param edgelist, matrix, 3-column edgelist (time, sender, receiver).
// @param riskset matrix, all potential dyads, the set of all sender/receiver 
// that are possible at time t.
// @param memory, based on the the event data you can decide about memory.E.g. 
// 30 days, 10 months,80 hours, etc.
// @param type equals 1 for Recency sender, type equals 2 for Recency receiver, 
// type equals 3 for Recency sender-receiver.
//
// @return matrix (event time x dyad)
//
// @examples
// test <- recency(edge, riskset, memory = 60, type  = 1)
//
// [[Rcpp::export]]
arma::mat recency(const arma::mat& edgelist, const arma::mat& riskset, 
    double memory_value, int type) {

	// Number of dyads
	int nedge = riskset.n_rows;

    // Rename riskset
    arma::mat ref = riskset;
    // Statistic matrix saving space
    arma::mat RE(edgelist.n_rows, ref.n_rows, arma::fill::zeros);

    // For loop over the edgelist
    for(arma::uword ee1 = 1; ee1 < edgelist.n_rows; ee1++) {
        // Create saving space
        arma::rowvec recencySend(nedge, arma::fill::zeros);
        arma::rowvec recencyReceive(nedge, arma::fill::zeros);
        arma::rowvec recencyContinue(nedge, arma::fill::zeros);

        // For loop over past events
        for(arma::uword ee2 = 0; ee2 < ee1; ee2++) {
            // How long ago in the past was this event?
            double verschil = edgelist(ee1, 0) - edgelist(ee2, 0);

            if(verschil > memory_value) {
                verschil = 1e6;
            } else {
                verschil = verschil;
            }

            //Reference to the column sender of riskset 
            arma::vec col_ref_se = ref.col(0);
            //Reference to the column sender of edgelist
            arma::vec col_edge_se = edgelist.col(1);

            //Reference to the column receiver of riskset 
            arma::vec col_ref_rec = ref.col(1);
            //Reference to the column receiver of edgelist
            arma::vec col_edge_rec = edgelist.col(2);

            
            if(type == 1) {
                // recency sender
                // Which dyads in the riskset have the sender equal to the 
                // sender of event ee2?
                arma::uvec gg = arma::find(col_ref_se == col_edge_se(ee2));
                // Set for these dyads the recency value equal to the recency
                for(arma::uword s = 0; s < gg.n_elem; ++s) {
                    recencySend(gg(s)) = 1/(verschil+1);
                }
                // Copy the current recencySend stats to the RE statistic matrix
				RE.row(ee1) = recencySend;
            } else if(type == 2) {
                // recency receiver
                // Which dyads in the riskset have the receiver equal to the 
                // receiver of event ee2?
                arma::uvec gg = arma::find(col_ref_rec == col_edge_rec(ee2));
                // Set for these dyads the recency value equal to the recency
                for(arma::uword s = 0; s < gg.n_elem; ++s) {
                    recencyReceive(gg(s)) = 1/(verschil+1);
                }
                // Copy the current recencyReceive stats to the RE statistic 
                // matrix
				RE.row(ee1) = recencyReceive;
            } else if (type == 3) {
                // recency continue
                // Which dyads in the riskset are equal to the dyad of event 
                // ee2?
                arma::uvec gg = 
                    arma::find((col_ref_se == col_edge_se(ee2)) &&    
                    (col_ref_rec == col_edge_rec(ee2)));
                // Set for these dyads the recency value equal to the recency
                for(arma::uword s = 0; s < gg.n_elem; ++s) {
                    recencyContinue(gg(s)) = 1/(verschil+1);
                }
                // Copy the current recencyReceive stats to the RE statistic 
                // matrix
				RE.row(ee1) = recencyContinue;
            }
        }
    }

    return RE;
}



