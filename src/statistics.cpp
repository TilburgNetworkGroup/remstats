#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]

// find_dyad
// 
// Helper function that finds the position of a dyad in the riskset.
// [[Rcpp::export]]
int find_dyad(int i, int j, int c, int N, bool directed) {
    
    int dyad = 0;
    
    if(!directed){
        if(i < j){
            dyad = (N*(N-1)/2)*c+(N-1)*i+j-i-1-(i*i)/2;
            if(i>0){
                dyad += i/2;
            }
        } else {
            dyad = (N*(N-1)/2)*c+(N-1)*j+i-j-1-(j*j)/2;
            if(j>0){
                dyad += j/2;
            }
        }
    } else { 
        dyad = N*(N-1)*c+(N-1)*i+j;
        if(j>i){
            dyad -= 1;
        }
    }

    return dyad;
}

// standardize
//
// Helper function that performs scales the statistic through standardization 
// per time point. 
//
// Per time point, the mean Mt and standard deviation SDt of the statistic Xt 
// is computed. The statistics is scaled by subtracting the mean of the values 
// and divide by the standard deviation: Xt = (Xt - Mt)/SDt. 
//
// *param [stat] statistic matrix. The rows in this matrix always refer to the 
// timepoints. In the case of the tie-oriented model, the columns refer to the 
// possible relational events in the risk set. In the case of the 
// actor-oriented model, the columns refer to all possible senders in the rate 
// model and to all possible receivers in the choice model. 
//
// *return [stat] statistic matrix standardized per time point, i.e., per row. 
//
// [[Rcpp::export]]
arma::mat standardize(arma::mat stat) {

    // For loop over timepoints, i.e., rows
    for(arma::uword i = 0; i < stat.n_rows; ++i) {
        // Subtract the row mean and divide by the row standard deviation
        stat.row(i) = (stat.row(i)-mean(stat.row(i)))/
            stddev(stat.row(i));
    }

    // If the standard deviation is 0, the resulting values are NaN, replace 
    // these values with 0
	stat.replace(arma::datum::nan, 0);

    // Return standardized statistic matrix
    return stat;
}

// compute_adjmat
//
// Helper function that computes the adjacency matrix. 
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
arma::mat compute_adjmat(const arma::mat& edgelist, int N, int D, bool directed,
    int memory, double memory_value, int start, int stop) {

    // Slice the edgelist according to "start" and "stop"
    arma::mat slice = edgelist.rows(start, stop);

    // Initialize memory and fill with zeros
    arma::mat adjmat(slice.n_rows, D, arma::fill::zeros);

    // Full memory
    if(memory == 1) {
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
    }

    // Windowed memory
    if(memory == 2) {
        // For loop over timepoints
		for(arma::uword i = 1; i < slice.n_rows; ++i) {
			// Current time
			double time = slice(i,0);

			// Past events
            arma::uvec pastkey = arma::find(edgelist.col(0) < time &&
			    edgelist.col(0) >= (time-memory_value));
            arma::mat past = edgelist.rows(pastkey);

            // For loop over the past
            for(arma::uword j = 0; j < past.n_rows; ++j) {
                // Add event weight to adjacency matrix
			    adjmat(i, past(j, 1)) += past(j, 2);
            }
		}
    }

    // Brandes memory
    if(memory == 3) {
        // For loop over timepoints
        for(arma::uword i = 0; i < slice.n_rows; ++i) {

            // Current time
            double time = slice(i,0);

            // Past events
            arma::uvec pastkey = arma::find(edgelist.col(0) < time);
            arma::mat past = edgelist.rows(pastkey);

            // For loop over the past
            for(arma::uword j = 0; j < past.n_rows; ++j) {
                // Weight of the event
                double we = past(j, 2);

                // Brandes weight
                double te = past(j, 0);
                we = we*exp(-(time-te)*(log(2)/memory_value));

                // Add weight to adjacency matrix
                adjmat(i, past(j, 1)) += we;
            }
        }
    }

    // Output
    return adjmat;
}

// actorStat_tie
// 
// Computes (or transforms/obtains) the exogenous actor statistics for the 
// tie-oriented model. 
//
// NOTE: A sender or receiver effect is not defined for undirected events. 
//
// *param [type] whether to compute a sender effect (type = 1) or receiver 
// effect (type = 2)
// *param [covariates] matrix with the covariate values. The first column 
// refers to the actors, the second colum to the time point when the covariate 
// value changes and the third column to the covariate value. 
// *param [edgelist] matrix with the observed relational event history. Rows 
// refers to the observed relational events. The first column refers to the 
// time, the second column to the events and the third column to the event 
// weight. 
// *param [actors] vector with the actors that can potentially interact. 
// *param [D] integer number indicating the number of potential relational 
// events in the risk set
// *param [start] integer number indicating the first row in the edgelist for 
// which statistics have to be computed. 
// *param [stop] integer number indicating the last row in the edgelist for 
// which statistics have to be computed. 
//
// *return [stat] matrix with the exogenous actor statistic. Rows refer to the 
// timepoints and columns refer to the actors. 
// 
// [[Rcpp::export]]
arma::mat actorStat_tie(int type, const arma::mat& covariates, 
    const arma::mat& edgelist, const arma::vec& actors, const arma::vec& types, 
    int D, int start, int stop) {

	// Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space 
	arma::mat stat(slice.n_rows, D, arma::fill::zeros);

	// Initialize statistic: 

    // First time point
	double time = slice(0,0);

    // For loop over actors 
	for(arma::uword k = 0; k < actors.n_elem; ++k) {
        // Actor
        int actor = actors(k);
		
        // Find all the attribute values for actor k before the first timepoint
        arma::uvec index = find(covariates.col(0) == actor &&
			covariates.col(1) <= time);
		arma::mat actorcovar = covariates.rows(index);
        // Find the last attribute value for actor k before the first timepoint 
		arma::uword max_index = index_max(actorcovar.col(1));
        double value = actorcovar(max_index, 2);
        
        // Add the value to the correct places in the stat 
        // Sender effect 
        if(type == 1) {
            // For loop over receivers
            for(arma::uword j = 0; j < actors.n_elem; ++j) {
                // Skip self-self events
                if(actors(j) == actor) {continue;}
                // For loop over event types
                for(arma::uword c = 0; c < types.n_elem; ++c) {
                    stat(0, find_dyad(actor, actors(j), types(c), 
                        actors.n_elem, TRUE)) = value;
                }
            }
        }

        // Receiver effect 
        if(type == 2) {
            // For loop over senders
            for(arma::uword i = 0; i < actors.n_elem; ++i) {
                // Skip self-self events
                if(actors(i) == actor) {continue;}
                // For loop over event types
                for(arma::uword c = 0; c < types.n_elem; ++c) {
                    stat(0, find_dyad(actors(i), actor, types(c), 
                        actors.n_elem, TRUE)) = value;
                }
            }
        }
	}

	// Find the unique change timepoints
    arma::vec changetimes = sort(unique(covariates.col(1)));
    changetimes = changetimes(find(changetimes!=0));
    arma::uword counter = 0;

    // For loop over the sequence
    for(arma::uword m = 1; m < slice.n_rows; ++m) {
        // Copy the previous row
        arma::rowvec thisrow = stat.row(m-1);

        // Update the statistic if required
        // Do not update after the last changetime
        if(counter < changetimes.n_elem) {
            // Update if the time of the event is larger than the current
            // changetime
            if(slice(m, 0) > changetimes(counter)) {
                // Update all changes in between
                while((counter < changetimes.n_elem) && (slice(m, 0) > changetimes(counter))) {
					// For loop over actors
					for(arma::uword k = 0; k < actors.n_elem; ++k) {
                        // Actor
                        int actor = actors(k);
                        
                        // Find attribute value for this actor at the change 
                        // timepoint 
						arma::uvec index = find(covariates.col(0) == actor &&
							covariates.col(1) == changetimes(counter));
						// Update if a new value exists
                        if(index.n_elem == 1) {
                        double value = covariates(index(0), 2);
                            
                        // Add the value to the correct places in the stat 
                        // Sender effect 
                        if(type == 1) {
                            // For loop over receivers
                            for(arma::uword j = 0; j < actors.n_elem; ++j) {
                                // Skip self-self events
                                if(actors(j) == actor) {continue;}
                                // For loop over event types
                                for(arma::uword c = 0; c < types.n_elem; ++c) {
                                    stat(0, find_dyad(actor, actors(j), 
                                        types(c), actors.n_elem, TRUE)) = 
                                        value;
                                }
                            }
                        }

                        // Receiver effect 
                        if(type == 2) {
                            // For loop over senders
                            for(arma::uword i = 0; i < actors.n_elem; ++i) {
                                // Skip self-self events
                                if(actors(i) == actor) {continue;}
                                // For loop over event types
                                for(arma::uword c = 0; c < types.n_elem; ++c) {
                                    stat(0, find_dyad(actors(i), actor, 
                                        types(c), actors.n_elem, TRUE)) = 
                                        value;
                                }
                            }
                        }
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

	// Return
	return stat;
}

// dyadStat_tie
// 
// Function to compute the dyadic exogenous statistics 'same', 'difference', 
// 'average', 'minimum' and 'maximum' in the tie-oriented model.
//
// type: integer, 1 = same, 2 = difference, 3 = average, 4 = minimum, 
// 5 = maximum
// covariates: matrix, (id, time, value)
// edgelist: matrix (time, event, weight)
// riskset: matrix (actor1, actor2, type, event)
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// 
// [[Rcpp::export]]
arma::mat dyadStat_tie(int type, const arma::mat& covariates,
	const arma::mat& edgelist, const arma::mat& riskset, 
    int start, int stop) {

	// Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space
	arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

    // Storage space for the current covariate values
    arma::vec current_ac1(riskset.n_rows, arma::fill::zeros);
    arma::vec current_ac2(riskset.n_rows, arma::fill::zeros);

    // Initialize statistic
	double time = slice(0,0);

	// For loop over dyads
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        // Find the relevant actors
        arma::uword actor1 = riskset(i, 0);
        arma::uword actor2 = riskset(i, 1);

        // Find the covariate values for actor1
        arma::uvec index1 = find(covariates.col(0) == actor1 && 
            covariates.col(1) <= time);
        arma::mat actor1_values = covariates.rows(index1);
        arma::uword max_index1 = index_max(actor1_values.col(1));
        current_ac1(i) = actor1_values(max_index1, 2);

        // Find the covariate values for actor2
        arma::uvec index2 = find(covariates.col(0) == actor2 && 
            covariates.col(1) <= time);
        arma::mat actor2_values = covariates.rows(index2);
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
    }

    // Find the unique change timepoints
    arma::vec changetimes = sort(unique(covariates.col(1)));
    changetimes = changetimes(find(changetimes!=0));
    arma::uword counter = 0;

    // For loop over the sequence
    for(arma::uword m = 1; m < slice.n_rows; ++m) {
        
        // Copy the previous row
        arma::rowvec thisrow = stat.row(m-1);

        // Update the statistic if required
        // Do not update after the last changetime
        if(counter < changetimes.n_elem) {
            // Update if the time of the event is larger than the current 
            // changetime
            if(slice(m, 0) > changetimes(counter)) {
                // Update all changes in between
                while((counter < changetimes.n_elem) && 
                    (slice(m, 0) > changetimes(counter))) {
                        
                        // For loop over dyads
                        for(arma::uword i = 0; i < riskset.n_rows; ++i) {
                            // Find the relevant actor
                            arma::uword actor1 = riskset(i, 0);
                            arma::uword actor2 = riskset(i, 1);

                            // Find the values for these actor
                            arma::uvec index1 = 
                                find((covariates.col(0) == actor1) 
                                && (covariates.col(1) == changetimes(counter)));
                            arma::uvec index2 = 
                                find((covariates.col(0) == actor2) 
                                && (covariates.col(1) == changetimes(counter)));
                            
                            // Update if a new value exists
                            if((index1.n_elem == 1) || (index2.n_elem == 1)) {
                                if(index1.n_elem == 1) {
                                    current_ac1(i) = covariates(index1(0), 2);
                                } 
                                if(index2.n_elem == 1) {
                                    current_ac2(i) = covariates(index2(0), 2);
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

	return stat;
}

// inertia_tie
//
// Computes the statistic for an inertia effect in the tie-oriented model. 
// 
// edgelist: matrix (time, event, weight)
// adjmat: matrix (events x dyads)
// riskset: matrix, (actor1, actor2, type, event)
// N: integer, number of actors
// directed: boolean, whether events are directed or undirected
// types: vector, type ids
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// consider_type: boolean indicating whether to compute the inertia per 
// event type (TRUE) or sum across types (FALSE)
// 
// [[Rcpp::export]]
arma::mat inertia_tie(const arma::mat& edgelist, const arma::mat& adjmat, 
    const arma::mat& riskset, int N, bool directed, const arma::vec& types, 
    int start, int stop, bool consider_type) {

    // Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space
	arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

    if(!consider_type) {
        // If there is only one event type, the adjmat is equal to the inertia 
        // statistic
        if(types.n_elem == 1) {
            stat = adjmat;
        } else {
            // For loop over dyads
            for(arma::uword d = 0; d < riskset.n_rows; ++d) {
                // Actors
                int ac1 = riskset(d,0);
                int ac2 = riskset(d,1);

                // For loop over event types
                for(arma::uword c = 0; c < types.n_elem; ++c) {
                    // Find the position of the dyad
                    int dyad = find_dyad(ac1, ac2, types(c), N, directed);
                    // Set the values
                    stat.col(d) += adjmat.col(dyad);   
                }
            }
        }      
    } else {
        stat = adjmat;
    }    

    // Output the computed stat
    return stat;
}

// reciprocity_tie
//
// Computes the statistic for a reciprocity effect in the tie-oriented model. 
// 
// edgelist: matrix (time, event, weight)
// adjmat: matrix (events x dyads)
// riskset: matrix, (actor1, actor2, type, event)
// N: integer, number of actors
// types: vector, type ids
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// consider_type: boolean indicating whether to compute the degree per 
// event type (TRUE) or sum across types (FALSE)
// 
// [[Rcpp::export]]
arma::mat reciprocity_tie(const arma::mat& edgelist, 
    const arma::mat& adjmat, const arma::mat& riskset, int N,  
    const arma::vec&types, int start, int stop, bool consider_type) {

    // Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space
	arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

    if(!consider_type) {
        // For loop over dyads 
        for(arma::uword j = 0; j < riskset.n_rows; j++) {
            // Actors 
            int ac1 = riskset(j,0);
            int ac2 = riskset(j,1);

            // For loop over event types
            for(arma::uword c = 0; c < types.n_elem; ++c) {
                // Find the position of the reverse dyad
                int rev = find_dyad(ac2, ac1, types(c), N, TRUE);
                // Set the values
                stat.col(j) += adjmat.col(rev);   
            }
        } 
    } else {
        // For loop over dyads 
        for(arma::uword j = 0; j < riskset.n_rows; j++) {
            // Actors and event type
            int ac1 = riskset(j,0);
            int ac2 = riskset(j,1);
            int c = riskset(j,2);

            // Find the position of the reverse dyad
            int rev = find_dyad(ac2, ac1, c, N, TRUE);

            // Set the values
            stat.col(j) = adjmat.col(rev);   
        } 
    }
    

    // Output the computed stat
    return stat;
}

// degree_tie
// 
// Function to compute the degree statistics for the tie-oriented model. 
//
// type: integer, 1 = indegreeSender, 2 = indegreeReceiver, 
// 3 = outdegreeSender, 4 = outdegreeReceiver, 5 = totaldegreeSender, 
// 6 = totaldegreeReceiver
// edgelist: matrix (time, event, weight)
// adjmat: matrix (events x dyads)
// actors: vector, actor ids
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// consider_type: boolean indicating whether to compute the degree per 
// event type (TRUE) or sum across types (FALSE)
// 
// [[Rcpp::export]]
arma::mat degree_tie(int type, const arma::mat& edgelist, 
    const arma::mat& adjmat, const arma::vec& actors, const arma::vec& types, int start, int stop, bool consider_type) {

    // Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space
	arma::mat stat(slice.n_rows, adjmat.n_cols, arma::fill::zeros);

    if(consider_type) {

        // Initialize saving space
        arma::cube indeg(slice.n_rows, actors.n_elem, types.n_elem, 
            arma::fill::zeros);
        arma::cube outdeg(slice.n_rows, actors.n_elem, types.n_elem, 
            arma::fill::zeros);

        // For loop over event types 
        for(arma::uword c = 0; c < types.n_elem; ++c) {

            arma::mat indegType = indeg.slice(c);
            arma::mat outdegType = outdeg.slice(c);

            // For loop over actors
            for(arma::uword k = 0; k < actors.n_elem; k++) {

                // Actor
                int actor = actors(k);

                // Actor's indegree
                if((type == 1) || (type == 2) || (type == 5) || (type == 6)) {

                    // For loop over actors h
                    for(arma::uword h = 0; h < actors.n_elem; ++h) {
                        // Skip when actor h = actor k
                        if(actors(h) == actor) {continue;}

                        // Get the index for the column in the riskset in which 
                        // actor k is the receiver, actor h is the sender and 
                        // the type is the current type 
                        int dyad = find_dyad(actors(h), actor, types(c), 
                            actors.n_elem, TRUE);

                        // Add the counts
                        indegType.col(actor) += adjmat.col(dyad);
                    }
                }

                // Actor's outdegree
                if((type == 3) || (type == 4) || (type == 5) || (type == 6)) {

                    // For loop over actors h
                    for(arma::uword h = 0; h < actors.n_elem; ++h) {
                        // Skip when actor h = actor k
                        if(actors(h) == actor) {continue;}

                        // Get the index for the column in the riskset in which 
                        // actor k is the sender, actor h is the receiver and 
                        // the type is the current type 
                        int dyad = find_dyad(actor, actors(h), types(c), 
                            actors.n_elem, TRUE);

                        // Add the counts
                        outdegType.col(actor) += adjmat.col(dyad);
                    }
                }

                // Save in the correct place
                // For loop over actors h
                for(arma::uword h = 0; h < actors.n_elem; ++h) {
                    // Skip when actor h = actor k
                    if(actors(h) == actor) {continue;}

                    int dyad = 0;

                    if((type == 1) || (type == 3) || (type == 5)) {
                        // Get the index for the column in the riskset in which 
                        // actor k is the sender, actor h is the receiver and 
                        // the type is the current type 
                        dyad = find_dyad(actor, actors(h), types(c), 
                            actors.n_elem, TRUE);
                    }

                    if((type == 2) || (type == 4) || (type == 6)) {
                        // Get the index for the column in the riskset in which 
                        // actor k is the sender, actor h is the receiver and 
                        // the type is the current type 
                        dyad = find_dyad(actors(h), actor, types(c), 
                            actors.n_elem, TRUE);
                    }

                    if((type == 1) | (type == 2)) {
                        stat.col(dyad) = indegType.col(actor);
                    }

                    if((type == 3) | (type == 4)) {
                        stat.col(dyad) = outdegType.col(actor);
                    }

                    if((type == 5) | (type == 6)) {
                        stat.col(dyad) = indegType.col(actor) +
                            outdegType.col(actor);
                    }
                }       
            }
        }
    } else {
        // Initialize saving space
        arma::mat indeg(slice.n_rows, actors.n_elem, arma::fill::zeros);
        arma::mat outdeg(slice.n_rows, actors.n_elem, arma::fill::zeros);

        // For loop over actors
        for(arma::uword k = 0; k < actors.n_elem; k++) {

            // Actor
            int actor = actors(k);

            // Actor's indegree
            if((type == 1) || (type == 2) || (type == 5) || (type == 6)) {
                
                // For loop over actors h
                for(arma::uword h = 0; h < actors.n_elem; ++h) {
                    // Skip when actor h = actor k
                    if(actors(h) == actor) {continue;}

                    // For loop over event types
                    for(arma::uword c = 0; c < types.n_elem; ++c) {
                        // Get the index for the column in the riskset in which 
                        // actor k is the receiver, actor h is the sender and 
                        // the type is the current type 
                        int dyad = find_dyad(actors(h), actor, types(c), 
                            actors.n_elem, TRUE);

                        // Add the counts
                        indeg.col(actor) += adjmat.col(dyad);
                    }
                }
            }

            // Actor's outdegree
            if((type == 3) || (type == 4) || (type == 5) || (type == 6)) {
                
                // For loop over actors h
                for(arma::uword h = 0; h < actors.n_elem; ++h) {
                    // Skip when actor h = actor k
                    if(actors(h) == actor) {continue;}

                    // For loop over event types
                    for(arma::uword c = 0; c < types.n_elem; ++c) {
                        // Get the index for the column in the riskset in which 
                        // actor k is the sender, actor h is the receiver and 
                        // the type is the current type 
                        int dyad = find_dyad(actor, actors(h), types(c), 
                            actors.n_elem, TRUE);

                        // Add the counts
                        outdeg.col(actor) += adjmat.col(dyad);
                    }
                }
            }

            // Save in the correct place

            // For loop over actors h
            for(arma::uword h = 0; h < actors.n_elem; ++h) {
                // Skip when actor h = actor k
                if(actors(h) == actor) {continue;}
                
                // For loop over event types
                for(arma::uword c = 0; c < types.n_elem; ++c) {
                    int dyad = 0;

                    if((type == 1) || (type == 3) || (type == 5)) {
                        // Get the index for the column in the riskset in which 
                        // actor k is the sender, actor h is the receiver and 
                        // the type is the current type 
                        dyad = find_dyad(actor, actors(h), types(c), 
                            actors.n_elem, TRUE);
                    }

                    if((type == 2) || (type == 4) || (type == 6)) {
                        // Get the index for the column in the riskset in which 
                        // actor k is the sender, actor h is the receiver and 
                        // the type is the current type 
                        dyad = find_dyad(actors(h), actor, types(c), 
                            actors.n_elem, TRUE);
                    }

                    if((type == 1) | (type == 2)) {
                        stat.col(dyad) = indeg.col(actor);
                    }

                    if((type == 3) | (type == 4)) {
                        stat.col(dyad) = outdeg.col(actor);
                    }

                    if((type == 5) | (type == 6)) {
                        stat.col(dyad) = indeg.col(actor) +
                            outdeg.col(actor);
                    }
                }                             
            }        
        }
    }

    // Output the computed stat
    return stat;
}

// triad_tie
// 
// Computes the triad statistics for the tie-oriented model. 
//
// type: integer, 1 = otp, 2 = itp, 3 = osp, 4 = isp, 5 = sp, 6 = spUnique
// edgelist: matrix (time, event, weight)
// adjmat: matrix (events x dyads)
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// consider_type: boolean indicating whether to compute the degree per 
// event type (TRUE) or sum across types (FALSE)
// 
// [[Rcpp::export]]
arma::mat triad_tie(int type, const arma::mat& edgelist, 
    const arma::vec& actors, const arma::vec& types, const arma::mat& adjmat, 
    const arma::mat& riskset,int start, int stop, bool consider_type) {

    // Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space
	arma::mat stat(slice.n_rows, adjmat.n_cols, arma::fill::zeros);

    if(consider_type) {
        // For loop over dyads
        for(arma::uword d = 0; d < adjmat.n_cols; ++d) {
            // Sender i, receiver j and event type c
            int i = riskset(d,0);
            int j = riskset(d,1);
            int c = riskset(d,2);
            
            // For loop over actors h
            for(arma::uword h = 0; h < actors.n_elem; ++h) {
                if((h == i) || (h == j)) {continue;}

                // Saving space
                int arrow1; int arrow2;
                
                // otp
                if(type == 1) {
                    // arrow1 = sender i sends to actor h
                    arrow1 = find_dyad(i, actors(h), c, actors.n_elem, TRUE);
                    // arrow2 = actor h sends to receiver j
                    arrow2 = find_dyad(actors(h), j, c, actors.n_elem, TRUE);
                }

                // itp
                if(type == 2) {
                    // arrow1 = actor h sends to sender i
                    arrow1 = find_dyad(actors(h), i, c, actors.n_elem, TRUE);
                    // arrow2 = receiver j sends to actor h
                    arrow2 = find_dyad(j, actors(h), c, actors.n_elem, TRUE);
                }

                // osp
                if(type == 3) {
                    // arrow1 = sender i sends to actor h
                    arrow1 = find_dyad(i, actors(h), c, actors.n_elem, TRUE);
                    // arrow2 = receiver j sends to actor h
                    arrow2 = find_dyad(j, actors(h), c, actors.n_elem, TRUE);
                }

                // isp
                if(type == 4) {
                    // arrow1 = actor h sends to sender i
                    arrow1 = find_dyad(actors(h), i, c, actors.n_elem, TRUE);
                    // arrow2 = actor h sends to receiver j
                    arrow2 = find_dyad(actors(h), j, c, actors.n_elem, TRUE);
                }

                // sp or spUnique
                if((type == 5) || (type == 6)) {
                    // arrow1 = actor h sends to sender i OR sender i sends to 
                    // actor h (undirected events, only one exists) ~ corrected 
                    // for by setting directed = FALSE
                    arrow1 = find_dyad(actors(h), i, c, actors.n_elem, FALSE);

                    // arrow2 = receiver j sends to actor h OR actor h sends to 
                    // receiver j (undirected events, only one exists)
                    arrow2 = find_dyad(actors(h), j, c, actors.n_elem, FALSE);
                }

                // For loop over timepoints
                for(arma::uword t = 0; t < slice.n_rows; ++t) {
                    double count1 = adjmat(t, arrow1);
                    double count2 = adjmat(t, arrow2);
                    if(type == 6) {
                        if(count1 > 1) {count1 = 1;}
                        if(count2 > 1) {count2 = 1;}
                    }
                    arma::vec count = {count1, count2};
                    stat(t, d) += min(count);
                }
            }
        }
    } else {
        // For loop over dyads
        for(arma::uword d = 0; d < adjmat.n_cols; ++d) {
            // Sender i and receiver j 
            int i = riskset(d,0);
            int j = riskset(d,1);
            
            // For loop over actors h
            for(arma::uword h = 0; h < actors.n_elem; ++h) {
                if((h == i) || (h == j)) {continue;}

                // Saving space
                arma::vec arrow1(types.n_elem); 
                arma::vec arrow2(types.n_elem);

                // For loop over event types
                for(arma::uword c = 0; c < types.n_elem; ++c) {
                                        
                    // otp
                    if(type == 1) {
                        // arrow1 = sender i sends to actor h
                        arrow1(c) = find_dyad(i,actors(h),types(c),actors.n_elem,TRUE);
                        // arrow2 = actor h sends to receiver j
                        arrow2(c) = find_dyad(actors(h),j,types(c),actors.n_elem,TRUE);
                    }

                    // itp
                    if(type == 2) {
                        // arrow1 = actor h sends to sender i
                        arrow1(c) = find_dyad(actors(h),i,types(c),actors.n_elem,TRUE);
                        // arrow2 = receiver j sends to actor h
                        arrow2(c) = find_dyad(j,actors(h),types(c),actors.n_elem,TRUE);
                    }

                    // osp
                    if(type == 3) {
                        // arrow1 = sender i sends to actor h
                        arrow1(c) = find_dyad(i,actors(h),types(c),actors.n_elem,TRUE);
                        // arrow2 = receiver j sends to actor h
                        arrow2(c) = find_dyad(j,actors(h),types(c),actors.n_elem,TRUE);
                    }

                    // isp
                    if(type == 4) {
                        // arrow1 = actor h sends to sender i
                        arrow1(c) = find_dyad(actors(h),i,types(c),actors.n_elem,TRUE);
                        // arrow2 = actor h sends to receiver j
                        arrow2(c) = find_dyad(actors(h),j,types(c),actors.n_elem,TRUE);
                    }

                    // sp or spUnique
                    if((type == 5) || (type == 6)) {
                        arrow1(c) = find_dyad(actors(h),i,c,actors.n_elem,FALSE);
                        arrow2(c) = find_dyad(actors(h),j,c,actors.n_elem,FALSE);
                    }
                }

                arma::uvec arrow1U = arma::conv_to<arma::uvec>::from(arrow1);
                arma::uvec arrow2U = arma::conv_to<arma::uvec>::from(arrow2);

                // For loop over timepoints
                for(arma::uword t = 0; t < slice.n_rows; ++t) {
                    arma::rowvec adjmatrow = adjmat.row(t);
                    double count1 = arma::sum(adjmatrow(arrow1U));
                    double count2 = arma::sum(adjmatrow(arrow2U));
                    if(type == 6) {
                        if(count1 > 1) {count1 = 1;}
                        if(count2 > 1) {count2 = 1;}
                    }
                    arma::vec count = {count1, count2};
                    stat(t, d) += arma::min(count);
                }     
            }
        }          
    }     

    // Output the computed stat
    return stat;
}

// pshift_tie
//
// Computes statistic for a p-shift effect (AB-BA, AB-BY, AB-XA, AB-XB, AB-XY, 
// AB-AY) 
//
// *param [type] integer value that indicates the type of p-shift effect 
// (1 = AB-BA, 2 = AB-BY, 3 = AB-XA, 4 = AB-XB, 5 = AB-XY, 6 = AB-AY)
// *param [edgelist] matrix with the observed relational event history. Rows 
// refers to the observed relational events. The first column refers to the 
// time, the second column to the events and the third column to the event 
// weight. 
// *param [D] integer value; the number of events in the risk set
// *param [start] integer number indicating the first row in the edgelist for 
// which statistics have to be computed. 
// *param [stop] integer number indicating the last row in the edgelist for 
// which statistics have to be computed. 
// *param [consider_type] boolean indicating whether to compute the pshift per 
// event type (TRUE) or across types (FALSE)
// 
// [[Rcpp::export]]
arma::mat pshift_tie(int type, const arma::mat& edgelist, 
    const arma::mat& riskset, int N, 
    int C, int start, int stop, bool consider_type) {

    // Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space
	arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

    // For loop over events
    for(arma::uword i = 0; i < slice.n_rows; ++i) {

        // Position of the current event in the full edgelist
        int current = start + i;
        // Position of the last event in the full edgelist
        int last = current - 1;

        // If the current event is the first event in the edgelist, continue to 
        // the next iteration
        if(last < 0) {continue;}

        // Sender and receiver of the last event
        int s = riskset(edgelist(last,1),0);
        int r = riskset(edgelist(last,1),1);

        // Type of the last event
        int c = 0;
        if(consider_type) {c = riskset(edgelist(last,1),2);}

        // Find the dyads that would create the respective p-shift
        switch(type) {
            // AB-BA
            case 1 :
                // Find the reverse dyad
                if(!consider_type) {                    
                    for(arma::uword k = 0; k < C; ++k) {
                        int dyad = find_dyad(r,s,k,N,TRUE);
                        stat(i, dyad) = 1.0;
                    }
                } else {
                    int dyad = find_dyad(r,s,c,N,TRUE);
                    stat(i, dyad) = 1.0;
                }
            break;

            // AB-BY
            case 2 :
                // Find all BY dyads
                if(!consider_type) {
                    for(arma::uword j = 0; j < N; ++j) {
                        if((j == r) | (j == s)) {continue;}
                        for(arma::uword k = 0; k < C; ++k) {
                            int dyad = find_dyad(r,j,k,N,TRUE);
                            stat(i, dyad) = 1.0;
                        }
                    }
                } else {
                    for(arma::uword j = 0; j < N; ++j) {
                        if((j == r) | (j == s)) {continue;}
                        int dyad = find_dyad(r,j,c,N,TRUE);
                        stat(i, dyad) = 1.0;
                    }
                }
            break;

            // AB-XA
            case 3 :
                // Find all XA dyads
                if(!consider_type) {
                    for(arma::uword j = 0; j < N; ++j) {
                        if((j == r) | (j == s)) {continue;}
                        for(arma::uword k = 0; k < C; ++k) {
                            int dyad = find_dyad(j,s,k,N,TRUE);
                            stat(i, dyad) = 1.0;
                        }
                    }
                } else {
                    for(arma::uword j = 0; j < N; ++j) {
                        if((j == r) | (j == s)) {continue;}
                        int dyad = find_dyad(j,s,c,N,TRUE);
                        stat(i, dyad) = 1.0;
                    }
                }
                break;
            
            // AB-XB
            case 4 :
                // Find all XB dyads
                if(!consider_type) {
                    for(arma::uword j = 0; j < N; ++j) {
                        if((j == r) | (j == s)) {continue;}
                        for(arma::uword k = 0; k < C; ++k) {
                            int dyad = find_dyad(j,r,k,N,TRUE);
                            stat(i, dyad) = 1.0;
                        }
                    }
                } else {
                    for(arma::uword j = 0; j < N; ++j) {
                        if((j == r) | (j == s)) {continue;}
                        int dyad = find_dyad(j,r,c,N,TRUE);
                        stat(i, dyad) = 1.0;
                    }
                }
                break;
            
            // AB-XY
            case 5 :
                // Find all XY dyads
                if(!consider_type) {
                    for(arma::uword j = 0; j < N; ++j) {
                        if((j == r) | (j == s)) {continue;}
                        for(arma::uword l = 0; l < N; ++l) {
                            if((l == r) | (l == s) | (l == j)) {continue;}
                            for(arma::uword k = 0; k < C; ++k) {
                                int dyad = find_dyad(j,l,k,N,TRUE);
                                stat(i, dyad) = 1.0;
                            }
                        }                        
                    }
                } else {
                    for(arma::uword j = 0; j < N; ++j) {
                        if((j == r) | (j == s)) {continue;}
                        for(arma::uword l = 0; l < N; ++l) {
                            if((l == r) | (l == s) | (l == j)) {continue;}
                            int dyad = find_dyad(j,l,c,N,TRUE);
                            stat(i, dyad) = 1.0;
                        }                        
                    }
                }
                break;

            // AB-AY
            case 6 :
                // Find all AY dyads
                if(!consider_type) {
                    for(arma::uword j = 0; j < N; ++j) {
                        if((j == r) | (j == s)) {continue;}
                        for(arma::uword k = 0; k < C; ++k) {
                            int dyad = find_dyad(s,j,k,N,TRUE);
                            stat(i, dyad) = 1.0;
                        }
                    }
                } else {
                    for(arma::uword j = 0; j < N; ++j) {
                        if((j == r) | (j == s)) {continue;}
                        int dyad = find_dyad(s,j,c,N,TRUE);
                        stat(i, dyad) = 1.0;
                    }
                }
                break;
                
        }
    }

    // Output the computed stat
    return stat;
}

// rrank_tie
// 
// Computes statistic for a recency-rank effect (rrankSend, rrankReceive) in 
// the tie-oriented model.
// 
// type: integer, 1 = rrankSend, 2 = rrankReceive
// edgelist: matrix (time, event, weight) 
// riskset: matrix (actor1, actor2, type, event)
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// consider_type: boolean indicating whether to compute the inertia per 
// event type (TRUE) or sum across types (FALSE)
// 
//[[Rcpp::export]]
arma::mat rrank_tie(int type, const arma::mat& edgelist, 
    const arma::mat& riskset, int N, int C, int start, int stop, bool consider_type) {

    // Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space
	arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

    if(consider_type) {
        // Storage space ranks
        arma::cube ranks(N, N, C, arma::fill::zeros);
        
        // Initialize the ranks at the first timepoint
        // Determine the past
        double time = slice(0,0);
        arma::uvec past = find(edgelist.col(0) < time);

        // For loop over events in the past
        for(arma::uword j = 0; j < past.n_elem; ++j) {
            // Sender, receiver and type of the event
            int event = edgelist(past(j), 1);
            int sender = riskset(event, 0);
            int receiver = riskset(event, 1);
            int eventtype = riskset(event, 2);
                    
            if(type == 1) {
                // To whom the sender has most recently send events
                int rank = ranks(sender, receiver, eventtype);
                if(rank == 1) {
                    // If the current actor is the most recent actor: 
                    // nothing changes
                    continue;
                } else {
                    arma::mat typeranks = ranks.slice(eventtype);
                    // Find all elements that should be changed
                    arma::uvec change = {0};
                    if(rank == 0) {
                        // All non-zero elements
                        change = find(typeranks.row(sender) > 0);
                    } else {
                        // All non-zero elements that are smaller than the 
                        // rank of the current actor
                        change = find(typeranks.row(sender) > 0 && 
                            typeranks.row(sender) < rank);
                    }
                    // Add one to all elements that should be changed
                    arma::rowvec rowranks = typeranks.row(sender);
                    rowranks(change) += 1;
                    // Set the rank of the current actor to one 
                    rowranks(receiver) = 1;
                    // Update ranks 
                    typeranks.row(sender) = rowranks;
                    ranks.slice(eventtype) = typeranks;
                }
            }
            if(type == 2) {
                // From whom the sender has most recently received events
                int rank = ranks(receiver, sender, eventtype);
                if(rank == 1) {
                    // If the current actor is the most recent actor: 
                    // nothing changes
                    continue;
                } else {
                    arma::mat typeranks = ranks.slice(eventtype);
                    // Find all elements that should be changed
                    arma::uvec change = {0};
                    if(rank == 0) {
                        // All non-zero elements
                        change = find(typeranks.row(receiver) > 0);
                    } else {
                        // All non-zero elements that are smaller than the 
                        // rank of the current actor
                        change = find(typeranks.row(receiver) > 0 && 
                            typeranks.row(receiver) < rank);
                    }
                    // Add one to all elements that should be changed
                    arma::rowvec rowranks = typeranks.row(receiver);
                    rowranks(change) += 1;
                    // Set the rank of the current actor to one 
                    rowranks(sender) = 1;
                    // Update ranks 
                    typeranks.row(receiver) = rowranks;
                    ranks.slice(eventtype) = typeranks;
                }
            }
        }
            
        // For loop over the sequence
        for(arma::uword i = 0; i < slice.n_rows; ++i) {
            
            // Compute the statistic based on the current ranks
            for(arma::uword d = 0; d < riskset.n_rows; ++d) {
                int dyadsender = riskset(d,0);
                int dyadreceiver = riskset(d,1);
                int dyadtype = riskset(d,2);

                stat(i,d) = 1/ranks(dyadsender, dyadreceiver, dyadtype);
                stat.replace(arma::datum::inf, 0);
            }

            // Update the ranks 
            // Sender, receiver and type of the event
            int event = slice(i, 1);
            int sender = riskset(event, 0);
            int receiver = riskset(event, 1);
            int eventtype = riskset(event, 2);
           
            if(type == 1) {
                // To whom the sender has most recently send events
                int rank = ranks(sender, receiver, eventtype);
                if(rank == 1) {
                    // If the current actor is the most recent actor: nothing 
                    // changes
                    continue;
                } else {
                    arma::mat typeranks = ranks.slice(eventtype);
                    // Find all elements that should be changed
                    arma::uvec change = {0};
                    if(rank == 0) {
                        // All non-zero elements
                        change = find(typeranks.row(sender) > 0);
                    } else {
                        // All non-zero elements that are smaller than the rank of 
                        // the current actor
                        change = find(typeranks.row(sender) > 0 && 
                            typeranks.row(sender) < rank);
                    }
                    // Add one to all elements that should be changed
                    arma::rowvec rowranks = typeranks.row(sender);
                    rowranks(change) += 1;
                    // Set the rank of the current actor to one 
                    rowranks(receiver) = 1;
                    // Update ranks 
                    typeranks.row(sender) = rowranks;
                    ranks.slice(eventtype) = typeranks;
                }
            }
            if(type == 2) {
                // From whom the sender has most recently received events
                int rank = ranks(receiver, sender, eventtype);
                if(rank == 1) {
                    // If the current actor is the most recent actor: nothing 
                    // changes
                    continue;
                } else {
                    arma::mat typeranks = ranks.slice(eventtype);
                    // Find all elements that should be changed
                    arma::uvec change = {0};
                    if(rank == 0) {
                        // All non-zero elements
                        change = find(typeranks.row(receiver) > 0);
                    } else {
                        // All non-zero elements that are smaller than the rank of 
                        // the current actor
                        change = find(typeranks.row(receiver) > 0 && 
                            typeranks.row(receiver) < rank);
                    }
                    // Add one to all elements that should be changed
                    arma::rowvec rowranks = typeranks.row(receiver);
                    rowranks(change) += 1;
                    // Set the rank of the current actor to one 
                    rowranks(sender) = 1;
                    // Update ranks 
                    typeranks.row(receiver) = rowranks;
                    ranks.slice(eventtype) = typeranks;
                }
            }
        }
    } else {
        // Storage space ranks
        arma::mat ranks(N, N, arma::fill::zeros);
        
        // Initialize the ranks at the first timepoint
        // Determine the past
        double time = slice(0,0);
        arma::uvec past = find(edgelist.col(0) < time);

        // For loop over events in the past
        for(arma::uword j = 0; j < past.n_elem; ++j) {

            // Sender, receiver and type of the event
            int event = edgelist(past(j), 1);
            int sender = riskset(event, 0);
            int receiver = riskset(event, 1);
                    
            if(type == 1) {
                // To whom the sender has most recently send events
                int rank = ranks(sender, receiver);
                if(rank == 1) {
                    // If the current actor is the most recent actor: 
                    // nothing changes
                    continue;
                } else {
                    // Find all elements that should be changed
                    arma::uvec change = {0};
                    if(rank == 0) {
                        // All non-zero elements
                        change = find(ranks.row(sender) > 0);
                    } else {
                        // All non-zero elements that are smaller than the 
                        // rank of the current actor
                        change = find(ranks.row(sender) > 0 && 
                            ranks.row(sender) < rank);
                    }
                    // Add one to all elements that should be changed
                    arma::rowvec rowranks = ranks.row(sender);
                    rowranks(change) += 1;
                    // Set the rank of the current actor to one 
                    rowranks(receiver) = 1;
                    // Update ranks 
                    ranks.row(sender) = rowranks;
                }
            }
            if(type == 2) {
                // From whom the sender has most recently received events
                int rank = ranks(receiver, sender);
                if(rank == 1) {
                    // If the current actor is the most recent actor: 
                    // nothing changes
                    continue;
                } else {
                    // Find all elements that should be changed
                    arma::uvec change = {0};
                    if(rank == 0) {
                        // All non-zero elements
                        change = find(ranks.row(receiver) > 0);
                    } else {
                        // All non-zero elements that are smaller than the 
                        // rank of the current actor
                        change = find(ranks.row(receiver) > 0 && 
                            ranks.row(receiver) < rank);
                    }
                    // Add one to all elements that should be changed
                    arma::rowvec rowranks = ranks.row(receiver);
                    rowranks(change) += 1;
                    // Set the rank of the current actor to one 
                    rowranks(sender) = 1;
                    // Update ranks 
                    ranks.row(receiver) = rowranks;
                }
            }
        }

        // For loop over the sequence
        for(arma::uword i = 0; i < slice.n_rows; ++i) {
           
            // Compute the statistic based on the current ranks
            for(arma::uword d = 0; d < riskset.n_rows; ++d) {
                int dyadsender = riskset(d,0);
                int dyadreceiver = riskset(d,1);
                int dyadtype = riskset(d,2);

                stat(i,d) = 1/ranks(dyadsender, dyadreceiver);
                stat.replace(arma::datum::inf, 0);
            }

            // Update the ranks 
            // Sender, receiver and type of the event
            int event = slice(i, 1);
            int sender = riskset(event, 0);
            int receiver = riskset(event, 1);
            
            if(type == 1) {
                // To whom the sender has most recently send events
                int rank = ranks(sender, receiver);
                if(rank == 1) {
                    // If the current actor is the most recent actor: nothing 
                    // changes
                    continue;
                } else {
                    // Find all elements that should be changed
                    arma::uvec change = {0};
                    if(rank == 0) {
                        // All non-zero elements
                        change = find(ranks.row(sender) > 0);
                    } else {
                        // All non-zero elements that are smaller than the rank of 
                        // the current actor
                        change = find(ranks.row(sender) > 0 && 
                            ranks.row(sender) < rank);
                    }
                    // Add one to all elements that should be changed
                    arma::rowvec rowranks = ranks.row(sender);
                    rowranks(change) += 1;
                    // Set the rank of the current actor to one 
                    rowranks(receiver) = 1;
                    // Update ranks 
                    ranks.row(sender) = rowranks;
                }
            }
            if(type == 2) {
                // From whom the sender has most recently received events
                int rank = ranks(receiver, sender);
                if(rank == 1) {
                    // If the current actor is the most recent actor: nothing 
                    // changes
                    continue;
                } else {
                    // Find all elements that should be changed
                    arma::uvec change = {0};
                    if(rank == 0) {
                        // All non-zero elements
                        change = find(ranks.row(receiver) > 0);
                    } else {
                        // All non-zero elements that are smaller than the rank of 
                        // the current actor
                        change = find(ranks.row(receiver) > 0 && 
                            ranks.row(receiver) < rank);
                    }
                    // Add one to all elements that should be changed
                    arma::rowvec rowranks = ranks.row(receiver);
                    rowranks(change) += 1;
                    // Set the rank of the current actor to one 
                    rowranks(sender) = 1;
                    // Update ranks 
                    ranks.row(receiver) = rowranks;
                }
            }
        }
    }   

    return stat;
}

// recency_tie
// 
// A function for computing the recency statistics, as in  Vu et al. (2017) 
// and Mulder and Leenders (2019).
// 
// type: integer, 1 = recencyContinue, 2 = recencySendSender, 
// 3 = recencySendReceiver, 4 = recencyReceiveSender, 5 = recencyReceiveReceiver
// edgelist: matrix (time, event, weight)
// riskset: matrix, (actor1, actor2, type, event)
// N: integer, number of actors
// C: integer, number of event types
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// consider_type: boolean indicating whether to compute the recency per 
// event type (TRUE) or sum across types (FALSE)
// directed: boolean, whether events are directed or undirected

// [[Rcpp::export]]
arma::mat recency_tie(int type, const arma::mat& edgelist,
    const arma::mat& riskset, int N, int C, int start, int stop, 
    bool consider_type, bool directed) {

	// Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize vector with times the dyads were last active
	arma::vec lastActive(riskset.n_rows);
	lastActive.fill(arma::datum::inf);

	// Select the past
	double time = slice(0,0);
	arma::uvec pastkey = arma::find(edgelist.col(0) < time);
	arma::mat past = edgelist.rows(pastkey);

	// For loop over the past
	for(arma::uword m = 0; m < past.n_rows; ++m) {
        // Sender, receiver and event type
        int d = past(m,1);
        int s = riskset(d,0);
        int r = riskset(d,1);
        int c = riskset(d,2);

        // Event time
        double time = past(m,0);

        // Find respective dyads
        if(type == 1) {
            // Last time active as dyad
            if(consider_type) {
                int dyad = find_dyad(s, r, c, N, directed);
                lastActive(dyad) = time;
                continue;
            } else {
                // For loop over event types
                for(arma::uword k = 0; k < C; k++) {
                    int dyad = find_dyad(s, r, k, N, directed);
                    lastActive(dyad) = time;
                }
            }
        }
		if(type == 2) {
			// Last time the sender was active as sender
            if(consider_type) {
                // For loop over receivers
                for(arma::uword j = 0; j < N; j++) {
                    if(j == s) {continue;}
                    int dyad = find_dyad(s, j, c, N, directed);
                    lastActive(dyad) = time;
                }
            } else {
                // For loop over receivers
                for(arma::uword j = 0; j < N; j++) {
                    if(j == s) {continue;}
                    // For loop over event types 
                    for(arma::uword k = 0; k < C; ++k) {
                        int dyad = find_dyad(s, j, k, N, directed);
                        lastActive(dyad) = time;
                    }
                }
            }
		}
        if(type == 3) {
			// Last time the receiver was active as sender
            if(consider_type) {
                // For loop over senders
                for(arma::uword i = 0; i < N; i++) {
                    if(i == s) {continue;}
                    int dyad = find_dyad(i, s, c, N, directed);
                    lastActive(dyad) = time;
                }
            } else {
                // For loop over senders
                for(arma::uword i = 0; i < N; i++) {
                    if(i == s) {continue;}
                    // For loop over event types 
                    for(arma::uword k = 0; k < C; ++k) {
                        int dyad = find_dyad(i, s, k, N, directed);
                        lastActive(dyad) = time;
                    }
                }
            }
		}
        if(type == 4) {
			// Last time the sender was active as receiver
            if(consider_type) {
                // For loop over receivers
                for(arma::uword j = 0; j < N; j++) {
                    if(j == r) {continue;}
                    int dyad = find_dyad(s, r, c, N, directed);
                    lastActive(dyad) = time;
                }
            } else {
                // For loop over receivers
                for(arma::uword j = 0; j < N; j++) {
                    if(j == r) {continue;}
                    // For loop over event types 
                    for(arma::uword k = 0; k < C; ++k) {
                        int dyad = find_dyad(s, r, k, N, directed);
                        lastActive(dyad) = time;
                    }
                }
            }
		}
		if(type == 5) {
			// Last time the receiver was active as receiver
            if(consider_type) {
                // For loop over senders
                for(arma::uword i = 0; i < N; i++) {
                    if(i == r) {continue;}
                    int dyad = find_dyad(i, r, c, N, directed);
                    lastActive(dyad) = time;
                }
            } else {
                // For loop over senders
                for(arma::uword i = 0; i < N; i++) {
                    if(i == r) {continue;}
                    // For loop over event types 
                    for(arma::uword k = 0; k < C; ++k) {
                        int dyad = find_dyad(i, r, k, N, directed);
                        lastActive(dyad) = time;
                    }
                }
            }
		}
	}

	// Initialize statistic
	arma::mat stat(slice.n_rows, riskset.n_rows);

	// For loop over time points
	for(arma::uword m = 0; m < slice.n_rows; ++m) {
        // Compute statistic
		arma::vec frC = 1/((slice(m,0) - lastActive) + 1);
		arma::rowvec fr = arma::conv_to<arma::rowvec>::from(frC);
		stat.row(m) = fr;

        // Sender, receiver and event type
        int d = slice(m,1);
        int s = riskset(d,0);
        int r = riskset(d,1);
        int c = riskset(d,2);

        // Event time
        double time = slice(m,0);

		// Update last active
        // Find respective dyads
        if(type == 1) {
            // Last time active as dyad
            if(consider_type) {
                lastActive(d) = time;
                continue;
            } else {
                // For loop over event types
                for(arma::uword k = 0; k < C; k++) {
                    int dyad = find_dyad(s, r, k, N, directed);
                    lastActive(dyad) = time;
                }
            }
        }
		if(type == 2) {
			// Last time the sender was active as sender
            if(consider_type) {
                // For loop over receivers
                for(arma::uword j = 0; j < N; j++) {
                    if(j == s) {continue;}
                    int dyad = find_dyad(s, j, c, N, directed);
                    lastActive(dyad) = time;
                }
            } else {
                // For loop over receivers
                for(arma::uword j = 0; j < N; j++) {
                    if(j == s) {continue;}
                    // For loop over event types 
                    for(arma::uword k = 0; k < C; ++k) {
                        int dyad = find_dyad(s, j, k, N, directed);
                        lastActive(dyad) = time;
                    }
                }
            }
		}
        if(type == 3) {
			// Last time the receiver was active as sender
            if(consider_type) {
                // For loop over senders
                for(arma::uword i = 0; i < N; i++) {
                    if(i == s) {continue;}
                    int dyad = find_dyad(i, s, c, N, directed);
                    lastActive(dyad) = time;
                }
            } else {
                // For loop over senders
                for(arma::uword i = 0; i < N; i++) {
                    if(i == s) {continue;}
                    // For loop over event types 
                    for(arma::uword k = 0; k < C; ++k) {
                        int dyad = find_dyad(i, s, k, N, directed);
                        lastActive(dyad) = time;
                    }
                }
            }
		}
        if(type == 4) {
			// Last time the sender was active as receiver
            if(consider_type) {
                // For loop over receivers
                for(arma::uword j = 0; j < N; j++) {
                    if(j == r) {continue;}
                    int dyad = find_dyad(r, j, c, N, directed);
                    lastActive(dyad) = time;
                }
            } else {
                // For loop over receivers
                for(arma::uword j = 0; j < N; j++) {
                    if(j == r) {continue;}
                    // For loop over event types 
                    for(arma::uword k = 0; k < C; ++k) {
                        int dyad = find_dyad(r, j, k, N, directed);
                        lastActive(dyad) = time;
                    }
                }
            }
		}
		if(type == 5) {
			// Last time the receiver was active as receiver
            if(consider_type) {
                // For loop over senders
                for(arma::uword i = 0; i < N; i++) {
                    if(i == r) {continue;}
                    int dyad = find_dyad(i, r, c, N, directed);
                    lastActive(dyad) = time;
                }
            } else {
                // For loop over senders
                for(arma::uword i = 0; i < N; i++) {
                    if(i == r) {continue;}
                    // For loop over event types 
                    for(arma::uword k = 0; k < C; ++k) {
                        int dyad = find_dyad(i, r, k, N, directed);
                        lastActive(dyad) = time;
                    }
                }
            }
		}
    }

  return stat;
}

// tie_tie

//[[Rcpp::export]]
arma::mat tie_tie(const arma::mat& covariates, const arma::mat& edgelist, 
    const arma::mat& riskset, int start, int stop) {

    // Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space 
	arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

    // Saving space
    arma::rowvec thisrow(riskset.n_rows);

    // For loop over dyads
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        
        // Find the relevant actors
        arma::uword actor1 = riskset(i, 0);
        arma::uword actor2 = riskset(i, 1);

        // Find the value
        double tieval = covariates(actor1, actor2);
        thisrow(i) = tieval;
    }

    // Save in stat
    for(arma::uword m = 0; m < slice.n_rows; ++m) {
        stat.row(m) = thisrow;
    }

    //Output
    return(stat);
}

// event_tie
//[[Rcpp::export]]
arma::mat event_tie(const arma::mat& covariates, const arma::mat& edgelist, 
    const arma::mat& riskset, int start, int stop) {

    // Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space 
	arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

    // For loop over dyads
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        stat.col(i) = covariates.col(0);
    }

    //Output
    return(stat);
}

//[[Rcpp::export]]
arma::mat FEtype_tie(const arma::mat& covariates, 
    const arma::mat& edgelist, const arma::mat& riskset, int start, int stop) {

    // Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space 
	arma::mat stat(slice.n_rows, riskset.n_rows, arma::fill::zeros);

    // For loop over dyads
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        arma::colvec val(slice.n_rows, arma::fill::zeros);
        if(riskset(i,2) == covariates(0,0)) {
            val.ones();
        }
        stat.col(i) = val;
    }

    //Output
    return(stat);
}

//[[Rcpp::export]]
arma::cube compute_stats_tie(const arma::vec& effects,
    const arma::mat& edgelist, const arma::mat& adjmat,
    const arma::vec& actors, const arma::vec& types,
    const arma::mat& riskset, const arma::vec& scaling, 
    const Rcpp::List& covariates, const Rcpp::List& interactions, 
    int start, int stop, bool directed) {

    // Initialize saving space
    arma::cube stats(edgelist.n_rows, riskset.n_rows, effects.n_elem);
    stats = stats.rows(start, stop);

    // For loop over effects
    for(arma::uword i = 0; i < effects.n_elem; ++i) {
        // Current effect
        int effect = effects(i);

        // Initialize saving space
        arma::mat stat(stats.n_rows, stats.n_cols, arma::fill::zeros);

        // Compute effect
        switch(effect) {

            // 1 baseline
            case 1 :
                stat.fill(1);
                break;

            // 2 send 
            case 2 :
                // Compute statistic
                stat = actorStat_tie(1, covariates[i],                  
                    edgelist, actors, types, riskset.n_rows, start, stop);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 3 receive 
            case 3 :
                // Compute statistic
                stat = actorStat_tie(2, covariates[i],                  
                    edgelist, actors, types, riskset.n_rows, start, stop);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 4 same
            case 4 : 
                // Compute statistic
                stat = dyadStat_tie(1, covariates[i], edgelist, riskset, start, 
                    stop);
                break;

            // 5 difference
            case 5 : 
                // Compute statistic
                stat = dyadStat_tie(2, covariates[i], edgelist, riskset, start, 
                    stop);
                // Absolute values
                if((scaling(i) == 2) || (scaling(i) == 4)) {
                    stat = abs(stat);
                }
                // Standardize
                if((scaling(i) == 3) || (scaling(i) == 4)) {
                    stat = standardize(stat);
                }
                break;

            // 6 average
            case 6 : 
                // Compute statistic
                stat = dyadStat_tie(3, covariates[i], edgelist, riskset, start, 
                    stop);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 7 minimum
            case 7 : 
                // Compute statistic
                stat = dyadStat_tie(4, covariates[i], edgelist, riskset, start, 
                    stop);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 8 maximum
            case 8 : 
                // Compute statistic
                stat = dyadStat_tie(5, covariates[i], edgelist, riskset, start, 
                    stop);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 10 inertia 
            case 10 : 
                // Compute statistic
                stat = inertia_tie(edgelist, adjmat, riskset, 
                    actors.n_elem, directed, types, start, stop, FALSE);
                // Scale by outdegree of the sender
                if(scaling(i) == 2) {
                    arma::mat deg = degree_tie(3, edgelist, adjmat, 
                        actors, types, start, stop, FALSE);
                    stat = stat/deg;
                    double rep = 1.0/(actors.n_elem-1.0);
                    stat.replace(arma::datum::nan, rep);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 11 reciprocity
            case 11 : 
                // Compute statistic
                stat = reciprocity_tie(edgelist, adjmat, riskset, 
                    actors.n_elem, types, start, stop, FALSE);
                // Scale by indegree of the sender
                if(scaling(i) == 2) {
                    arma::mat deg = degree_tie(1, edgelist, adjmat, 
                        actors, types, start, stop, FALSE);
                    stat = stat/deg;
                    double rep = 1.0/(actors.n_elem-1.0);
                    stat.replace(arma::datum::nan, rep);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 12 indegreeSender 
            case 12 : 
                // Compute statistic
                stat = degree_tie(1, edgelist, adjmat, actors, 
                    types, start, stop, FALSE);
                // Divide by the number/weight of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/sum(adjmat.row(t));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 13 indegreeReceiver
            case 13 : 
                // Compute statistic
                stat = degree_tie(2, edgelist, adjmat, actors, 
                    types, start, stop, FALSE);
                // Divide by the number/weight of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/sum(adjmat.row(t));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 14 outdegreeSender 
            case 14 : 
                // Compute statistic
                stat = degree_tie(3, edgelist, adjmat, actors, 
                    types, start, stop, FALSE);
                // Divide by the number/weight of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/sum(adjmat.row(t));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 15 outdegreeReceiver 
            case 15 : 
                // Compute statistic
                stat = degree_tie(4, edgelist, adjmat, actors, 
                    types, start, stop, FALSE);
                // Divide by the number/weight of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/sum(adjmat.row(t));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 16 totaldegreeSender 
            case 16 : 
                // Compute statistic
                stat = degree_tie(5, edgelist, adjmat, actors, 
                    types, start, stop, FALSE);
                // Divide by two times the number/weight of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/(2*sum(adjmat.row(t)));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 17 totaldegreeReceiver
            case 17 : 
                // Compute statistic
                stat = degree_tie(6, edgelist, adjmat, actors, 
                    types, start, stop, FALSE);
                // Divide by two times the number/weight of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/(2*sum(adjmat.row(t)));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 18 otp
            case 18 :
                // Compute statistic
                stat = triad_tie(1, edgelist, actors, types, adjmat, 
                    riskset, start, stop, FALSE);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 19 itp
            case 19 :
                // Compute statistic
                stat = triad_tie(2, edgelist, actors, types, adjmat, 
                    riskset, start, stop, FALSE);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 20 osp
            case 20 :
                // Compute statistic
                stat = triad_tie(3, edgelist, actors, types, adjmat, 
                    riskset, start, stop, FALSE);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 21 osp
            case 21 :
                // Compute statistic
                stat = triad_tie(4, edgelist, actors, types, adjmat, 
                    riskset, start, stop, FALSE);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 22 sp
            case 22 :
                // Compute statistic
                stat = triad_tie(5, edgelist, actors, types, adjmat, 
                    riskset, start, stop, FALSE);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 23 spUnique
            case 23 :
                // Compute statistic
                stat = triad_tie(6, edgelist, actors, types, adjmat, 
                    riskset, start, stop, FALSE);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 24 psABBA
            case 24 :
                // Compute statistic
                stat = pshift_tie(1, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE);
                break;

            // 25 psABBY
            case 25 :
                // Compute statistic
                stat = pshift_tie(2, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE);
                break;

            // 26 psABXA
            case 26 :
                // Compute statistic
                stat = pshift_tie(3, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE);
                break;

            // 27 psABXB
            case 27 :
                // Compute statistic
                stat = pshift_tie(4, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE);
                break;

            // 28 psABXY
            case 28 :
                // Compute statistic
                stat = pshift_tie(5, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE);
                break;

            // 29 psABAY
            case 29 :
                // Compute statistic
                stat = pshift_tie(6, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE);
                break;

            // 30 rrankSend
            case 30: 
                // Compute statistic
                stat = rrank_tie(1, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE);
                break;

            // 31 rrankReceive
            case 31: 
                // Compute statistic
                stat = rrank_tie(2, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE);
                break;

            // 32 FEtype
            case 32:
                // Compute statistic
                stat = FEtype_tie(covariates[i], edgelist, riskset, start, 
                    stop);
                break;

            // 33 event
            case 33:
                // Compute statistic
                stat = event_tie(covariates[i], edgelist, riskset, start, 
                    stop);
                break;
            
            // 34 recencyContinue
            case 34:
                // Compute statistic
                stat = recency_tie(1, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE, directed);
                break;

            // 35 recencySendSender
            case 35:
                // Compute statistic
                stat = recency_tie(2, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE, directed);
                break;

            // 36 recencySendReceiver
            case 36:
                // Compute statistic
                stat = recency_tie(3, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE, directed);
                break;

            // 37 recencyReceiveSender
            case 37:
                // Compute statistic
                stat = recency_tie(4, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE, directed);
                break;

            // 38 recencyReceiveReceiver
            case 38:
                // Compute statistic
                stat = recency_tie(5, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, FALSE, directed);
                break;            

            // 39 tie
            case 39 : 
                // Compute statistic
                stat = tie_tie(covariates[i], edgelist, riskset, start, 
                    stop);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 40 indegreeSender.type
            case 40 : 
                // Compute statistic
                stat = degree_tie(1, edgelist, adjmat, actors, 
                    types, start, stop, TRUE);
                // Divide by the number/weight of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/sum(adjmat.row(t));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 41 indegreeReceiver.type
            case 41 : 
                // Compute statistic
                stat = degree_tie(2, edgelist, adjmat, actors, 
                    types, start, stop, TRUE);
                // Divide by the number/weight of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/sum(adjmat.row(t));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 42 outdegreeSender.type
            case 42 : 
                // Compute statistic
                stat = degree_tie(3, edgelist, adjmat, actors, 
                    types, start, stop, TRUE);
                // Divide by the number/weight of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/sum(adjmat.row(t));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 43 outdegreeReceiver.type
            case 43 : 
                // Compute statistic
                stat = degree_tie(4, edgelist, adjmat, actors, 
                    types, start, stop, TRUE);
                // Divide by the number/weight of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/sum(adjmat.row(t));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 44 totaldegreeSender.type 
            case 44 : 
                // Compute statistic
                stat = degree_tie(5, edgelist, adjmat, actors, 
                    types, start, stop, TRUE);
                // Divide by two times the number/weight of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/(2*sum(adjmat.row(t)));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 45 totaldegreeReceiver.type
            case 45 : 
                // Compute statistic
                stat = degree_tie(6, edgelist, adjmat, actors, 
                    types, start, stop, TRUE);
                // Divide by two times the number/weight of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/(2*sum(adjmat.row(t)));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;
            
            // 46 psABBA.type
            case 46 :
                // Compute statistic
                stat = pshift_tie(1, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE);
                break;

            // 47 psABBY.type
            case 47 :
                // Compute statistic
                stat = pshift_tie(2, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE);
                break;

            // 48 psABXA.type
            case 48 :
                // Compute statistic
                stat = pshift_tie(3, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE);
                break;

            // 49 psABXB.type
            case 49 :
                // Compute statistic
                stat = pshift_tie(4, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE);
                break;

            // 50 psABXY.type
            case 50 :
                // Compute statistic
                stat = pshift_tie(5, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE);
                break;

            // 51 psABAY.type
            case 51 :
                // Compute statistic
                stat = pshift_tie(6, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE);
                break;

            // 52 inertia.type
            case 52 : 
                // Compute statistic
                stat = inertia_tie(edgelist, adjmat, riskset, 
                    actors.n_elem, directed, types, start, stop, TRUE);
                // Scale by outdegree of the sender, considering event type
                if(scaling(i) == 2) {
                    arma::mat deg = degree_tie(3, edgelist, adjmat, 
                        actors, types, start, stop, TRUE);
                    stat = stat/deg;
                    double rep = 1.0/(actors.n_elem-1.0);
                    stat.replace(arma::datum::nan, rep);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;

            // 53 reciprocity.type
            case 53 : 
                // Compute statistic
                stat = reciprocity_tie(edgelist, adjmat, riskset, 
                    actors.n_elem, types, start, stop, TRUE);
                // Scale by indegree of the sender, considering event type
                if(scaling(i) == 2) {
                    arma::mat deg = degree_tie(1, edgelist, adjmat, 
                        actors, types, start, stop, TRUE);
                    stat = stat/deg;
                    double rep = 1.0/(actors.n_elem-1.0);
                    stat.replace(arma::datum::nan, rep);
                }
                // Standardize 
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;
            
            // 54 otp.type
            case 54 :
                // Compute statistic
                stat = triad_tie(1, edgelist, actors, types, adjmat, 
                    riskset, start, stop, TRUE);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 55 itp.type
            case 55 :
                // Compute statistic
                stat = triad_tie(2, edgelist, actors, types, adjmat, 
                    riskset, start, stop, TRUE);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 56 osp.type
            case 56 :
                // Compute statistic
                stat = triad_tie(3, edgelist, actors, types, adjmat, 
                    riskset, start, stop, TRUE);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 57 isp.type
            case 57 :
                // Compute statistic
                stat = triad_tie(4, edgelist, actors, types, adjmat, 
                    riskset, start, stop, TRUE);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 58 sp.type
            case 58 :
                // Compute statistic
                stat = triad_tie(5, edgelist, actors, types, adjmat, 
                    riskset, start, stop, TRUE);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 59 spUnique.type
            case 59 :
                // Compute statistic
                stat = triad_tie(6, edgelist, actors, types, adjmat, 
                    riskset, start, stop, TRUE);
                // Standardize 
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;

            // 60 rrankSend.type
            case 60: 
                // Compute statistic
                stat = rrank_tie(1, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE);
                break;

            // 61 rrankReceive.type
            case 61: 
                // Compute statistic
                stat = rrank_tie(2, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE);
                break;
            
            // 62 recencyContinue.type
            case 62:
                // Compute statistic
                stat = recency_tie(1, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE, directed);
                break;

            // 63 recencySendSender.type
            case 63:
                // Compute statistic
                stat = recency_tie(2, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE, directed);
                break;

            // 64 recencySendReceiver.type
            case 64:
                // Compute statistic
                stat = recency_tie(3, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE, directed);
                break;
            
            // 65 recencyReceiveSender.type
            case 65:
                // Compute statistic
                stat = recency_tie(4, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE, directed);
                break;

            // 66 recencyReceiveReceiver.type
            case 66:
                // Compute statistic
                stat = recency_tie(5, edgelist, riskset, actors.n_elem, 
                    types.n_elem, start, stop, TRUE, directed);
                break;
             
            // 99 interact
            case 99 :
                // Get the indices of the statistics slices (+1) with the
                // statistics for which an interaction needs to be computed.
                arma::vec x = interactions[i];
                int main1 = x(0);
                int main2 = x(1);
                // Element-wise multiplication
                stat = stats.slice(main1-1)%stats.slice(main2-1);
                break;
        }
        
        // Save statistic
        stats.slice(i) = stat;
    }

    return stats;
}

// actorStat_rc
// 
// Computes (or transforms/obtains) the exogenous actor statistic (sender 
// effect) for the rate step and (receiver effect) choice step in the 
// actor-oriented model. 
//
// *param [covariates] matrix with the covariate values. The first column 
// refers to the actors, the second colum to the time point when the covariate 
// value changes and the third column to the covariate value. 
// edgelist: matrix (time, event, weight)
// riskset: matrix, (actor1, actor2, type, event)
// *param [actors] vector with the actors that can potentially interact. 
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed 
// 
// [[Rcpp::export]]
arma::mat actorStat_rc(const arma::mat& covariates, const arma::mat& edgelist, 
    const arma::mat& riskset, const arma::vec& actors, int start, int stop, 
    int scaling) {

	// Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space
	arma::mat stat(slice.n_rows, actors.n_elem, arma::fill::zeros);

	// Initialize statistic
	double time = slice(0,0);
	for(arma::uword actor = 0; actor < actors.n_elem; ++actor) {
		arma::uvec index = find(covariates.col(0) == actor &&
			covariates.col(1) <= time);
		arma::mat actorcovar = covariates.rows(index);
		arma::uword max_index = index_max(actorcovar.col(1));
        stat(0,actor) = actorcovar(max_index, 2);

        // Scaling in choice model
        if(scaling == 2) {
            int event = slice(0,1);
            int sender = riskset(event,0);

            arma::rowvec statrow = stat.row(0);
            arma::vec statrowMin = statrow(arma::find(actors != sender));

            // For loop over receivers
            for(arma::uword r = 0; r < actors.n_elem; ++r) {
                if(sender == r) {
                    stat(0, r) = 0;
                } else {
                    stat(0, r) = (stat(0,r)-mean(statrowMin))/
                        stddev(statrowMin);
                }
            }

            stat.replace(arma::datum::nan, 0);
        }
	}

	// Find the unique change timepoints
    arma::vec changetimes = sort(unique(covariates.col(1)));
    changetimes = changetimes(find(changetimes!=0));
    arma::uword counter = 0;

    // For loop over the sequence
    for(arma::uword m = 1; m < slice.n_rows; ++m) {
        // Copy the previous row
        arma::rowvec thisrow = stat.row(m-1);

        // Update the statistic if required
        // Do not update after the last changetime
        if(counter < changetimes.n_elem) {
            // Update if the time of the event is larger than the current
            // changetime
            if(slice(m, 0) > changetimes(counter)) {
                // Update all changes in between
                while((counter < changetimes.n_elem) &&
                    (slice(m, 0) > changetimes(counter))) {
						// For loop over actors
						for(arma::uword j = 0; j < actors.n_elem; ++j) {
						arma::uvec index = find(covariates.col(0) == j &&
							covariates.col(1) == changetimes(counter));
						// Update if a new value exists
                        if(index.n_elem == 1) {
                            double value = covariates(index(0), 2);
                            thisrow(j) = value;
                        }
					}

                    //Update the counter
                    counter+=1;
                }
            }
        }

        // Save the row
        stat.row(m) = thisrow;

        // Scaling in choice model
        if(scaling == 2) {
            int event = slice(m,1);
            int sender = riskset(event,0);

            arma::rowvec statrow = stat.row(m);
            arma::vec statrowMin = statrow(arma::find(actors != sender));

            // For loop over receivers
            for(arma::uword r = 0; r < actors.n_elem; ++r) {
                if(sender == r) {
                    stat(m, r) = 0;
                } else {
                    stat(m, r) = (stat(m,r)-mean(statrowMin))/
                        stddev(statrowMin);
                }
            }

            stat.replace(arma::datum::nan, 0);
        }
    }

	// Return
	return stat;
}

// degree_rc
// 
// Function to compute the degree statistics for the actor-oriented model.
// type: integer, 1 = indegree, 2 = outdegree, 3 = total degree
// riskset: matrix, (actor1, actor2, type, event)
// actors: vector, actor ids
// adjmat: matrix (events x dyads)
//
// [[Rcpp::export]]
arma::mat degree_rc(int type, const arma::mat& riskset, 
    const arma::vec& actors, const arma::mat& adjmat) {

	// Initialize saving space 
	arma::mat stat(adjmat.n_rows, actors.n_elem, arma::fill::zeros);
    arma::mat ideg(adjmat.n_rows, actors.n_elem, arma::fill::zeros);
    arma::mat odeg(adjmat.n_rows, actors.n_elem, arma::fill::zeros);

    // For loop over actors i
    for(arma::uword i = 0; i < actors.n_elem; i++) {
        // For loop over actors j
        for(arma::uword j = 0; j < actors.n_elem; j++) {
            // Skip self-to-self events
            if(i == j) {continue;}
            
            if(type == 1 | type == 3) {
                // For the in-degree of actor i: get the (j,i) dyad
                int dyad = find_dyad(j, i, 0, actors.n_elem, TRUE);
                // Extract this column from the adjmat and add it to actor i's 
                // in-degree
                ideg.col(i) += adjmat.col(dyad);
            }

            if(type == 2 | type == 3) {
                // For the out-degree of actor i: get the (i,j) dyad
                int dyad = find_dyad(i, j, 0, actors.n_elem, TRUE);
                // Extract this column from the adjmat and add it to actor i's 
                // in-degree
                odeg.col(i) += adjmat.col(dyad);
            }           
        }
    }

    // Results
    if(type == 1) {stat = ideg;}
    if(type == 2) {stat = odeg;}
    if(type == 3) {stat = ideg + odeg;}
    return stat;
}


// recency_rc
// 
// A function for computing the recency statistics, as in  Vu et al. (2017) 
// and Mulder and Leenders (2019) for the actor-oriented model. 
// 
// type: integer, 1 = recencyContinue, 2 = recencySend, 3 = recencyReceive
// edgelist: matrix (time, event, weight)
// riskset: matrix, (actor1, actor2, type, event)
// N: integer, number of actors
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// consider_type: boolean indicating whether to compute the recency per 
// event type (TRUE) or sum across types (FALSE)
// directed: boolean, whether events are directed or undirected

// [[Rcpp::export]]
arma::mat recency_rc(int type, const arma::mat& edgelist,
    const arma::mat& riskset, int N, int start, int stop) {

	// Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize vector with times the actors/dyads were last active
    arma::vec lastActive(N);
    if(type == 1) {lastActive.resize(riskset.n_rows);}
	lastActive.fill(arma::datum::inf);

	// Select the past
	double time = slice(0,0);
	arma::uvec pastkey = arma::find(edgelist.col(0) < time);
	arma::mat past = edgelist.rows(pastkey);

	// For loop over the past
	for(arma::uword m = 0; m < past.n_rows; ++m) {
        // Sender and receiver of the event
        int d = past(m,1);
        int s = riskset(d,0);
        int r = riskset(d,1);

        // Event time
        double time = past(m,0);

        // Find respective dyads
        if(type == 1) {
            // Last time dyad was active
            lastActive(d) = time;
        }
		if(type == 2) {
            // Last time the actor was active as sender
            lastActive(s) = time;
		}
        if(type == 3) {
            // Last time the actor was active as sender
            lastActive(r) = time;
		}
    }

	// Initialize statistic
	arma::mat stat(slice.n_rows, N, arma::fill::zeros);

	// For loop over time points
	for(arma::uword m = 0; m < slice.n_rows; ++m) {

        // Sender and receiver of the event
        int d = slice(m,1);
        int s = riskset(d,0);
        int r = riskset(d,1);

        // Event time
        double time = slice(m,0);
        
        // Compute statistic
        if(type == 1) {
            // For loop over actors
            for(arma::uword i = 0; i < N; ++i) {
                if(i == s) {continue;}
                int dyad = find_dyad(s, i, 0, N, TRUE);
                double fr = 1/((time - lastActive(dyad)) + 1);
                stat(m, i) = fr;
            }
        } else {
            arma::vec frC = 1/((time - lastActive) + 1);
		    arma::rowvec fr = arma::conv_to<arma::rowvec>::from(frC);
		    stat.row(m) = fr;
        }        

		// Update last active
        // Find respective dyads
        if(type == 1) {
            // Last time dyad was active
            lastActive(d) = time;
        }
		if(type == 2) {
            // Last time the actor was active as sender
            lastActive(s) = time;
		}
        if(type == 3) {
            // Last time the actor was active as sender
            lastActive(r) = time;
		}
    }

  return stat;
}

// dyadStat_choice
// 
// Function to compute the dyadic exogenous statistics 'same', 'difference' and 
// 'average' in the 'Choice'-step of the actor-oriented model.
//
// type: integer, 1 = same, 2 = difference, 3 = average
// covariates: matrix, (id, time, value)
// edgelist: matrix (time, event, weight)
// riskset: matrix (actor1, actor2, type, event)
// actors: vector, actor ids
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// 
// [[Rcpp::export]]
arma::mat dyadStat_choice(int type, const arma::mat& covariates,
	const arma::mat& edgelist, const arma::mat& riskset, 
    const arma::vec& actors, int start, int stop) {

	// Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space
	arma::mat stat(slice.n_rows, actors.n_elem, arma::fill::zeros);

	// For loop over the sequence
	for(arma::uword m = 0; m < slice.n_rows; ++m) {
		// Sender of the event
        int event = slice(m, 1);
        int sender = riskset(event, 0);

		// The sender's current exogenous value
		double time = slice(m, 0);

		arma::uvec indexSender = find(covariates.col(0) == sender &&
			covariates.col(1) <= time);
		arma::mat senderCovar = covariates.rows(indexSender);
		arma::uword senderMax = index_max(senderCovar.col(1));
		double senderValue = senderCovar(senderMax, 2);

		// For loop over receivers
		for(arma::uword r = 0; r < actors.n_elem; ++r) {
			// The receiver's current exogenous value
			arma::uvec indexReceiver = find(covariates.col(0) == r &&
				covariates.col(1) <= time);
			arma::mat receiverCovar = covariates.rows(indexReceiver);
			arma::uword receiverMax = index_max(receiverCovar.col(1));
			double receiverValue = receiverCovar(receiverMax, 2);

			// 1: Same
			if(type == 1) {
                int same = {senderValue == receiverValue};
                stat(m,r) = same;
            }            
			// 2: Difference
			if(type == 2) {stat(m,r) = {senderValue - receiverValue};}

			arma::vec both = {senderValue, receiverValue};
			// 3: Average
			if(type == 3) {stat(m,r) = mean(both);}
			// 4: Minimum
			if(type == 4) {stat(m,r) = min(both);}
			// 5: Maximum
			if(type == 5) {stat(m,r) = max(both);}
		}
	}

	return stat;
}

// [[Rcpp::export]]
arma::mat tie_choice(const arma::mat& covariates, const arma::mat& edgelist, 
	const arma::vec& actors, const arma::mat& riskset, int start, int stop) {

	// Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space 
	arma::mat stat(slice.n_rows, actors.n_elem, arma::fill::zeros);

	// For loop over the sequence
	for(arma::uword m = 0; m < slice.n_rows; ++m) {
		
		// Sender of the event
		arma::uword event = slice(m,1);
        arma::uword sender = riskset(event,0);

		// Fill the statistic
		stat.row(m) = covariates.row(sender);
	}

	return stat;
}

// inertia_choice
//
// Computes the statistic for an inertia effect in the actor-oriented model. 
// 
// edgelist: matrix (time, event, weight)
// adjmat: matrix (events x dyads)
// riskset: matrix, (actor1, actor2, type, event)
// actors: vector, actor ids
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// scaling: integer, 1 = as.is, 2 = prop, 3 = std
//
// [[Rcpp::export]]
arma::mat inertia_choice(const arma::mat& edgelist, const arma::mat& adjmat, 
    const arma::mat& riskset, const arma::vec& actors, int start, int stop, 
    int scaling) {

	// Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space 
	arma::mat stat(slice.n_rows, actors.n_elem, arma::fill::zeros);

    // Degree matrix
    arma::mat deg(slice.n_rows, actors.n_elem, arma::fill::zeros);
    if(scaling == 2) {
        deg = degree_rc(2, riskset, actors, adjmat);
    }

	// For loop over the sequence 
	for(arma::uword m = 0; m < slice.n_rows; ++m) {

		// Sender of the event
		int event = slice(m,1);
        int sender = riskset(event, 0);

		// For loop over receivers
		for(arma::uword r = 0; r < actors.n_elem; ++r) {
						
			// Skip if the sender is the receiver (no self-self edges)
			if(sender == r) {continue;}

			// Get the index for the column in the riskset that refer to the 
			// (i,j) event
            int dyad = find_dyad(sender, r, 0, actors.n_elem, TRUE);

            // Extract the value from the adjmat
            stat(m, r) = adjmat(m, dyad);
		}

        // Scaling
        if(scaling == 2) {
            stat.row(m) = stat.row(m)/deg(m, sender);
            double rep = 1.0/(actors.n_elem-1.0);
            stat.replace(arma::datum::nan, rep);
            stat(m, sender) = 0;
        }

        // Scaling
        if(scaling == 3) {
            arma::rowvec statrow = stat.row(m);
            arma::vec statrowMin = statrow(arma::find(actors != sender));

            // For loop over receivers
            for(arma::uword r = 0; r < actors.n_elem; ++r) {
                if(sender == r) {
                    stat(m, r) = 0;
                } else {
                    stat(m, r) = (stat(m,r)-mean(statrowMin))/
                        stddev(statrowMin);
                }
            }

            stat.replace(arma::datum::nan, 0);
        }
	}

	return stat;
}

// reciprocity_choice
//
// Computes the statistic for a reciprocity effect in the actor-oriented model. 
// 
// edgelist: matrix (time, event, weight)
// adjmat: matrix (events x dyads)
// riskset: matrix, (actor1, actor2, type, event)
// actors: vector, actor ids
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// scaling: integer, 1 = as.is, 2 = prop, 3 = std
//
// [[Rcpp::export]]
arma::mat reciprocity_choice(const arma::mat& edgelist, 
    const arma::mat& adjmat, const arma::mat& riskset, 
    const arma::vec& actors, int start, int stop, int scaling) {

	// Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space 
	arma::mat stat(slice.n_rows, actors.n_elem, arma::fill::zeros);

    // Degree matrix
    arma::mat deg(slice.n_rows, actors.n_elem, arma::fill::zeros);
    if(scaling == 2) {
        deg = degree_rc(1, riskset, actors, adjmat);
    }

	// For loop over the sequence 
	for(arma::uword m = 0; m < slice.n_rows; ++m) {

		// Sender of the event
		int event = slice(m,1);
        int sender = riskset(event, 0);

		// For loop over receivers
		for(arma::uword r = 0; r < actors.n_elem; ++r) {
						
			// Skip if the sender is the receiver (no self-self edges)
			if(sender == r) {continue;}

			// Get the index for the column in the riskset that refers to the 
			// (j,i) event
            int dyad = find_dyad(r, sender, 0, actors.n_elem, TRUE);

            // Extract the value from the adjmat
            stat(m, r) = adjmat(m, dyad);
		}

        // Scaling
        if(scaling == 2) {
            stat.row(m) = stat.row(m)/deg(m, sender);
            double rep = 1.0/(actors.n_elem-1.0);
            stat.replace(arma::datum::nan, rep);
            stat(m, sender) = 0;
        }

        // Scaling
        if(scaling == 3) {
            arma::rowvec statrow = stat.row(m);
            arma::vec statrowMin = statrow(arma::find(actors != sender));

            // For loop over receivers
            for(arma::uword r = 0; r < actors.n_elem; ++r) {
                if(sender == r) {
                    stat(m, r) = 0;
                } else {
                    stat(m, r) = (stat(m,r)-mean(statrowMin))/
                        stddev(statrowMin);
                }
            }

            stat.replace(arma::datum::nan, 0);
        }
	}

	return stat;
}

// triad_choice
// 
// Computes the triad statistics for the choice step in the actor-oriented 
// model.
//
// type: integer, 1 = otp, 2 = itp, 3 = osp, 4 = isp 
// edgelist: matrix (time, event, weight)
// adjmat: matrix (events x dyads)
// riskset: matrix, (actor1, actor2, type, event)
// actors: vector, actor ids
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// scaling: integer, 1 = as.is, 2 = std
// 
// [[Rcpp::export]]
arma::mat triad_choice(int type, const arma::mat& edgelist, 
    const arma::mat& adjmat, const arma::mat& riskset, 
    const arma::vec& actors, int start, int stop, int scaling) {

	// Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space 
	arma::mat stat(slice.n_rows, actors.n_elem, arma::fill::zeros);

	// For loop over the sequence 
	for(arma::uword m = 0; m < slice.n_rows; ++m) {

		// Sender of the event
		int event = slice(m,1);
        int s = riskset(event, 0);

		// For loop over receivers
		for(arma::uword r = 0; r < actors.n_elem; ++r) {
						
			// Skip if the sender is the receiver (no self-self edges)
			if(s == r) {continue;}

            // For loop over actors h 
            for(arma::uword h = 0; h < actors.n_elem; ++h) {
                // Skip self-to-self edges
                if((h == s) || (h == r)) {continue;}

                // Saving space
                int arrow1; int arrow2;

                // otp
                if(type == 1) {
                    // arrow1 = sender i sends to actor h
                    arrow1 = find_dyad(s, actors(h), 0, actors.n_elem, TRUE);
                    // arrow2 = actor h sends to receiver j
                    arrow2 = find_dyad(actors(h), r, 0, actors.n_elem, TRUE);
                }

                // itp
                if(type == 2) {
                    // arrow1 = actor h sends to sender i
                    arrow1 = find_dyad(actors(h), s, 0, actors.n_elem, TRUE);
                    // arrow2 = receiver j sends to actor h
                    arrow2 = find_dyad(r, actors(h), 0, actors.n_elem, TRUE);
                }

                // osp
                if(type == 3) {
                    // arrow1 = sender i sends to actor h
                    arrow1 = find_dyad(s, actors(h), 0, actors.n_elem, TRUE);
                    // arrow2 = receiver j sends to actor h
                    arrow2 = find_dyad(r, actors(h), 0, actors.n_elem, TRUE);
                }

                // isp
                if(type == 4) {
                    // arrow1 = actor h sends to sender i
                    arrow1 = find_dyad(actors(h), s, 0, actors.n_elem, TRUE);
                    // arrow2 = actor h sends to receiver j
                    arrow2 = find_dyad(actors(h), r, 0, actors.n_elem, TRUE);
                }

                // Sum past events
                double count1 = adjmat(m, arrow1);
                double count2 = adjmat(m, arrow2);
                arma::vec count = {count1, count2};
                stat(m, r) += min(count);
            }
		}

        // Scaling
        if(scaling == 2) {
            arma::rowvec statrow = stat.row(m);
            arma::vec statrowMin = statrow(arma::find(actors != s));

            // For loop over receivers
            for(arma::uword r = 0; r < actors.n_elem; ++r) {
                if(s == r) {
                    stat(m, r) = 0;
                } else {
                    stat(m, r) = (stat(m,r)-mean(statrowMin))/
                        stddev(statrowMin);
                }
            }

            stat.replace(arma::datum::nan, 0);
        }
	}

	return stat;
}

// rrank_choice
// 
// Computes statistic for a recency-rank effect (rrankSend, rrankReceive) in 
// the tie-oriented model.
// 
// type: integer, 1 = rrankSend, 2 = rrankReceive
// edgelist: matrix (time, event, weight) 
// riskset: matrix (actor1, actor2, type, event)
// actors: vector, actor ids
// start: integer, first event in the edgelist for which the statistic is 
// computed
// stop: integer, last event in the edgelist for which the statistic is 
// computed
// 
//[[Rcpp::export]]
arma::mat rrank_choice(int type, const arma::mat& edgelist, 
    const arma::mat& riskset, const arma::vec& actors, int start, int stop) {

	// Slice the edgelist according to "start" and "stop"
	arma::mat slice = edgelist.rows(start, stop);

	// Initialize saving space 
	arma::mat stat(slice.n_rows, actors.n_elem, arma::fill::zeros);
    arma::mat ranks(actors.n_elem, actors.n_elem, arma::fill::zeros);

    // Determine the ranks at the first timepoint
    double time = slice(0,0);
    arma::uvec past = find(edgelist.col(0) < time);

    for(arma::uword j = 0; j < past.n_elem; ++ j) {
        // Sender and receiver of the event
        int event = edgelist(past(j), 1);
        int sender = riskset(event, 0);
        int receiver = riskset(event, 1);

        // rrankSend
        if(type == 1) {
            // To whom the sender has most recently send events
            int rank = ranks(sender, receiver);
            if(rank == 1) {
                // If the current actor is the most recent actor: 
                // nothing changes
                continue;
            } else {
                // Find all elements that should be changed
                arma::uvec change = {0};
                if(rank == 0) {
                    // All non-zero elements
                    change = find(ranks.row(sender) > 0);
                } else {
                    // All non-zero elements that are smaller than the 
                    // rank of the current actor
                    change = find(ranks.row(sender) > 0 && 
                        ranks.row(sender) < rank);
                }
                // Add one to all elements that should be changed
                arma::rowvec rowranks = ranks.row(sender);
                rowranks(change) += 1;
                // Set the rank of the current actor to one 
                rowranks(receiver) = 1;
                // Update ranks 
                ranks.row(sender) = rowranks;
            }
        }

        // rrankReceive
        if(type == 2) {
            // From whom the sender has most recently received events
            int rank = ranks(receiver, sender);
            if(rank == 1) {
                // If the current actor is the most recent actor: 
                // nothing changes
                continue;
            } else {
                // Find all elements that should be changed
                arma::uvec change = {0};
                if(rank == 0) {
                    // All non-zero elements
                    change = find(ranks.row(receiver) > 0);
                } else {
                    // All non-zero elements that are smaller than the 
                    // rank of the current actor
                    change = find(ranks.row(receiver) > 0 && 
                        ranks.row(receiver) < rank);
                }
                // Add one to all elements that should be changed
                arma::rowvec rowranks = ranks.row(receiver);
                rowranks(change) += 1;
                // Set the rank of the current actor to one 
                rowranks(sender) = 1;
                // Update ranks 
                ranks.row(receiver) = rowranks;
            }
        }
    }

    // For loop over the sequence
    for(arma::uword m = 0; m < slice.n_rows; ++m) {

        // Compute the statistic based on the current ranks
        int event = slice(m, 1);
        int s = riskset(event, 0);         
        for(arma::uword j = 0; j < actors.n_elem; ++j) {
            stat(m,j) = 1/ranks(s, j);
            stat.replace(arma::datum::inf, 0);
        }

        // Update the ranks 
        // Sender, receiver and type of the event
        int r = riskset(event, 1);
            
        if(type == 1) {
            // To whom the sender has most recently send events
            int rank = ranks(s, r);
            if(rank == 1) {
                // If the current actor is the most recent actor: nothing 
                // changes
                continue;
            } else {
                // Find all elements that should be changed
                arma::uvec change = {0};
                if(rank == 0) {
                    // All non-zero elements
                    change = find(ranks.row(s) > 0);
                } else {
                    // All non-zero elements that are smaller than the rank 
                    // of the current actor
                    change = find(ranks.row(s) > 0 && 
                        ranks.row(s) < rank);
                }
                // Add one to all elements that should be changed
                arma::rowvec rowranks = ranks.row(s);
                rowranks(change) += 1;
                // Set the rank of the current actor to one 
                rowranks(r) = 1;
                // Update ranks 
                ranks.row(s) = rowranks;
            }
        }
        if(type == 2) {
            // From whom the sender has most recently received events
            int rank = ranks(r, s);
            if(rank == 1) {
                // If the current actor is the most recent actor: nothing 
                // changes
                continue;
            } else {
                // Find all elements that should be changed
                arma::uvec change = {0};
                if(rank == 0) {
                    // All non-zero elements
                    change = find(ranks.row(r) > 0);
                } else {
                    // All non-zero elements that are smaller than the rank 
                    // of the current actor
                    change = find(ranks.row(r) > 0 && 
                        ranks.row(r) < rank);
                }
                // Add one to all elements that should be changed
                arma::rowvec rowranks = ranks.row(r);
                rowranks(change) += 1;
                // Set the rank of the current actor to one 
                rowranks(s) = 1;
                // Update ranks 
                ranks.row(r) = rowranks;
            }
        }
    }
      

	return stat;
}

//[[Rcpp::export]]
arma::cube compute_stats_rate(const arma::vec& effects,
    const arma::mat& edgelist, const arma::mat& riskset,
    const arma::mat& adjmat, const arma::vec& actors, 
    const arma::vec& scaling, const Rcpp::List& covariates,
    const Rcpp::List& interactions, int start, int stop) {

    // Initialize saving space
    arma::cube rateStats(edgelist.n_rows, actors.n_elem, effects.n_elem);
    rateStats = rateStats.rows(start, stop);

    // For loop over effects
    for(arma::uword i = 0; i < effects.n_elem; ++i) {
        // Current effect
        int effect = effects(i);

        // Initialize saving space
        arma::mat stat(rateStats.n_rows, rateStats.n_cols, arma::fill::zeros);

        // Compute effect
        switch(effect) {
            // 1 baseline
            case 1 :
                stat.fill(1);
                break;
            // 2 send
            case 2:
                // Compute statistic
                stat = actorStat_rc(covariates[i], edgelist, riskset, actors, 
                    start, stop, 1);
                // Standardize
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;
            // 3 in-degree
            case 3 :
                stat = degree_rc(1, riskset, actors, adjmat);
                // Divide by the number of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/sum(adjmat.row(t));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;
            // 4 out-degree
            case 4 :
                stat = degree_rc(2, riskset, actors, adjmat);
                // Divide by the number of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/sum(adjmat.row(t));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;
            // 5 total-degree
            case 5 :
                stat = degree_rc(3, riskset, actors, adjmat);
                // Divide by two times the number of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/(2*sum(adjmat.row(t)));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;
            // 6 recencySendSender
            case 6 :
                // Compute statistic
                stat = recency_rc(2, edgelist, riskset, actors.n_elem, 
                    start, stop);
                break;
            // 7 recencyReceiveSender
            case 7 :
                // Compute statistic
                stat = recency_rc(3, edgelist, riskset, actors.n_elem, 
                    start, stop);
                break;
            // 99 interact
            case 99 :
                // Get the indices of the statistics slices (+1) with the
                // statistics for which an interaction needs to be computed.
                arma::vec x = interactions[i];
                int main1 = x(0);
                int main2 = x(1);
                // Element-wise multiplication
                stat = rateStats.slice(main1-1)%rateStats.slice(main2-1);
                break;
        }

        // Save statistic
        rateStats.slice(i) = stat;
    }

    return rateStats;
}

//[[Rcpp::export]]
arma::cube compute_stats_choice(const arma::vec& effects,
    const arma::mat& edgelist, const arma::mat& adjmat,
    const arma::vec& actors, const arma::mat& riskset,
    const arma::vec& scaling, const Rcpp::List& covariates,
    const Rcpp::List& interactions, int start, int stop) {

    // Initialize saving space
    arma::cube choiceStats(edgelist.n_rows, actors.n_elem, effects.n_elem);
    choiceStats = choiceStats.rows(start, stop);

    // For loop over effects
    for(arma::uword i = 0; i < effects.n_elem; ++i) {
        // Current effect
        int effect = effects(i);

        // Initialize saving space
        arma::mat stat(choiceStats.n_rows, choiceStats.n_cols,
            arma::fill::zeros);

        // Compute effect
        switch(effect) {
            // 1 receive
            case 1 :
                // Compute statistic
                stat = actorStat_rc(covariates[i], edgelist, riskset, actors, 
                    start, stop, scaling(i));
                break;
            // 2 same
            case 2 :
                // Compute statistic
                stat = dyadStat_choice(1, covariates[i], edgelist, riskset, 
                    actors, start, stop);
                break;
            // 3 difference
            case 3 :
                // Compute statistic
                stat = dyadStat_choice(2, covariates[i], edgelist, riskset, 
                    actors, start, stop);
                // Absolute values
                if((scaling(i) == 2) || (scaling(i) == 4)) {
                    stat = abs(stat);
                }
                // Standardize
                if((scaling(i) == 3) || (scaling(i) == 4)) {
                    stat = standardize(stat);
                }
                break;
            // 4 average
            case 4 :
                // Compute statistic
                stat = dyadStat_choice(3, covariates[i], edgelist, riskset, 
                    actors, start, stop);
                // Standardize
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;
            // 5 tie
            case 5 : 
                // Compute statistic
                stat = tie_choice(covariates[i], edgelist, actors, riskset, 
                    start, stop);
                // Standardize
                if(scaling(i) == 2) {
                    stat = standardize(stat);
                }
                break;
            // 6 inertia
            case 6 : 
                // Compute statistic
                stat = inertia_choice(edgelist, adjmat, riskset, actors, 
                    start, stop, scaling(i));
                break;
            // 7 reciprocity
            case 7 : 
                // Compute statistic
                stat = reciprocity_choice(edgelist, adjmat, riskset, actors, 
                    start, stop, scaling(i));
                break;
            // 8 in-degree
            case 8 :
                stat = degree_rc(1, riskset, actors, adjmat);
                // Divide by the number of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/sum(adjmat.row(t));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;
            // 9 out-degree
            case 9 :
                stat = degree_rc(2, riskset, actors, adjmat);
                // Divide by the number of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/sum(adjmat.row(t));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;
            // 10 total-degree
            case 10 :
                stat = degree_rc(3, riskset, actors, adjmat);
                // Divide by two times the number of past events
                if(scaling(i) == 2) {
                    for(arma::uword t = 0; t < stat.n_rows; ++t) {
                        stat.row(t) = stat.row(t)/(2*sum(adjmat.row(t)));
                    }
                    stat.replace(arma::datum::nan, 0);
                }
                // Standardize
                if(scaling(i) == 3) {
                    stat = standardize(stat);
                }
                break;
            // 11 otp
            case 11 :
                stat = triad_choice(1, edgelist, adjmat, riskset, actors,   
                    start, stop, scaling(i));
                break;
            // 12 itp
            case 12 :
                stat = triad_choice(2, edgelist, adjmat, riskset, actors,   
                    start, stop, scaling(i));
                break;
            // 13 osp
            case 13 :
                stat = triad_choice(3, edgelist, adjmat, riskset, actors,   
                    start, stop, scaling(i));
                break;
            // 14 isp
            case 14 :
                stat = triad_choice(4, edgelist, adjmat, riskset, actors,   
                    start, stop, scaling(i));
                break;
            // 15 rrankSend
            case 15 :
                stat = rrank_choice(1, edgelist, riskset, actors, start, stop);
                break;
            // 16 rrankReceive
            case 16 :
                stat = rrank_choice(2, edgelist, riskset, actors, start, stop);
                break;
            // 17 recencySendReceiver
            case 17 :
                stat = recency_rc(2, edgelist, riskset, actors.n_elem, 
                    start, stop);
                break;
            // 18 recencyReceiveReceiver
            case 18 :
                stat = recency_rc(3, edgelist, riskset, actors.n_elem, 
                    start, stop);
                break;
            // 19 recencyContinue
            case 19 :
                stat = recency_rc(1, edgelist, riskset, actors.n_elem, 
                    start, stop);
                break;
            // 99 interact
            case 99 :
                // Get the indices of the statistics slices (+1) with the
                // statistics for which an interaction needs to be computed.
                arma::vec x = interactions[i];
                int main1 = x(0);
                int main2 = x(1);
                // Element-wise multiplication
                stat = choiceStats.slice(main1-1)%choiceStats.slice(main2-1);
                break;
        }

        // Save statistic
        choiceStats.slice(i) = stat;
    }

    return choiceStats;
}
