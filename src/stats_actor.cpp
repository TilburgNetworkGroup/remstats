#define ARMA_64BIT_WORD 1
#include "RcppArmadillo.h"
#include <remify.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]] 
// [[Rcpp::interfaces(r, cpp)]]

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
arma::mat standardizeRC(arma::mat stat) {

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
arma::mat compute_adjmatRC(const arma::mat& edgelist, int N, int D, 
    bool directed, int memory, double memory_value, int start, int stop) {

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
                we = we*exp(-(time-te)*(log(2)/memory_value))*(log(2)/memory_value);

                // Add weight to adjacency matrix
                adjmat(i, past(j, 1)) += we;
            }
        }
    }

    // Output
    return adjmat;
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
                int dyad = remify::getDyadIndex(j, i, 0, actors.n_elem, TRUE);
                // Extract this column from the adjmat and add it to actor i's 
                // in-degree
                ideg.col(i) += adjmat.col(dyad);
            }

            if(type == 2 | type == 3) {
                // For the out-degree of actor i: get the (i,j) dyad
                int dyad = remify::getDyadIndex(i, j, 0, actors.n_elem, TRUE);
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
                int dyad = remify::getDyadIndex(s, i, 0, N, TRUE);
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
            int dyad = remify::getDyadIndex(sender, r, 0, actors.n_elem, TRUE);

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
            int dyad = remify::getDyadIndex(r, sender, 0, actors.n_elem, TRUE);

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
                int a1; int a2;

                // otp
                if(type == 1) {
                    // arrow1 = sender i sends to actor h
                    a1 = remify::getDyadIndex(s, actors(h), 0, actors.n_elem, TRUE);
                    // arrow2 = actor h sends to receiver j
                    a2 = remify::getDyadIndex(actors(h), r, 0, actors.n_elem, TRUE);
                }

                // itp
                if(type == 2) {
                    // arrow1 = actor h sends to sender i
                    a1 = remify::getDyadIndex(actors(h), s, 0, actors.n_elem, TRUE);
                    // arrow2 = receiver j sends to actor h
                    a2 = remify::getDyadIndex(r, actors(h), 0, actors.n_elem, TRUE);
                }

                // osp
                if(type == 3) {
                    // arrow1 = sender i sends to actor h
                    a1 = remify::getDyadIndex(s, actors(h), 0, actors.n_elem, TRUE);
                    // arrow2 = receiver j sends to actor h
                    a2 = remify::getDyadIndex(r, actors(h), 0, actors.n_elem, TRUE);
                }

                // isp
                if(type == 4) {
                    // arrow1 = actor h sends to sender i
                    a1 = remify::getDyadIndex(actors(h), s, 0, actors.n_elem, TRUE);
                    // arrow2 = actor h sends to receiver j
                    a2 = remify::getDyadIndex(actors(h), r, 0, actors.n_elem, TRUE);
                }

                // Sum past events
                double count1 = adjmat(m, a1);
                double count2 = adjmat(m, a2);
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
                    stat = standardizeRC(stat);
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
                    stat = standardizeRC(stat);
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
                    stat = standardizeRC(stat);
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
                    stat = standardizeRC(stat);
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
                    stat = standardizeRC(stat);
                }
                break;
            // 4 average
            case 4 :
                // Compute statistic
                stat = dyadStat_choice(3, covariates[i], edgelist, riskset, 
                    actors, start, stop);
                // Standardize
                if(scaling(i) == 2) {
                    stat = standardizeRC(stat);
                }
                break;
            // 5 tie
            case 5 : 
                // Compute statistic
                stat = tie_choice(covariates[i], edgelist, actors, riskset, 
                    start, stop);
                // Standardize
                if(scaling(i) == 2) {
                    stat = standardizeRC(stat);
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
                    stat = standardizeRC(stat);
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
                    stat = standardizeRC(stat);
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
                    stat = standardizeRC(stat);
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