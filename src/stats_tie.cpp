#define ARMA_64BIT_WORD 1
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]] 
// [[Rcpp::interfaces(r, cpp)]]

// title getDyadIndex
//
// param actor1 id of actor1 from 0 to N-1
// param actor2 id of actor2 from 0 to N-1
// param type id of event type from 0 to C-1
// param N number of actors
// param directed bool FALSE/TRUE if the networks is directed (TRUE) or not
// (FALSE)
//
// source: remify, author: Giuseppe
//
// return dyad index according to the combination of id's of actor1/actor2/type
// [[Rcpp::export]]
int getDyadIndex(double actor1, double actor2, double type, int N, bool directed) {

    int dyad = -999; // returning impossible index if the dyad is a self-edge (i.e., sender and receiver are the same actor)
    if(actor1 != actor2){
        if(!directed){ // when directed == FALSE we sort actor1 and actor2
            if(actor1 < actor2){
                double dyad_loc = (N*(N-1)/2)*type+(N-1)*actor1+actor2-actor1-1-(actor1*actor1)/2;
                if(actor1>0){
                    dyad_loc += actor1/2;
                }
                dyad = dyad_loc;
            }
            else{
                double dyad_loc = (N*(N-1)/2)*type+(N-1)*actor2+actor1-actor2-1-(actor2*actor2)/2;
                if(actor2>0){
                    dyad_loc += actor2/2;
                }
                dyad = dyad_loc;
            }
        }
        else{ 
            // when directed == TRUE (we do not sort) (actor1 = sender, actor2 = receiver)
            double dyad_loc = N*(N-1)*type+(N-1)*actor1+actor2;
            if(actor2>actor1){
                dyad_loc -= 1;
            }
            dyad = dyad_loc;
        }
    }

    return dyad;
}

// @title getRisksetMatrix 
//
// Obtain permutations of actors' ids and event types.
//
// @param actorID: vector of actors' id's.
// @param typeID: vector of types' id's.
// @param directed: boolean value, are events directed (1) or undirected (0)?
// source: remify, author: Giuseppe Arena
//
// @return return matrix of possible dyadic events.
// [[Rcpp::export]]
arma::mat getRisksetMatrix(arma::uvec actorID, arma::uvec typeID, 
    bool directed) {

    arma::uword N = actorID.n_elem;
    arma::uword C = typeID.n_elem;

    switch(directed){
    case 0: { // for undirected network
        arma::uword i,j,c;
        arma::uword col_index = 0;
        arma::mat riskset(((N*(N-1))/2)*C,4); 
        for(c = 0; c < C; c++){
            for(i = 0; i < N; i++){
                for(j = (i+1); j < N ; j++){
                        // unit increase col_index 
                        riskset(col_index,0) = actorID(i);
                        riskset(col_index,1) = actorID(j);
                        riskset(col_index,2) = typeID(c);
                        riskset(col_index,3) = col_index;
                        col_index += 1;       
                }
            }
        }
        return riskset; 
    }

    case 1: { // for directed network
        arma::uword i,j,c;
        arma::mat riskset(N*N*C,4);
        arma::uvec indices_to_shed(N*C); // this is the vector where to store the indices of selfedges to remove at the end of the function
        indices_to_shed.fill(N*N*C);
        for(c = 0; c < C; c++){
            for(i = 0; i < N; i++){
                for(j = 0; j < N ; j++){
                        if(j != i){
                        riskset(j+i*N+c*(N*N),0) = actorID(i);
                        riskset(j+i*N+c*(N*N),1) = actorID(j);
                        riskset(j+i*N+c*(N*N),2) = typeID(c);
                    }
                    else {
                        indices_to_shed(j+c*N) = (j+i*N+c*(N*N));
                    }
                }
            }
        }
        riskset.shed_rows(indices_to_shed); 
        riskset.col(3) = arma::linspace(0,riskset.n_rows-1,riskset.n_rows);
        return riskset;
    }
    }
}

// @title compute_adjmat
//
// Calculates (or updates) the number of past events for each dyad.
// 
// @param adjmat: matrix [actors x actors]: the adjacency matrix for the 
// past (!) network. 
// @param edgelist: matrix [time, event id, weight]: the observed relational 
// event history.
// @param riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events. 
// @param memory: 1 = full, 2 = window, 3 = Brandes.
// @param memory_param 
// @param start: the index of the first event in "edgelist" that is added to 
// the adjacency matrix.  
// @param stop: the index of the last event in "edgelist" that is added to the 
// adjacency matrix. !If memory is 2, or 3 it can be at most the number of 
// events in the edgelist - 2 (i.e., the index of the event before the last 
// event, because we do not have a time for an event after an observed sequence 
// the last observed event cannot be added.)
// 
// [[Rcpp::export]]
arma::mat compute_adjmat(arma::mat adjmat, const arma::mat& edgelist, 
	const arma::mat& riskset, int memory, double memory_param, 
    arma::uword start, arma::uword stop) {

    // In the case of full memory: add all events that are observed between 
    // "start" and "stop" to the adjacency matrix
    if(memory == 1) {
        // Slice the edgelist
        arma::mat slice = edgelist.rows(start, stop);

        // For loop over the events
        for(arma::uword m = 0; m < slice.n_rows; ++m) {
            // Update the adjacency matrix
            int dyad = slice(m, 1);
            adjmat(riskset(dyad,0), riskset(dyad,1)) += slice(m,2);
        }
    }

    // In the case of windowed memory: recompute the adjacency matrix. Add all 
    // events between [t-memory_param, t], including the "stop" event itself 
    // (where t is the time for the "stop" event).
    if(memory == 2) {
        // Time of the next event after the "stop" event
        double time = edgelist(stop + 1,0);

        // Slice the edgelist
        arma::uvec wi = arma::find(edgelist.col(0) < time && 
            edgelist.col(0) >= (time - memory_param));
        arma::mat slice = edgelist.rows(wi);

        // Fil the adjacency matrix with zeros
        adjmat.fill(0);

        // For loop over the events
        for(arma::uword m = 0; m < slice.n_rows; ++m) {
            // Update the adjacency matrix
            int dyad = slice(m, 1);
            adjmat(riskset(dyad,0), riskset(dyad,1)) += slice(m,2);
        }
    }

    // In the case of Brandes memory: recompute the adjacency matrix based on 
    // all events until (including) the "stop" event. The weight of the event 
    // in the adjacency matrix is based on time between the current time and 
    // the time of the event. 
    if(memory == 3) {
        // Slice the edgelist
        arma::mat slice = edgelist.rows(0,stop);

        // Time of the next event after the "stop" event
        double timeC = edgelist(stop + 1,0);

        // Fil the adjacency matrix with zeros
        adjmat.fill(0);

        // For loop over the events
        for(arma::uword m = 0; m < slice.n_rows; ++m) {
            // Event info
            double timeE = slice(m,0); 
            int dyad = slice(m, 1);
            double weight = slice(m,2);

            // Brandes weight 
            weight = weight*exp(-(timeC-timeE)*(log(2)/memory_param))*(log(2)/memory_param);

            // Add weight to the adjacency matrix
            adjmat(riskset(dyad,0), riskset(dyad,1)) += weight;
        }
    }

	return adjmat;
}

// compute_indeg
//
// Function that computes indegree of the sender or receiver at time point t 
// for all dyads in the risk set based on the adjacency matrix. 
// 
// * type: integer (1: in-degree of the sender, 2: in-degree of the receiver)
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// * adjmat: matrix [actors x actors]: the past (!) network. 
// 
// [[Rcpp::export]]
arma::rowvec compute_indeg(int type, const arma::mat& riskset, 
    const arma::mat& adjmat) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // Compute in-degree for each actor
    arma::rowvec deg = arma::sum(adjmat, 0);

    // In-degree of the sender
    if(type == 1) {
        for(arma::uword i = 0; i < riskset.n_rows; ++i) {
            statrow(i) = deg(riskset(i,0));
        }
    }

    // In-degree of the receiver
    if(type == 2) {
        for(arma::uword i = 0; i < riskset.n_rows; ++i) {
            statrow(i) = deg(riskset(i,1));
        }
    }
    
    return statrow;
}

// compute_odeg
//
// Function that computes out-degree of the sender or receiver at time point t 
// for all dyads in the risk set based on the adjacency matrix. 
// 
// * type: integer (1: out-degree of the sender, 2: out-degree of the receiver)
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// * adjmat: matrix [actors x actors]: the past (!) network. 
// 
// [[Rcpp::export]]
arma::rowvec compute_odeg(int type, const arma::mat& riskset, 
    const arma::mat& adjmat) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // Compute in-degree for each actor
    arma::vec deg = arma::sum(adjmat, 1);

    // Out-degree of the sender
    if(type == 1) {
        for(arma::uword i = 0; i < riskset.n_rows; ++i) {
            statrow(i) = deg(riskset(i,0));
        }
    }

    // Out-degree of the receiver
    if(type == 2) {
        for(arma::uword i = 0; i < riskset.n_rows; ++i) {
            statrow(i) = deg(riskset(i,1));
        }
    }
    
    return statrow;
}

// compute_tdeg
//
// Function that computes total degree of the sender or receiver at time point 
// t for all dyads in the risk set based on the adjacency matrix. 
// 
// * type: integer (1: total degree of the sender, 2: total degree of the 
// receiver)
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// * adjmat: matrix [actors x actors]: the past (!) network. 
// 
// [[Rcpp::export]]
arma::rowvec compute_tdeg(int type, const arma::mat& riskset, 
    const arma::mat& adjmat) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // Compute in-degree for each actor
    arma::rowvec ideg = arma::sum(adjmat, 0);
    arma::vec odeg = arma::sum(adjmat, 1);

    // Out-degree of the sender
    if(type == 1) {
        for(arma::uword i = 0; i < riskset.n_rows; ++i) {
            statrow(i) = ideg(riskset(i,0)) + odeg(riskset(i,0));
        }
    }

    // Out-degree of the receiver
    if(type == 2) {
        for(arma::uword i = 0; i < riskset.n_rows; ++i) {
            statrow(i) = ideg(riskset(i,1)) + odeg(riskset(i,1));
        }
    }
    
    return statrow;
}

// compute_inertia
//
// Function that computes inertia at time point t for all dyads in the risk set 
// based on the adjacency matrix. 
// 
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// * adjmat: matrix [actors x actors]: the past (!) network. 
// 
// [[Rcpp::export]]
arma::rowvec compute_inertia(const arma::mat& riskset, 
    const arma::mat& adjmat) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // For loop over possible events
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        statrow(i) = adjmat(riskset(i,0), riskset(i,1));
    }

    return statrow;
}

// compute_reciprocity
//
// Function that computes reciprocity at time point t for all dyads in the risk 
// set based on the adjacency matrix. 
// 
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// * adjmat: matrix [actors x actors]: the past (!) network. 
// 
// [[Rcpp::export]]
arma::rowvec compute_reciprocity(const arma::mat& riskset, 
    const arma::mat& adjmat) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // For loop over possible events
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        statrow(i) = adjmat(riskset(i,1), riskset(i,0));
    }

    return statrow;
}

// compute_otp
//
// Function that computes outgoing two-paths at time point t for all dyads in 
// the risk set based on the adjacency matrix. 
// 
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// * actors: vector: holds all actors.
// * adjmat: matrix [actors x actors]: the past (!) network. 
// 
// [[Rcpp::export]]
arma::rowvec compute_otp(const arma::mat& riskset, const arma::vec& actors,
    const arma::mat& adjmat) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // For loop over possible events
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        // For loop over actors
		for(arma::uword h = 0; h < actors.n_elem; h++) {
            // s -> h, h -> r
		    statrow(i) += std::min(adjmat(riskset(i,0), h),
				adjmat(h, riskset(i,1)));
		}
    }

    return statrow;
}

// compute_itp
//
// Function that computes incoming two-paths at time point t for all dyads in 
// the risk set based on the adjacency matrix. 
// 
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// * actors: vector: holds all actors.
// * adjmat: matrix [actors x actors]: the past (!) network. 
// 
// [[Rcpp::export]]
arma::rowvec compute_itp(const arma::mat& riskset, const arma::vec& actors,
    const arma::mat& adjmat) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // For loop over possible events
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        // For loop over actors
		for(arma::uword h = 0; h < actors.n_elem; h++) {
            // r -> h, h -> s
		    statrow(i) += std::min(adjmat(riskset(i,1), h),
				adjmat(h, riskset(i,0)));
		}
    }

    return statrow;
}

// compute_osp
//
// Function that computes outbound shared partners at time point t for all 
// dyads in the risk set based on the adjacency matrix. 
// 
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// * actors: vector: holds all actors.
// * adjmat: matrix [actors x actors]: the past (!) network. 
// 
// [[Rcpp::export]]
arma::rowvec compute_osp(const arma::mat& riskset, const arma::vec& actors,
    const arma::mat& adjmat) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // For loop over possible events
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        // For loop over actors
		for(arma::uword h = 0; h < actors.n_elem; h++) {
            // s -> h, r -> h
		    statrow(i) += std::min(adjmat(riskset(i,0), h),
				adjmat(riskset(i,1), h));
		}
    }

    return statrow;
}

// compute_isp
//
// Function that computes inbound shared partners at time point t for all 
// dyads in the risk set based on the adjacency matrix. 
// 
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// * actors: vector: holds all actors.
// * adjmat: matrix [actors x actors]: the past (!) network. 
// 
// [[Rcpp::export]]
arma::rowvec compute_isp(const arma::mat& riskset, const arma::vec& actors,
    const arma::mat& adjmat) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // For loop over possible events
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        // For loop over actors
		for(arma::uword h = 0; h < actors.n_elem; h++) {
            // h -> s, h -> r
		    statrow(i) += std::min(adjmat(h, riskset(i,0)),
				adjmat(h, riskset(i,1)));
		}
    }

    return statrow;
}

// compute_pshift
//
// Function that computes participation shift statistics based on the previous 
// event. 
// 
// * type: integer (1: AB-BA, 2: AB-BY, 3: AB-XA, 4: AB-XB, 5: AB-XY, 6: AB-AY)
// * last_event: matrix [time, event id, weight]: holds the last observed event
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// [[Rcpp::export]]
arma::rowvec compute_pshift(int type, const arma::mat& last_event, 
    const arma::mat& riskset) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // Check if there is a last event
    bool skip = last_event.has_nan();
    
    if(!skip) {
        // Actors in the last event
        int ac1 = riskset(last_event(1), 0);
        int ac2 = riskset(last_event(1), 1);

        // Initialize ps_dyads vector
        arma::uvec ps_dyads = {0};

        // 1: pshift AB-BA: Find reverse event
        if(type == 1) {
            ps_dyads = arma::find(riskset.col(0) == ac2 && 
                riskset.col(1) == ac1);
        }    
        // 2: pshift AB-BY: Find receiver -> other actors events
        if(type == 2) {
            ps_dyads = arma::find(riskset.col(0) == ac2 && 
                riskset.col(1) != ac1 && riskset.col(1) != ac2);
        }   
        // 3: pshift AB-XA: Find other actors -> sender events
        if(type == 3) {
            ps_dyads = arma::find(riskset.col(1) == ac1 && 
                riskset.col(0) != ac1 && riskset.col(0) != ac2);
        }   
        // 4: pshift AB-XB: Find other actors -> receiver events
        if(type == 4) {
            ps_dyads = arma::find(riskset.col(1) == ac2 && 
                riskset.col(0) != ac1 && riskset.col(0) != ac2);
        }    
        // 5: pshift AB-XY: Find other actors -> other actors events
        if(type == 5) {
            ps_dyads = arma::find(riskset.col(0) != ac1 && 
                riskset.col(0) != ac2 && riskset.col(1) != ac1 &&
                riskset.col(1) != ac2);
        }   
        // 6: pshift AB-AY: Find sender -> other actors events
        if(type == 6) {
            ps_dyads = arma::find(riskset.col(0) == ac1 && 
                riskset.col(1) != ac1 && riskset.col(1) != ac2);
        }    
        
        // For loop over ps_dyads
        for(arma::uword i = 0; i < ps_dyads.n_elem; ++i) {
            statrow(ps_dyads(i)) = 1;
        }     
    }  
    
    return statrow;
}

// NOTE: NOT TESTED YET
// compute_sp
//
// Function that computes shared partners at time point t for all undirected 
// dyads in the risk set based on the adjacency matrix. 
// 
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// * actors: vector: holds all actors.
// * adjmat: matrix [actors x actors]: the past (!) network. 
// 
// [[Rcpp::export]]
arma::rowvec compute_sp(const arma::mat& riskset, const arma::vec& actors,
    const arma::mat& adjmat) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // For loop over possible events
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        // For loop over actors
		for(arma::uword h = 0; h < actors.n_elem; h++) {
            // s <-> h
            int sh = adjmat(riskset(i,0), h) + adjmat(h, riskset(i,0));
            // h <-> r
            int rh = adjmat(riskset(i,1), h) + adjmat(h, riskset(i,1));
		    statrow(i) += std::min(sh,rh);
		}
    }

    return statrow;
}

// compute_send
// 
// Function that computes a sender exogenous covariate statistic.
// 
// * event: matrix [time, event id, weight]: holds the current observed event
// * values: matrix [actor id, time, covariate] : holds the covariate values
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// [[Rcpp::export]]
arma::rowvec compute_send(const arma::mat& event, const arma::mat& values, 
    const arma::mat& riskset) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // Time of the event
    double time = event(0,0);

    // For loop over possible events in the riskset
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        arma::uword actor = riskset(i,0);
        
        // All values before the current timepoint 
        arma::uvec index = arma::find(values.col(0) == actor && 
            values.col(1) <= time);
        arma::mat actor_values = values.rows(index);

        // Index with maximum time
        arma::uword max_index = arma::index_max(actor_values.col(1));
        statrow(i) = actor_values(max_index, 2);
    }

    return statrow; 
}

// compute_receive
// 
// Function that computes a receiver exogenous covariate statistic.
// 
// * event: matrix [time, event id, weight]: holds the current observed event
// * values: matrix [actor id, time, covariate] : holds the covariate values
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// [[Rcpp::export]]
arma::rowvec compute_receive(const arma::mat& event, const arma::mat& values, 
    const arma::mat& riskset) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // Time of the event
    double time = event(0,0);

    // For loop over possible events in the riskset
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        arma::uword actor = riskset(i,1);
        
        // All values before the current timepoint 
        arma::uvec index = arma::find(values.col(0) == actor && 
            values.col(1) <= time);
        arma::mat actor_values = values.rows(index);

        // Index with maximum time
        arma::uword max_index = arma::index_max(actor_values.col(1));
        statrow(i) = actor_values(max_index, 2);
    }

    return statrow; 
}

// TO DO: Absolute difference! 
// compute_dyad
// 
// Function that computes a number of dyadic exogenous covariate statistics 
// (same, difference, mean, maximum, minimum). 
// 
// * type: integer (1: same, 2: difference, 3: mean, 4: minimum, 5: maximum)
// * event: matrix [time, event id, weight]: holds the current observed event
// * values: matrix [actor id, time, covariate] : holds the covariate values
// * riskset: matrix [sender, receiver, type, id]: holds all the possible 
// events.
// [[Rcpp::export]]
arma::rowvec compute_dyad(int type, const arma::mat& event, 
    const arma::mat& values, const arma::mat& riskset) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // Time of the event
    double time = event(0,0);

    // For loop over possible events in the riskset
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        arma::uword actor0 = riskset(i,0);
        arma::uword actor1 = riskset(i,1);
        
        // All values before the current timepoint 
        arma::uvec index0 = arma::find(values.col(0) == actor0 && 
            values.col(1) <= time);
        arma::mat actor0_values = values.rows(index0);
        arma::uvec index1 = arma::find(values.col(0) == actor1 && 
            values.col(1) <= time);
        arma::mat actor1_values = values.rows(index1);

        // Index with maximum time
        arma::uword max_index0 = arma::index_max(actor0_values.col(1));
        arma::uword max_index1 = arma::index_max(actor1_values.col(1));

        // Values
        double val0 = actor0_values(max_index0, 2);
        double val1 = actor1_values(max_index1, 2);

        // 1: Same
        if(type == 1) {statrow(i) = (val0==val1);}
        // 2: Difference
        if(type == 2) {statrow(i) = (val0-val1);}

        arma::vec both = {val0, val1};
        // 3: Average
        if(type == 3) {statrow(i) = mean(both);}
        // 4: Minimum
        if(type == 4) {statrow(i) = min(both);}
        // 5: Maximum
        if(type == 5) {statrow(i) = max(both);}
    }

    // Absolute difference
    if(type == 2) {statrow = arma::abs(statrow);}

    return statrow; 
}

// init_lastActive
//
// Initializes the lastActive matrix
// [[Rcpp::export]]
arma::mat init_lastActive(arma::mat lastActive, const arma::mat& slice, 
    const arma::mat& edgelist, const arma::mat& riskset) {

    // Fill lastActive
    lastActive.fill(arma::datum::inf);

    // Select the past
    double time = slice(0,0);
    arma::uvec pastkey = arma::find(edgelist.col(0) < time);
    arma::mat past = edgelist.rows(pastkey);

    // For loop over the past
    for(arma::uword i = 0; i < past.n_rows; i++) {
        // Sender and receiver
        int s = riskset(past(i,1),0);
        int r = riskset(past(i,1),1);
        // Add event time to lastActive
        lastActive(s,r) = past(i,0);
    }

    return lastActive;
}

// compute_recency
//
// TO DO: Check how to do this while keeping the rule that statistics only get 
// updated after a new timepoint. TO DO: check type 1 for undirected events.
// @param: time. current timepoint. 
// [[Rcpp::export]]
arma::rowvec compute_recency(int type, const arma::mat& lastActive, const 
    arma::mat& riskset, double time) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // For loop over possible events in the riskset
    for(arma::uword i = 0; i < riskset.n_rows; ++i) {
        arma::uword s = riskset(i,0);
        arma::uword r = riskset(i,1);

        // RecencyContinue (last time active as a dyad)
        if(type == 1) {
            statrow(i) = 1/((time - lastActive(s, r)) + 1);
        }

        // RecencySendSender (last time the sender was active as sender)
        if(type == 2) {
            double lastTime = arma::datum::inf;
            arma::uvec indices = arma::find_finite(lastActive.row(s));
            if(indices.n_elem > 0) {
                arma::rowvec A = lastActive.row(s);
                arma::vec B = A.elem(arma::find(A != arma::datum::inf));
                lastTime = max(B);
            } 
            statrow(i) = 1/((time - lastTime) + 1);
        }

        // RecencySendReceiver (last time the receiver was active as sender)
        if(type == 3) {
            double lastTime = arma::datum::inf;
            arma::uvec indices = arma::find_finite(lastActive.row(r));
            if(indices.n_elem > 0) {
                arma::rowvec A = lastActive.row(r);
                arma::vec B = A.elem(arma::find(A != arma::datum::inf));
                lastTime = max(B);
            } 
            statrow(i) = 1/((time - lastTime) + 1);
        }

        // RecencyReceiveSender (last time the sender was active as receiver)
        if(type == 4) {
            double lastTime = arma::datum::inf;
            arma::uvec indices = arma::find_finite(lastActive.col(s));
            if(indices.n_elem > 0) {
                arma::vec A = lastActive.col(s);
                arma::vec B = A.elem(arma::find(A != arma::datum::inf));
                lastTime = max(B);
            } 
            statrow(i) = 1/((time - lastTime) + 1);
        }

        // RecencyReceiveReceiver (last time the receiver was active as 
        // receiver)
        if(type == 5) {
            double lastTime = arma::datum::inf;
            arma::uvec indices = arma::find_finite(lastActive.col(r));
            if(indices.n_elem > 0) {
                arma::vec A = lastActive.col(r);
                arma::vec B = A.elem(arma::find(A != arma::datum::inf));
                lastTime = max(B);
            } 
            statrow(i) = 1/((time - lastTime) + 1);
        }
    }

    return statrow; 
}

// Helper to compute ranks for the rrank stats
// [[Rcpp::export]]
arma::rowvec ranks(arma::rowvec x, int N) {
    arma::uvec ranksU = N - sort_index(sort_index(x));
    arma::rowvec ranks = arma::conv_to<arma::rowvec>::from(ranksU);
    arma::uvec indices = arma::find(x == 0);
    arma::rowvec reps(indices.n_elem, arma::fill::zeros);
    ranks(indices) = reps;
    return ranks;
}

// [[Rcpp::export]]
arma::rowvec compute_rrank(int type, const arma::mat& lastActive, 
    const arma::mat& riskset, const arma::vec& actors) {

    // Initialize saving space [length = possible events]
    arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

    // Convert inf in lastActive to 0
    arma::mat A = lastActive;
    A.replace(arma::datum::inf, 0);

    // For loop over senders
    for(arma::uword s = 0; s < actors.n_elem; ++s) {
        
        // Saving space
        arma::rowvec senderranks(actors.n_elem, arma::fill::zeros);

        // rrankSend
        if(type == 1) {
            arma::rowvec sendSender = A.row(s);
            senderranks = ranks(sendSender, lastActive.n_rows);
        }

        // rrankReceive
        if(type == 2) {
            arma::vec receiveSender = A.col(s);
            arma::rowvec con = arma::conv_to<arma::rowvec>::from(receiveSender);
            senderranks = ranks(con, lastActive.n_rows);
        }

        // For loop over receivers
        for(arma::uword r = 0; r < actors.n_elem; ++r) {
            if(s == r) {continue;}
            // Dyad index
            double dyad = getDyadIndex(s,r,0,actors.n_elem,TRUE);
            statrow(dyad) = 1/senderranks(r);
        }
    }

    // Replace divides by 0 to 0
    statrow.replace(arma::datum::inf, 0);

    return statrow;
}

// compute_stats_tie
//
// Main function that computes statistics for the tie-oriented model.
// 
// @param effects: vector with the effects referenced by integers
// @param edgelist: matrix [time, event id, weight]: the observed relational 
// event history
// @param riskset: matrix [sender, receiver, type, id]: holds all the possible
// events.
// @param actors
// @param adjmat: matrix [actors x actors]: the past (!) network (only 
// relevant when memory is full).  
// @param start
// @param stop
//
// @return 
// 
// [[Rcpp::export]]
arma::cube compute_stats_tie(const arma::vec& effects, 
    const arma::mat& edgelist, const arma::mat& riskset, 
    const arma::vec& actors, arma::mat adjmat, int memory, 
    double memory_param, arma::uword start, arma::uword stop) {

    // Slice the edgelist
    arma::mat slice = edgelist.rows(start, stop);

    // Initialize saving space [time points x possible events x effects]
    arma::cube stat(slice.n_rows, riskset.n_rows, effects.n_elem, 
        arma::fill::zeros);

    // If any of the effects is a recency effect (28-34), initialize the 
    // lastActive matrix. TO DO: check if it also works for undirected events.
    arma::mat lastActive(actors.n_elem, actors.n_elem);
    if(any(effects == 28) | any(effects == 29) | any(effects == 30) | 
        any(effects == 31) | any(effects == 32) | any(effects == 33) |
        any(effects == 34)) {
        lastActive = init_lastActive(lastActive, slice, edgelist, riskset);
    } 
    
    // updateActive: holds the lastActive matrix with updates between events 
    // with the same time point
    arma::mat updateActive = lastActive;

    // For loop over events in the slice
    for(arma::uword m = 0; m < slice.n_rows; m++) {

        // Initialize saving space [1 x possible events x effects]
        arma::cube event_stat(1, riskset.n_rows, effects.n_elem, 
            arma::fill::zeros);
        
        // For loop over effects []
        for(arma::uword p = 0; p < effects.n_elem; ++p) {
            int effect = effects(p);

            // Initialize saving space [length = possible events]
            arma::rowvec statrow(riskset.n_rows, arma::fill::zeros);

            // Compute statistic
            switch(effect) {
                // 1 baseline
                case 1:
                    statrow.fill(1);
                    break;
                // 10 inertia 
                case 10:
                    statrow = compute_inertia(riskset, adjmat);
                    break;
                // 11 reciprocity 
                case 11:
                    statrow = compute_reciprocity(riskset, adjmat);
                    break;
                // 12 indegreeSender  
                case 12:
                    statrow = compute_indeg(1, riskset, adjmat);
                    break;
                // 13 indegreeReceiver 
                case 13:
                    statrow = compute_indeg(2, riskset, adjmat);
                    break;
                // 14 outdegreeSender  
                case 14:
                    statrow = compute_odeg(1, riskset, adjmat);
                    break;
                // 15 outdegreeReceiver 
                case 15:
                    statrow = compute_odeg(2, riskset, adjmat);
                    break;
                // 16 totaldegreeSender  
                case 16:
                    statrow = compute_tdeg(1, riskset, adjmat);
                    break;
                // 17 totaldegreeReceiver 
                case 17:
                    statrow = compute_tdeg(2, riskset, adjmat);
                    break;
                // 18 otp 
                case 18:
                    statrow = compute_otp(riskset, actors, adjmat);
                    break;
                // 19 itp 
                case 19:
                    statrow = compute_itp(riskset, actors, adjmat);
                    break;
                // 20 osp 
                case 20:
                    statrow = compute_osp(riskset, actors, adjmat);
                    break;
                // 21 isp 
                case 21:
                    statrow = compute_isp(riskset, actors, adjmat);
                    break;
                // 22 psABBA
                case 22:
                    if(m > 0) {
                        statrow = compute_pshift(1, slice.row(m-1), riskset);
                    } else if(start > 0) {
                        statrow = compute_pshift(1, edgelist.row(start - 1), 
                            riskset);
                    } else {
                        statrow = statrow;
                    }
                    break;      
                // 23 psABBY
                case 23:
                    if(m > 0) {
                        statrow = compute_pshift(2, slice.row(m-1), riskset);
                    } else if(start > 0) {
                        statrow = compute_pshift(2, edgelist.row(start - 1), 
                            riskset);
                    } else {
                        statrow = statrow;
                    }
                    break;      
                // 24 psABXA
                case 24:
                    if(m > 0) {
                        statrow = compute_pshift(3, slice.row(m-1), riskset);
                    } else if(start > 0) {
                        statrow = compute_pshift(3, edgelist.row(start - 1), 
                            riskset);
                    } else {
                        statrow = statrow;
                    }
                    break;      
                // 25 psABXB
                case 25:
                    if(m > 0) {
                        statrow = compute_pshift(4, slice.row(m-1), riskset);
                    } else if(start > 0) {
                        statrow = compute_pshift(4, edgelist.row(start - 1), 
                            riskset);
                    } else {
                        statrow = statrow;
                    }
                    break;      
                // 26 psABXY
                case 26:
                    if(m > 0) {
                        statrow = compute_pshift(5, slice.row(m-1), riskset);
                    } else if(start > 0) {
                        statrow = compute_pshift(5, edgelist.row(start - 1), 
                            riskset);
                    } else {
                        statrow = statrow;
                    }
                    break;      
                // 27 psABAY
                case 27:
                    if(m > 0) {
                        statrow = compute_pshift(6, slice.row(m-1), riskset);
                    } else if(start > 0) {
                        statrow = compute_pshift(6, edgelist.row(start - 1), 
                            riskset);
                    } else {
                        statrow = statrow;
                    }
                    break;  
                // 28 recencyContinue
                case 28:
                    // Compute stats
                    statrow = compute_recency(1, lastActive, riskset, 
                        slice(m,0));
                    break;
                // 29 recencySendSender
                case 29:
                    // Compute stats
                    statrow = compute_recency(2, lastActive, riskset, 
                        slice(m,0));
                    break;     
                // 30 recencySendReceiver
                case 30:
                    // Compute stats
                    statrow = compute_recency(3, lastActive, riskset, 
                        slice(m,0));
                    break;         
                // 31 recencyReceiveSender
                case 31:
                    // Compute stats
                    statrow = compute_recency(4, lastActive, riskset, 
                        slice(m,0));
                    break;     
                // 32 recencyReceiveReceiver
                case 32:
                    // Compute stats
                    statrow = compute_recency(5, lastActive, riskset, 
                        slice(m,0));
                    break;      
                // 33 rrankSend
                case 33:
                    // Compute stats
                    statrow = compute_rrank(1, lastActive, riskset, actors);
                    break;     
                // 34 rrankReceive
                case 34:
                    // Compute stats
                    statrow = compute_rrank(2, lastActive, riskset, actors);
                    break;                       
                
            }

            // Save stat
            event_stat.slice(p) = statrow;
        }

        // Save the stat
        stat.row(m) = event_stat;
        
        // If we compute the statistics for more than one event
        if(slice.n_rows > 1) {           
            // Update the adjacency matrix
            if(m < (slice.n_rows - 1)) {
                adjmat = compute_adjmat(adjmat, edgelist, riskset, memory, 
                    memory_param, m, m);
            }           
            
            // Update updateActive if any recency effects
            if(any(effects == 28) | any(effects == 29) | any(effects == 30) | 
                any(effects == 31) | any(effects == 32) | any(effects == 33) |
                any(effects == 34)) {
                // Sender and receiver
                int s = riskset(slice(m,1),0);
                int r = riskset(slice(m,1),1);
                // Add event time to lastActive
                updateActive(s,r) = slice(m,0);
                // Update lastActive if the next event has a new time point
                if(m < (slice.n_rows - 1)) {
                    if(slice(m + 1, 0) > slice(m,0)) {
                        lastActive = updateActive;
                    }
                }
            }       
        }            
    }
    
    return stat;
}

