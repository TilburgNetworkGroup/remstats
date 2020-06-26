#include <RcppArmadillo.h>
#include "compute_effects.h"
// [[Rcpp::depends(RcppArmadillo)]]

// compute_stats
//
// Calls functions in compute_effects.h to compute statistics, scales 
// statistics, and combines all computed statistics in an array. 
// 
//  effects: vector of length p with integers referring to effect types.
//  edgelist: matrix [time, sender/actor1, receiver/actor2, (type), 
// riskset position]
//  riskset: matrix [sender/actor1, receiver/actor2, (type)]
//  start: integer value referring to the first row + 1in the edgelist 
// for which effects have to be computed
//  stop: integer value referring to the last row + 1 in the edgelist 
// for which effects have to be computed
//  values: list of length p with matrices with exogenous information 
// for the exogenous effects and NULL for all other effects
//  scaling: vector of length p with integer values referring to the 
// type of scaling method that has to be applied to the statistic.
//  memory_value: vector of length p with numeric values referring to 
// the length of the window in which past events are considered for endogenous 
// effects
//  with_type: vector of length p with logical values indicating whether 
// effects have to be computed considering event types a dependent variable
//  event_weights: matrix with p columns where each column refers to the 
// weights of the events for a specific effect
//  equal_val: vector of length p 
//
//  statistics: array [timepoint x riskset position x statistic]
//
//[[Rcpp::export]]
arma::cube compute_stats(const arma::vec& effects, const arma::mat& edgelist, 
    const arma::mat& riskset, int start, int stop, 
    const Rcpp::List& values, const arma::vec& scaling, 
    const arma::vec& memory_value, const arma::vec& with_type, 
    const arma::mat& event_weights, const arma::vec& equal_val) {
    
    // Extract the part of the edgelist for which statistics need to be computed
    arma::mat small_edgelist = edgelist.rows((start-1), (stop-1));
    
    // Initialize saving space
    arma::cube statistics(small_edgelist.n_rows, riskset.n_rows, effects.n_elem);

    // For loop over effects
    for(arma::uword i = 0; i < effects.n_elem; ++i) {
        // Current effect
        int effect = effects(i);

        // Initialize saving space
        arma::mat stat(statistics.n_rows, statistics.n_cols, arma::fill::zeros);
        arma::mat deno(statistics.n_rows, statistics.n_cols, arma::fill::zeros);

        switch(effect) {
            // baseline 
            case 1 :
                stat.fill(1);
                break;
            // send
            case 2 :
                stat = compute_actorEffect(values[i], 1, edgelist, riskset, 
                    start, stop);
                break;
            // receiv
            case 3 :
                stat = compute_actorEffect(values[i], 2, edgelist, riskset, 
                    start, stop);
                break;
            // same
            case 4 :
                stat = compute_dyadEffect(values[i], 1, edgelist, riskset,  
                    start, stop, 0);
                break;
            // difference
            case 5 :
                stat = compute_dyadEffect(values[i], 2, edgelist, riskset,  
                    start, stop, 0);
                break;
            // mean
            case 6 :
                stat = compute_dyadEffect(values[i], 3, edgelist, riskset,  
                    start, stop, 0);
                break;
            // minimum
            case 7 :
                stat = compute_dyadEffect(values[i], 4, edgelist, riskset,  
                    start, stop, 0);
                break;
            // maximum
            case 8 :
                stat = compute_dyadEffect(values[i], 5, edgelist, riskset,  
                    start, stop, 0);
                break;
            // equate
            case 9 :
                stat = compute_dyadEffect(values[i], 6, edgelist, riskset,  
                    start, stop, equal_val(i));
                break;
            // inertia 
            case 10 :
                stat = compute_inertia(edgelist, riskset, memory_value(i), 
                    with_type(i), event_weights.col(i), start, stop);
                if(scaling(i) == 2) {
                    deno = compute_degree(3, edgelist, riskset, 
                        memory_value(i), with_type(i), event_weights.col(i), 
                        start, stop);
                    stat = stat/deno;
                    stat.replace(arma::datum::nan, 0);
                }
                if(scaling(i) == 3) {
                    stat = standardize(stat);                    
                }
                break;
            // reciprocity
            case 11 :
                stat = compute_reciprocity(edgelist, riskset, memory_value(i), 
                    with_type(i), event_weights.col(i), start, stop);
                if(scaling(i) == 2) {
                    deno = compute_degree(1, edgelist, riskset, 
                        memory_value(i), with_type(i), event_weights.col(i), 
                        start, stop);
                    stat = stat/deno;
                    stat.replace(arma::datum::nan, 0);
                }
                if(scaling(i) == 3) {
                    stat = standardize(stat);                    
                }
                break;
            // indegreeSender
            case 12 :
                stat = compute_degree(1, edgelist, riskset, memory_value(i), 
                    with_type(i), event_weights.col(i), start, stop);
                if(scaling(i) == 2) {
                    stat = divide_by_past(stat);
                }
                if(scaling(i) == 3) {
                    stat = standardize(stat);                    
                }
                break;
            // indegreeReceiver
            case 13 :
                stat = compute_degree(2, edgelist, riskset, memory_value(i), 
                    with_type(i), event_weights.col(i), start, stop);
                if(scaling(i) == 2) {
                    stat = divide_by_past(stat);
                }
                if(scaling(i) == 3) {
                    stat = standardize(stat);                    
                }
                break;
            // outdegreeSender
            case 14 :
                stat = compute_degree(3, edgelist, riskset, memory_value(i), 
                    with_type(i), event_weights.col(i), start, stop);
                if(scaling(i) == 2) {
                    stat = divide_by_past(stat);
                }
                if(scaling(i) == 3) {
                    stat = standardize(stat);                    
                }
                break;
            // outdegreeReceiver
            case 15 :
                stat = compute_degree(4, edgelist, riskset, memory_value(i), 
                    with_type(i), event_weights.col(i), start, stop);
                if(scaling(i) == 2) {
                    stat = divide_by_past(stat);
                }
                if(scaling(i) == 3) {
                    stat = standardize(stat);                    
                }
                break;
            // totaldegreeSender
            case 16 :
                stat = compute_degree(5, edgelist, riskset, memory_value(i), 
                    with_type(i), event_weights.col(i), start, stop);
                if(scaling(i) == 2) {
                    stat = divide_by_2past(stat);
                }
                if(scaling(i) == 3) {
                    stat = standardize(stat);                    
                }
                break;
            // totaldegreeReceiver
            case 17 :
                stat = compute_degree(6, edgelist, riskset, memory_value(i), 
                    with_type(i), event_weights.col(i), start, stop);
                if(scaling(i) == 2) {
                    stat = divide_by_2past(stat);
                }
                if(scaling(i) == 3) {
                    stat = standardize(stat);                    
                }
                break;
            // otp
            case 18 :
                stat = compute_triad(1, edgelist, riskset,                
                    memory_value(i), with_type(i), event_weights.col(i), start, 
                    stop);
                if(scaling(i) == 2) {
                    stat = standardize(stat);                    
                }
                break;
            // itp
            case 19 :
                stat = compute_triad(2, edgelist, riskset,  
                    memory_value(i), with_type(i), event_weights.col(i), start, 
                    stop);
                if(scaling(i) == 2) {
                    stat = standardize(stat);                    
                }
                break;
            // osp
            case 20 :
                stat = compute_triad(3, edgelist, riskset,  
                    memory_value(i), with_type(i), event_weights.col(i), start, 
                    stop);
                if(scaling(i) == 2) {
                    stat = standardize(stat);                    
                }
                break;
            // isp
            case 21 :
                stat = compute_triad(4, edgelist, riskset,  
                    memory_value(i), with_type(i), event_weights.col(i), start, 
                    stop);
                if(scaling(i) == 2) {
                    stat = standardize(stat);                    
                }
                break;
            // sp
            case 22 :
                stat = compute_triad(5, edgelist, riskset,  
                    memory_value(i), with_type(i), event_weights.col(i), start, 
                    stop);
                if(scaling(i) == 2) {
                    stat = standardize(stat);                    
                }
                break;
            // spUnique
            case 23 :
                stat = compute_triad(6, edgelist, riskset,  
                    memory_value(i), with_type(i), event_weights.col(i), start, stop);
                if(scaling(i) == 2) {
                    stat = standardize(stat);                    
                }
                break;
            // PSAB-BA
            case 24 :
                stat = compute_pshift(1, edgelist, riskset, with_type(i), 
                    start, stop);
                break;
            // PSAB-BY
            case 25 :
                stat = compute_pshift(2, edgelist, riskset, with_type(i), 
                    start, stop);
                break;
            // PSAB-XA
            case 26 :
                stat = compute_pshift(3, edgelist, riskset, with_type(i), 
                    start, stop);
                break;
            // PSAB-XB
            case 27 :
                stat = compute_pshift(4, edgelist, riskset, with_type(i), 
                    start, stop);
                break;
            // PSAB-XY
            case 28 :
                stat = compute_pshift(5, edgelist, riskset, with_type(i), 
                    start, stop);
                break;
            // PSAB_AY
            case 29 :
                stat = compute_pshift(6, edgelist, riskset, with_type(i), 
                    start, stop);
                break;
            // rrankSend
            case 30 :
                stat = compute_rrank(1, edgelist, riskset, with_type(i), start, 
                    stop);
                break;
            // rrankReceive
            case 31 :
                stat = compute_rrank(2, edgelist, riskset, with_type(i), start, 
                    stop);
                break;
            // baselineType
            case 32 :
                stat = compute_baselineType(values[i], edgelist, riskset, 
                    start, stop);
                break;
            // interact
            case 33 :
                stat = compute_interact(values[i], statistics);
                break;
            // event
            case 34 :
                stat = compute_eventEffect(values[i], statistics);
                break;
        }

        // Save statistic
        statistics.slice(i) = stat; 
    }
    
    return statistics;
}