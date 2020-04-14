#include <RcppArmadillo.h>
#include "remstats.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//' remstatsCpp
//'
//' A function to compute statistics and combine the statistics in an array
//' prepared for estimation of a REM with relevent::rem(). Used internally 
//' in remstats. 
//' 
//' @param effects integer vector (effects)
//' @param standardize logical, indicates whether statistics for endogenous 
//' effects should be standardized
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//' @param actors vector with numeric actor IDs (correspod to edgelist, riskset)
//' @param covariates List with matrices
//'     0: [sender_values] matrix (id, time, covariate values)
//'     1: [receiver_values] matrix(id, time, covariate values)
//'     2: [same] matrix(id, time, covariate values)
//'     3: [difference] matrix(id, time, covariate values)
//'     4: [mean] matrix(id, time, covariate values)
//'     5: [min] matrix(id, time, covariate values)
//'     6: [max] matrix(id, time, covariate values)
//'     7: [both_equal_to] matrix(id, time, covariate values)
//' @param event_effect matrix (event effect per column)
//' @param types vector
//' @param weights vector (length evls) 
//' @param equal_val vector (length ncol both_equal_to minus 2)
//' @param int_positions matrix (effect 1, effect 2)
//'
//' @return statistics 3-dimensional array (event time x risk set entry x 
//' statistic)
//' 
//[[Rcpp::export]]
arma::cube remstatsCpp(arma::vec effects, bool standardize, arma::mat edgelist, 
    arma::mat riskset, arma::vec actors, Rcpp::List covariates, 
    arma::mat event_effect, arma::vec types, arma::vec weights, arma::vec equal_val, 
    arma::mat int_positions) {

    // Initialize saving space
    arma::cube statistics(edgelist.n_rows, riskset.n_rows, effects.n_elem);

    // Prepare exogenous effects
    //  Matrices
    arma::mat sender_values = covariates(0);
    arma::mat receiver_values = covariates(1);
    arma::mat same = covariates(2);
    arma::mat difference = covariates(3);
    arma::mat mean_values = covariates(4);
    arma::mat min_values = covariates(5);
    arma::mat max_values = covariates(6);
    arma::mat both_equal_to = covariates(7);

    //  Counters
    arma::uword se_counter = 0;
    arma::uword re_counter = 0;
    arma::uword s_counter = 0;
    arma::uword d_counter = 0;
    arma::uword me_counter = 0;
    arma::uword mi_counter = 0;
    arma::uword ma_counter = 0;
    arma::uword bet_counter = 0;

    //  Initialize indices
    arma::uvec se_ind = {0, 0, 0};
    arma::uvec re_ind = {0, 0, 0};
    arma::uvec s_ind = {0, 0, 0};
    arma::uvec d_ind = {0, 0, 0};
    arma::uvec me_ind = {0, 0, 0};
    arma::uvec mi_ind = {0, 0, 0};
    arma::uvec ma_ind = {0, 0, 0};
    arma::uvec bet_ind = {0, 0, 0};

    // Prepare event effects
    // Counter
    arma::uword e_counter = 0;

    // Prepare type effects
    // Counter
    arma::uword t_counter = 0;

    // Prepare interaction effects
    // Counter
    arma::uword int_counter = 0;

    // For loop over effects
    for(arma::uword i = 0; i < effects.n_elem; ++i) {
        // Current effect
        int effect = effects(i);
            
        // Initialize saving space
        arma::mat stat(edgelist.n_rows, riskset.n_rows, fill::zeros);
        
        switch(effect) {
            // Baseline 
            case 0 :
                stat.fill(1);
                break;
            // sender_effect 
            case 1 :
                se_ind = {0, 1, se_counter+2};
                stat = actorstat(sender_values.cols(se_ind), 1,
                    edgelist, riskset);
                se_counter += 1;
                break;
            // receiver_effect
            case 2 :
                re_ind = {0, 1, re_counter+2};
                stat = actorstat(receiver_values.cols(re_ind), 2, 
                    edgelist, riskset);
                re_counter += 1;
                break;
            // same
            case 3 :
                s_ind = {0, 1, s_counter+2};
                stat = dyadstat(same.cols(s_ind), 1, edgelist, riskset, 0);
                s_counter += 1;
                break;
            // difference
            case 4 :
                d_ind = {0, 1, d_counter+2};
                stat = dyadstat(difference.cols(d_ind), 2, 
                    edgelist, riskset, 0);
                d_counter += 1;
                break;
            // mean
            case 5 :
                me_ind = {0, 1, me_counter+2};
                stat = dyadstat(mean_values.cols(me_ind), 3, 
                    edgelist, riskset, 0);
                me_counter += 1;
                break;
            // min
            case 6 :
                mi_ind = {0, 1, mi_counter+2};
                stat = dyadstat(min_values.cols(mi_ind), 4, 
                    edgelist, riskset, 0);
                mi_counter += 1;
                break;
            // max
            case 7 :
                ma_ind = {0, 1, ma_counter+2};
                stat = dyadstat(max_values.cols(ma_ind), 5, 
                    edgelist, riskset, 0);
                ma_counter += 1;
                break;
            // both_equal_to
            case 8 :
                bet_ind = {0, 1, bet_counter+2};
                stat = dyadstat(both_equal_to.cols(bet_ind), 6, 
                    edgelist, riskset, equal_val(bet_counter));
                bet_counter += 1;
                break;
            // event_effect
            case 9 :
                stat.each_col() = event_effect.col(e_counter);
                e_counter += 1;
                break;
            // type_effect
            case 10:
                stat = typestat(edgelist, riskset, types(t_counter));
                t_counter += 1;
                break;
            // inertia
            case 11 :
                stat = inertia(edgelist, riskset, weights, standardize);
                break;
            // inertia_weighted
            case 12 :
                stat = inertia(edgelist, riskset, weights, standardize);
                break;
            // inertia_type
            case 13 :
                stat = inertia_type(edgelist, riskset, weights, standardize);
                break;
            // inertia_type_weighted
            case 14 :
                stat = inertia_type(edgelist, riskset, weights, standardize);
                break;
            // reciprocity
            case 15 : 
                stat = reciprocity(edgelist, riskset, weights, standardize);
                break;
            // reciprocity_weighted
            case 16 : 
                stat = reciprocity(edgelist, riskset, weights, standardize);
                break;
            // indegree_sender
            case 17 :
                stat = degree(edgelist, riskset, 1, standardize);
                break;
            // indegree_receiver
            case 18 :
                stat = degree(edgelist, riskset, 2, standardize);
                break;
            // outdegree_sender
            case 19 :
               stat = degree(edgelist, riskset, 3, standardize);
                break;
            // outdegree_receiver
            case 20 :
                stat = degree(edgelist, riskset, 4, standardize);
                break;
            // totaldegree_sender
            case 21 :
                stat = degree(edgelist, riskset, 5, standardize);
                break;
            // totaldegree_receiver
            case 22 :
                stat = degree(edgelist, riskset, 6, standardize);
                break;
            //rrank_send
            case 25:
                stat = recency(edgelist, actors, 1);
                break;
            //rrank_receive
            case 26:
                stat = recency(edgelist, actors, 2);
                break;
            // OTP
            case 27 :
                stat = triad(actors, edgelist, riskset, 1, standardize);
                break;
            // ITP
            case 28 :
                stat = triad(actors, edgelist, riskset, 2, standardize);
                break;
            // OSP
            case 29 :
                stat = triad(actors, edgelist, riskset, 3, standardize);
                break;
            // ISP
            case 30 :
                stat = triad(actors, edgelist, riskset, 4, standardize);
                break;
            // shared_partners
            case 31 :
                stat = triadU(actors, edgelist, riskset, FALSE, standardize);
                break;
            // unique_sp
            case 32 :
                stat = triadU(actors, edgelist, riskset, TRUE, standardize);
                break;
            // shared_partners_type
            case 33 :
                stat = triadU_type(actors, edgelist, riskset, FALSE, 
                    standardize);
                break;
            // unique_sp_type
            case 34 :
                stat = triadU_type(actors, edgelist, riskset, TRUE, 
                    standardize);
                break;
            // PSAB-BA
            case 35 :
                stat = pshift(edgelist, riskset, 1);
                break;
            // PSAB-BY
            case 36 :
                stat = pshift(edgelist, riskset, 2);
                break;
            // PSAB-XA
            case 37 :
                stat = pshift(edgelist, riskset, 3);
                break;
            // PSAB-XB
            case 38 :
                stat = pshift(edgelist, riskset, 4);
                break;
            // PSAB-XY
            case 39 :
                stat = pshift(edgelist, riskset, 5);
                break;
            // PSAB-AY
            case 40 :
                stat = pshift(edgelist, riskset, 6);
                break;
            // interaction effects
            case 999:
                stat = statistics.slice(int_positions(int_counter, 0))% 
                    statistics.slice(int_positions(int_counter, 1));
                int_counter += 1;
                break;
        }
            
        // Save statistic
        statistics.slice(i) = stat; 
    }

    // Output statistics object
    return statistics;
}

//' remstatsMWCpp
//'
//' A function to compute statistics and combine the statistics in an array
//' prepared for estimation of a moving window REM with relevent::rem(). Used 
//' internally in remstatsMW. 
//' 
//' @param effects integer vector (effects)
//' @param standardize logical, indicates whether statistics for endogenous 
//' effects should be standardized
//' @param full_edgelist 3-column edgelist (time, sender, receiver)
//' @param window_edgelist 3-column edgelist (time, sender, receiver)
//' @param window_length numeric value.
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//' @param actors vector with numeric actor IDs (correspod to edgelist, riskset)
//' @param covariates List with matrices
//'     0: [sender_values] matrix (id, time, covariate values)
//'     1: [receiver_values] matrix(id, time, covariate values)
//'     2: [same] matrix(id, time, covariate values)
//'     3: [difference] matrix(id, time, covariate values)
//'     4: [mean] matrix(id, time, covariate values)
//'     5: [min] matrix(id, time, covariate values)
//'     6: [max] matrix(id, time, covariate values)
//'     7: [both_equal_to] matrix(id, time, covariate values)
//' @param event_effect matrix (event effect per column)
//' @param types vector
//' @param full_weights vector (length evls) 
//' @param equal_val vector (length ncol both_equal_to minus 2)
//' @param int_positions matrix (effect 1, effect 2)
//'
//' @return statistics 3-dimensional array (event time x risk set entry x 
//' statistic)
//' 
//[[Rcpp::export]]
arma::cube remstatsMWCpp(arma::vec effects, bool standardize, 
    arma::mat full_edgelist, arma::mat window_edgelist, double window_length, 
    arma::mat riskset, arma::vec actors, Rcpp::List covariates, 
    arma::mat event_effect, arma::vec types, arma::vec full_weights, 
    arma::vec equal_val, arma::mat int_positions) {

    // Initialize saving space
    arma::cube statistics(window_edgelist.n_rows, riskset.n_rows, 
        effects.n_elem);

    // Prepare exogenous effects
    //  Matrices
    arma::mat sender_values = covariates(0);
    arma::mat receiver_values = covariates(1);
    arma::mat same = covariates(2);
    arma::mat difference = covariates(3);
    arma::mat mean_values = covariates(4);
    arma::mat min_values = covariates(5);
    arma::mat max_values = covariates(6);
    arma::mat both_equal_to = covariates(7);

    //  Counters
    arma::uword se_counter = 0;
    arma::uword re_counter = 0;
    arma::uword s_counter = 0;
    arma::uword d_counter = 0;
    arma::uword me_counter = 0;
    arma::uword mi_counter = 0;
    arma::uword ma_counter = 0;
    arma::uword bet_counter = 0;

    //  Initialize indices
    arma::uvec se_ind = {0, 0, 0};
    arma::uvec re_ind = {0, 0, 0};
    arma::uvec s_ind = {0, 0, 0};
    arma::uvec d_ind = {0, 0, 0};
    arma::uvec me_ind = {0, 0, 0};
    arma::uvec mi_ind = {0, 0, 0};
    arma::uvec ma_ind = {0, 0, 0};
    arma::uvec bet_ind = {0, 0, 0};

    // Prepare event effects
    // Counter
    arma::uword e_counter = 0;

    // Prepare type effects
    // Counter
    arma::uword t_counter = 0;

    // Prepare interaction effects
    // Counter
    arma::uword int_counter = 0;

    // For loop over effects
    for(arma::uword i = 0; i < effects.n_elem; ++i) {
        // Current effect
        int effect = effects(i);
            
        // Initialize saving space
        arma::mat stat(window_edgelist.n_rows, riskset.n_rows, fill::zeros);
        
        switch(effect) {
            // Baseline 
            case 0 :
                stat.fill(1);
                break;
            // sender_effect 
            case 1 :
                se_ind = {0, 1, se_counter+2};
                stat = actorstat(sender_values.cols(se_ind), 1,
                    window_edgelist, riskset);
                se_counter += 1;
                break;
            // receiver_effect
            case 2 :
                re_ind = {0, 1, re_counter+2};
                stat = actorstat(receiver_values.cols(re_ind), 2, 
                    window_edgelist, riskset);
                re_counter += 1;
                break;
            // same
            case 3 :
                s_ind = {0, 1, s_counter+2};
                stat = dyadstat(same.cols(s_ind), 1, window_edgelist, 
                    riskset, 0);
                s_counter += 1;
                break;
            // difference
            case 4 :
                d_ind = {0, 1, d_counter+2};
                stat = dyadstat(difference.cols(d_ind), 2, 
                    window_edgelist, riskset, 0);
                d_counter += 1;
                break;
            // mean
            case 5 :
                me_ind = {0, 1, me_counter+2};
                stat = dyadstat(mean_values.cols(me_ind), 3, 
                    window_edgelist, riskset, 0);
                me_counter += 1;
                break;
            // min
            case 6 :
                mi_ind = {0, 1, mi_counter+2};
                stat = dyadstat(min_values.cols(mi_ind), 4, 
                    window_edgelist, riskset, 0);
                mi_counter += 1;
                break;
            // max
            case 7 :
                ma_ind = {0, 1, ma_counter+2};
                stat = dyadstat(max_values.cols(ma_ind), 5, 
                    window_edgelist, riskset, 0);
                ma_counter += 1;
                break;
            // both_equal_to
            case 8 :
                bet_ind = {0, 1, bet_counter+2};
                stat = dyadstat(both_equal_to.cols(bet_ind), 6, 
                    window_edgelist, riskset, equal_val(bet_counter));
                bet_counter += 1;
                break;
            // event_effect
            case 9 :
                stat.each_col() = event_effect.col(e_counter);
                e_counter += 1;
                break;
            // type_effect
            case 10:
                stat = typestat(window_edgelist, riskset, types(t_counter));
                t_counter += 1;
                break;
            // inertia
            case 11 :
                stat = inertiaMW(full_edgelist, window_edgelist, window_length, 
                    riskset, full_weights, standardize);
                break;
            // inertia_weighted
            case 12:
                stat = inertiaMW(full_edgelist, window_edgelist, window_length, 
                    riskset, full_weights, standardize);
                break;
            // inertia_type
            case 13:
                stat = inertia_typeMW(full_edgelist, window_edgelist, 
                    window_length, riskset, full_weights, standardize);
                break;
            // inertia_type_weighted
            case 14:
                stat = inertia_typeMW(full_edgelist, window_edgelist, 
                    window_length, riskset, full_weights, standardize);
                break;
            // shared_partners
            case 31:
                stat = triadUMW(actors, full_edgelist, window_edgelist, window_length, riskset, FALSE, standardize);
                break;
            // unique_sp
            case 32:
                stat = triadUMW(actors, full_edgelist, window_edgelist, window_length, riskset, TRUE, standardize);
                break;
            // shared_partners_type
            case 33:
                stat = triadU_typeMW(actors, full_edgelist, window_edgelist, 
                    window_length, riskset, FALSE, standardize);
                break;
            // unique_sp_type
            case 34:
                stat = triadU_typeMW(actors, full_edgelist, window_edgelist, 
                    window_length, riskset, TRUE, standardize);
                break;
            // PSAB-BA
            case 35 :
                stat = pshift(window_edgelist, riskset, 1);
                break;
            // PSAB-BY
            case 36 :
                stat = pshift(window_edgelist, riskset, 2);
                break;
            // PSAB-XA
            case 37 :
                stat = pshift(window_edgelist, riskset, 3);
                break;
            // PSAB-XB
            case 38 :
                stat = pshift(window_edgelist, riskset, 4);
                break;
            // PSAB-XY
            case 39 :
                stat = pshift(window_edgelist, riskset, 5);
                break;
            // PSAB-AY
            case 40 :
                stat = pshift(window_edgelist, riskset, 6);
                break;
            // interaction effects
            case 999:
                stat = statistics.slice(int_positions(int_counter, 0))% 
                    statistics.slice(int_positions(int_counter, 1));
                int_counter += 1;
                break;
        }
            
        // Save statistic
        statistics.slice(i) = stat; 
    }

    // Output statistics object
    return statistics;
}