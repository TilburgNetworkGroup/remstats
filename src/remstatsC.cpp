#include <RcppArmadillo.h>
#include "remstats.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//' remStatsC
//'
//' A function to compute statistics and combine the statistics in an array
//' prepared for estimation of a REM with relevent::rem(). Used internally 
//' in remStats. 
//' 
//' param:
//' [effects] integer vector (effects)
//' [edgelist] 3-column edgelist (time, sender, receiver)
//' [riskset] 2-column riskset (sender/actor 1, receiver/actor 2)
//' [evls] 2-column edgelist (event, time) in relevent::rem format
//' [actors] vector with numeric actor IDs (correspod to edgelist, riskset)
//' [covariates] List with matrices
//'     0: [sender_values] matrix (id, time, covariate values)
//'     1: [receiver_values] matrix(id, time, covariate values)
//'     2: [same] matrix(id, time, covariate values)
//'     3: [difference] matrix(id, time, covariate values)
//'     4: [mean] matrix(id, time, covariate values)
//'     5: [min] matrix(id, time, covariate values)
//'     6: [max] matrix(id, time, covariate values)
//'     7: [both_equal_to] matrix(id, time, covariate values)
//' [event_effect] matrix (event effect per column)
//' [weights] vector (length evls) 
//' [equal_val] vector (length ncol both_equal_to minus 2)
//' [int_positions] matrix (effect 1, effect 2)
//'
//' return:
//' [statistics] 3-dimensional array (event time x risk set entry x statistic)
//' 
//[[Rcpp::export]]
arma::cube remStatsC(arma::vec effects, arma::mat edgelist, arma::mat riskset, 
    arma::mat evls, arma::vec actors, Rcpp::List covariates, 
    arma::mat event_effect, arma::vec weights, arma::vec equal_val, 
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
                stat = actorStat(sender_values.cols(se_ind), 1,
                    edgelist, riskset);
                se_counter += 1;
                break;
            // receiver_effect
            case 2 :
                re_ind = {0, 1, re_counter+2};
                stat = actorStat(receiver_values.cols(re_ind), 2, 
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
            // inertia
            case 10 :
                stat = inertia(evls, riskset, weights);
                break;
            // inertia_weighted
            case 11:
                stat = inertia(evls, riskset, weights);
                break;
            // reciprocity
            case 12: 
                stat = reciprocity(edgelist, riskset);
                break;
            // indegree_sender
            case 14:
                stat = degree(edgelist, riskset, 1);
                break;
            // indegree_receiver
            case 15:
                stat = degree(edgelist, riskset, 2);
                break;
            // outdegree_sender
            case 16:
               stat = degree(edgelist, riskset, 3);
                break;
            // outdegree_receiver
            case 17:
                stat = degree(edgelist, riskset, 4);
                break;
            // totaldegree_sender
            case 18:
                stat = degree(edgelist, riskset, 5);
                break;
            // totaldegree_receiver
            case 19:
                stat = degree(edgelist, riskset, 6);
                break;
            // OTP
            case 24:
                stat = triad(actors, edgelist, riskset, 1);
                break;
            // ITP
            case 25:
                stat = triad(actors, edgelist, riskset, 2);
                break;
            // OSP
            case 26:
                stat = triad(actors, edgelist, riskset, 3);
                break;
            // ISP
            case 27:
                stat = triad(actors, edgelist, riskset, 4);
                break;
            // shared_partners
            case 28:
                stat = triadU(actors, edgelist, riskset, FALSE);
                break;
            // unique_sp
            case 29:
                stat = triadU(actors, edgelist, riskset, TRUE);
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