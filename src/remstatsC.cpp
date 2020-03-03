#include <RcppArmadillo.h>
#include "remstats.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//' remStatsC
//'
//' A function to compute statistics and combine the statistics in an array
//' prepared for estimation of a REM with relevent::rem()
//' 
//' @param effects integer vector (effects)
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//' @param evls 2-column edgelist (event, time) in relevent::rem format
//' @param actors vector with numeric actor IDs (correspod to edgelist, riskset)
//' @param weights vector (length evls) 
//'
//' @return statistics 3-dimensional array (event time x risk set entry x 
//'     statistic)
//' 
//[[Rcpp::export]]
arma::cube remStatsC(arma::vec effects, arma::mat edgelist, arma::mat riskset, 
    arma::mat evls, arma::vec actors, arma::vec weights) {

    // Initialize saving space
    arma::cube statistics(edgelist.n_rows, riskset.n_rows, effects.n_elem);

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
            // Inertia
            case 1 :
                stat = inertia(evls, riskset, weights);
                break;
            // Reciprocity
            case 2: 
                stat = reciprocity(edgelist, riskset);
                break;
            // Indegree_sender
            case 3:
                stat = degree(edgelist, riskset, 1);
                break;
            // Indegree_receiver
            case 4:
                stat = degree(edgelist, riskset, 2);
                break;
            // Outdegree_sender
            case 5:
               stat = degree(edgelist, riskset, 3);
                break;
            // Outdegree_receiver
            case 6:
                stat = degree(edgelist, riskset, 4);
                break;
            // Totaldegree_sender
            case 7:
                stat = degree(edgelist, riskset, 5);
                break;
            // Totaldegree_receiver
            case 8:
                stat = degree(edgelist, riskset, 6);
                break;
            // OTP
            case 13:
                stat = triad(actors, edgelist, riskset, 1);
                break;
            // ITP
            case 14:
                stat = triad(actors, edgelist, riskset, 2);
                break;
            // OSP
            case 15:
                stat = triad(actors, edgelist, riskset, 3);
                break;
            // ISP
            case 16:
                stat = triad(actors, edgelist, riskset, 4);
                break;
            // shared_partners
            case 17:
                stat = triadU(actors, edgelist, riskset);
                break;
            // inertia_weighted
            case 24:
                stat = inertia(evls, riskset, weights);
                break;
        }
            
        // Save statistic
        statistics.slice(i) = stat; 
    }

    // Output statistics object
    return statistics;
}