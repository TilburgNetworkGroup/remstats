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
//' 
//' @export
//'
//[[Rcpp::export]]
arma::cube remStatsC(arma::vec effects, arma::mat edgelist, arma::mat riskset, 
    arma::mat evls) {

    // Initialize saving space
    arma::cube statistics(edgelist.n_rows, riskset.n_rows, effects.n_elem);

    // For loop over all possible effects
    for(arma::uword i = 0; i < 2; ++i) {
        // Check whether the effect is requested
        if(any(effects == i)) {
            // Current effect
            int effect = effects(i);
            arma::uvec index = find(effects == i);
            
            // Initialize saving space
            arma::mat stat(edgelist.n_rows, riskset.n_rows, fill::zeros);
            
            switch(effect) {
                // Baseline 
                case 0 :
                    stat.fill(1);
                    break;
                // Inertia
                case 1 :
                    stat = inertia(evls, riskset);
                    break;
            }
            
            // Save statistic
            statistics.slice(index(0)) = stat;
        } 
    }

    // Output statistics object
    return statistics;
}