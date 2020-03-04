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
//' [sender_values] matrix (id, time, sender covariate values)
//' [receiver_values] matrix(id, time, receiver covariate values)
//' [weights] vector (length evls) 
//'
//' return:
//' [statistics] 3-dimensional array (event time x risk set entry x statistic)
//' 
//[[Rcpp::export]]
arma::cube remStatsC(arma::vec effects, arma::mat edgelist, arma::mat riskset, 
    arma::mat evls, arma::vec actors, arma::mat sender_values, 
    arma::mat receiver_values, arma::vec weights) {

    // Initialize saving space
    arma::cube statistics(edgelist.n_rows, riskset.n_rows, effects.n_elem);

    // Counters
    arma::uword sc = 0;
    arma::uword rc = 0;

    // Initialize indices
    arma::uvec indS = {0, 0, 0};
    arma::uvec indR = {0, 0, 0};

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
                //stat = actorStat(sender_values, 1, edgelist, riskset);
                indS = {0, 1, sc+2};
                stat = actorStat(sender_values.cols(indS), 1,
                    edgelist, riskset);
                sc += 1;
                break;
            // receiver_effect
            case 2 :
                //stat = actorStat(receiver_values, 2, edgelist, riskset);
                indR = {0, 1, rc+2};
                stat = actorStat(receiver_values.cols(indR), 2, 
                    edgelist, riskset);
                rc += 1;
                break;
            // inertia
            case 3 :
                stat = inertia(evls, riskset, weights);
                break;
            // inertia_weighted
            case 4:
                stat = inertia(evls, riskset, weights);
                break;
            // reciprocity
            case 5: 
                stat = reciprocity(edgelist, riskset);
                break;
            // indegree_sender
            case 7:
                stat = degree(edgelist, riskset, 1);
                break;
            // indegree_receiver
            case 8:
                stat = degree(edgelist, riskset, 2);
                break;
            // outdegree_sender
            case 9:
               stat = degree(edgelist, riskset, 3);
                break;
            // outdegree_receiver
            case 10:
                stat = degree(edgelist, riskset, 4);
                break;
            // totaldegree_sender
            case 11:
                stat = degree(edgelist, riskset, 5);
                break;
            // totaldegree_receiver
            case 12:
                stat = degree(edgelist, riskset, 6);
                break;
            // OTP
            case 17:
                stat = triad(actors, edgelist, riskset, 1);
                break;
            // ITP
            case 18:
                stat = triad(actors, edgelist, riskset, 2);
                break;
            // OSP
            case 19:
                stat = triad(actors, edgelist, riskset, 3);
                break;
            // ISP
            case 20:
                stat = triad(actors, edgelist, riskset, 4);
                break;
            // shared_partners
            case 21:
                stat = triadU(actors, edgelist, riskset, FALSE);
                break;
            // unique_sp
            case 22:
                stat = triadU(actors, edgelist, riskset, TRUE);
                break;
        }
            
        // Save statistic
        statistics.slice(i) = stat; 
    }

    // Output statistics object
    return statistics;
}