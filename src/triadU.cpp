#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//' triadU
//'
//' A function to compute the shared partners effect for undirected relational 
//' events.
//'
//' @param actors vector with numeric actor IDs (correspod to edgelist, riskset)
//' @param edgelist 3-column edgelist (time, sender, receiver)
//' @param riskset 2-column riskset (sender/actor 1, receiver/actor 2)
//'
//' @return matrix (time x dyad)
//' 
//' @examples
//' data(edgelistU)
//' out <- prepER(edgelist = edgelistU, directed = FALSE, type = FALSE, 
//'     riskset = NULL, actors = NULL)
//' el <- out$edgelist
//' rs <- out$riskset
//' ac <- sort(unique(c(rs[,1], rs[,2])))
//' otp <- triadU(ac, el, rs)
//'
//' @export
//'
//[[Rcpp::export]]
arma::mat triadU(arma::vec actors, arma::mat edgelist, arma::mat riskset) {

    //Storage space
    //(1) Adjacency matrix 
    arma::mat adj(max(actors), max(actors), fill::zeros);
    //(2) Statistic matrix (output))
    arma::mat stat(edgelist.n_rows, riskset.n_rows, fill::zeros);

    //For loop over the sequence
    for(arma::uword i = 1; i < edgelist.n_rows; ++i) {
        //Actors of the previous event
        arma::uword actor1 = edgelist(i-1, 1);
        arma::uword actor2 = edgelist(i-1, 2);
        
        //Update the adjacency matrix
        adj(actor1 - 1, actor2 - 1) += 1;

        //Actors that have a relation with the actors involved in the previous 
        //event
        arma::uvec relActors1 = find(adj.row(actor1-1) != 0);
        arma::uvec relActors2 = find(adj.col(actor2-1) != 0);
        arma::uvec relActors = join_cols(relActors1, relActors2);
        relActors = sort(unique(relActors));

        //Copy the previous row
        arma::rowvec thisrow = stat.row(i-1);

        //For loop over dyads
        for(arma::uword d = 0; d < riskset.n_rows; ++d) {
            //Actors in the dyad
            arma::uword actor1D = riskset(d, 0);
            arma::uword actor2D = riskset(d, 1);

            //Only change their statistic if both actors have a relation to the 
            //actors involved in the previous event
            if(any(relActors == (actor1D - 1)) &&
                any(relActors == (actor2D - 1))) {

                    //Communications actor1D
                    arma::rowvec com1Actor1Dtemp = adj.row(actor1D-1);
                    arma::colvec com1Actor1D = conv_to<colvec>::from(com1Actor1Dtemp);
                    arma::colvec com2Actor1D = adj.col(actor1D-1);
                    arma::vec comActor1D = com1Actor1D + com2Actor1D;
                    //Communications actor2D
                    arma::rowvec com1Actor2Dtemp = adj.row(actor2D-1);
                    arma::colvec com1Actor2D = conv_to<colvec>::from(com1Actor2Dtemp);
                    arma::colvec com2Actor2D = adj.col(actor2D-1);
                    arma::vec comActor2D = com1Actor2D + com2Actor2D;

                    //Saving space
                    arma::uword dyadstat = 0;

                    //For loop over actors 
                    for(arma::uword h = 0; h < max(actors); ++h) {
                        if((h != (actor1D-1)) && (h != (actor2D-1))) {
                            arma::vec thisactor = {comActor1D(h), comActor2D(h)};
                            dyadstat += min(thisactor);        
                        }
                    }

                    //Save the statistic for this dyad
                    thisrow(d) = dyadstat;
                }
        }

        //Save the statistic for this timepoint
        stat.row(i) = thisrow;
    }
    //Output
    return(stat);
}