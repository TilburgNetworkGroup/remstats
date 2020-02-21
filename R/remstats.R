#' remStats
#'
#' A function to compute statistics for a relational event sequence. 
#' 
#' @param edgelist [matrix] or [dataframe], should minimally contain the time, 
#' sender/actor 1 and receiver/actor 2 in the first three columns respectively. 
#' If the riskset contains typed relational events, the fourth column should 
#' contain the event type. 
#' @param effects [character vector], indicates the effects that are requested.
#' @param directed [logical], are relational events in the riskset directional 
#' (directed = TRUE, default) or undirectional (directed = FALSE).
#' @param type [logical], do relational events in the riskset consider an 
#' action type (type = TRUE) or not (type = FALSE, default). 
#'
#' @param riskset optional; [matrix] or [dataframe], should minimally contain 
#' sender/actor 1 and receiver/actor 2 in the first two columns, respectively. 
#' If it contains typed relational events, the third column should contain the 
#' event type. If a riskset is not supplied, it is assumed that all possible 
#' actors (and action types) are observed in the edgelist.
#'
#' @return statistics [array], with three dimensions: timepoint x riskset x 
#' statistic. 
#' @return evls [matrix], edgelist transformed in the format that is required 
#' for estimation by relevent::rem() with in the first column the relational 
#' event ID and in the second column the time.
#' 
#' @export

remStats <- function(edgelist, effects, riskset = NULL, directed = TRUE, 
                     type = FALSE) {

    # (1) Prepare the edgelist and riskset
    out <- prepER(edgelist, riskset, directed, type)
    el <- out$edgelist
    rs <- out$riskset
	
	# (2) Prepare the evls (edgelist in relevent::rem() format)
    evls <- prepEvls(el, rs, type)

	
	
	# ... Compute and return statistics
    

}