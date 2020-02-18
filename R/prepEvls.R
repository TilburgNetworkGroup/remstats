#' prepEvls
#'
#' A helper function that matches events in the edgelist with events in the 
#' riskset to create an evls, i.e., an edgelist in the format required by 
#' relevent::rem.
#' 
#' @param edgelist [matrix] or [dataframe], should minimally contain the time, 
#' sender/actor 1 and receiver/actor 2 in the first three columns respectively. 
#' If the riskset contains typed relational events, the fourth column should 
#' contain the event type. 
#' @param riskset [matrix] or [dataframe], should minimally contain sender/
#' actor 1 and receiver/actor 2 in the first two columns, respectively. If it 
#' contains typed relational events, the third column should contain the event 
#' type. 
#' @param type [logical], do relational events in the riskset consider an 
#' action type (type = TRUE) or not (type = FALSE, default). 
#'
#' @return evls [matrix], edgelist transformed in the format that is required 
#' for estimation by relevent::rem() with in the first column the relational 
#' event ID and in the second column the time.

prepEvls <- function(edgelist, riskset, type) {
    # Get the relational event IDs from the riskset
    if(!type) {
		evls <- apply(edgelist, 1, function(x) {
			which(x[2] == riskset[,1] & x[3] == riskset[,2])
		})
	} else {
		evls <- apply(edgelist, 1, function(x) {
			which(x[2] == riskset[,1] & x[3] == riskset[,2] & 
							x[4] == riskset[,3])
		})
	}

    # Bind the relational event IDs with the timing of the events
	evls <- cbind(evls, edgelist[,1])

    return(evls)
}