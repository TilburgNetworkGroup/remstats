#' prepER
#'
#' A helper function to prepare the edgelist and riskset for and within 
#' remstats().
#' 
#' @param edgelist [matrix] or [dataframe], should minimally contain the time, 
#' sender/actor 1 and receiver/actor 2 in the first three columns respectively. 
#' If the riskset contains typed relational events, the fourth column should 
#' contain the event type. 
#' @param directed [logical], are relational events in the riskset directional 
#' (directed = TRUE, default) or undirectional (directed = FALSE).
#' @param type [logical], do relational events in the riskset consider an 
#' action type (type = TRUE) or not (type = FALSE, default). 
#' @param riskset optional; [matrix] or [dataframe], should minimally contain 
#' sender/actor 1 and receiver/actor 2 in the first two columns, respectively. 
#' If it contains typed relational events, the third column should contain the 
#' event type. If a riskset is not supplied, it is assumed that all possible 
#' actors (and action types) are observed in the edgelist.
#' @param actors optional; [vector], if supplied, should contain all actors 
#' that can potentially interact. Used to create the riskset. 
#'
#' @return edgelist [matrix] with actor IDs that run from 1 to N and types that #' run from 1 to C. 
#' @return riskset [matrix] with actor IDs that run from 1 to N and types that
#' run from 1 to C. 
#' 
#' @examples 
#' data(edgelistD)
#' out <- prepER(edgelist = edgelistD, directed = TRUE, type = FALSE, 
#' 	riskset = NULL, actors = NULL)
#' el <- out$edgelist
#' rs <- out$riskset
#' 
#' @export

prepER <- function(edgelist, directed = TRUE, type = FALSE, riskset = NULL, 	
	actors = NULL) {

	# Obtain all actors from either ... 
	if(is.null(actors)) {
		if(is.null(riskset)) {
			# (1) .. the edgelist,
			if(class(edgelist[,2]) == "factor") {
				ac <- sort(unique(c(levels(edgelist[,2]), levels(edgelist[,3]))))
			} else {
				ac <- sort(unique(c(edgelist[,2], edgelist[,3])))
			}
		} else {
			# (2) ... the riskset,
			if(class(riskset[,1]) == "factor") {
				ac <- sort(unique(c(levels(riskset[,1]), levels(riskset[,2]))))
			} else {
				ac <- sort(unique(c(riskset[,1], riskset[,2])))
			}
		}
	} else {
		# (3) ... the supplied actors. 
		ac <- sort(actors)
	}

  	# Prepare the edgelist (let actor IDs run from 1 to N)
	edgelist[,2] <- match(edgelist[,2], ac)
	edgelist[,3] <- match(edgelist[,3], ac)
	
	# Sort the actors if the events are undirected
	if(!directed) {
		edgelist[,c(2,3)] <- t(apply(edgelist, 1, function(x) sort(x[c(2,3)])))
	}
	
	# If the relational events should consider event type, let the types in the 
	# edgelist run from 1 to # types.
	if(type) {
		if(is.null(riskset)) {
			if(class(edgelist[,4]) == "factor") {
				ty <- sort(levels(edgelist[,4]))
			} else {
				ty <- sort(unique(edgelist[,4]))
			}
		} else {
			if(class(riskset[,3]) == "factor") {
				ty <- sort(levels(riskset[,3]))
			} else {
				ty <- sort(unique(riskset[,3]))
			}
		}
		edgelist[,4] <- match(edgelist[,4], ty)
	}
	
	# Prepare the riskset 
	# If a riskset is not supplied, create a riskset
	if(is.null(riskset)) {        
		# Riskset for directed relational events
		if(directed == TRUE) {
			# All NxN pairs
			riskset <- as.matrix(expand.grid(seq_along(ac), seq_along(ac)))
			# Remove self-self events
			riskset <- riskset[riskset[,1] != riskset[,2],]
		}
		# Riskset for undirected relational events 
		if(directed == FALSE) {
			# All N over 2 combinations
			riskset <- as.matrix(t(combn(seq_along(ac), 2)))
		}
		# If the riskset should be typed, add the event types 
		if(type) {
			riskset <- cbind(riskset, NA)
			nOriginal <- nrow(riskset)
			for(i in 1:length(ty)) {
				if(i == 1) {
					riskset[,3] <- i
				} else {
					riskset <- rbind(riskset, cbind(riskset[1:nOriginal,c(1,2)], i))
				}
			}
		}
	} else {
		# If a riskset is supplied, let actor IDs run from 1 to N
		riskset[,1] <- match(riskset[,1], ac)
		riskset[,2] <- match(riskset[,2], ac)
		
		# Let type IDs run from 1 to # event types. 
		if(type) {
			riskset[,3] <- match(riskset[,3], ty)
		}
	}
	
	# Remove rownames
	rownames(edgelist) <- NULL

  # Output
  list(edgelist = as.matrix(edgelist), riskset = as.matrix(riskset))
}

