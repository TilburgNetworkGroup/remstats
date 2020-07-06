# prepEdgelist
#
# A helper function to prepare the edgelist, riskset and evls to be used 
# within remstats() (easy format for the computations in cpp). The function 
# also outputs a user-friendly format of the edgelist, riskset and evls to be 
# outputted with remstats().
# 
# @param edgelist [matrix] or [dataframe], should minimally contain the time, 
# sender/actor 1 and receiver/actor 2 in the first three columns respectively. 
# If the riskset contains typed relational events, the fourth column should 
# contain the event type. 
# @param directed [logical], are relational events in the riskset directional 
# (directed = TRUE, default) or undirectional (directed = FALSE).
# @param with_type [logical], do relational events in the riskset consider an 
# action type (type = TRUE) or not (type = FALSE, default). 
# @param riskset optional; [matrix] or [dataframe], should minimally contain 
# sender/actor 1 and receiver/actor 2 in the first two columns, respectively. 
# If it contains typed relational events, the third column should contain the 
# event type. If a riskset is not supplied, it is assumed that all possible 
# actors (and action types) are observed in the edgelist.
# @param actors optional; [vector], if supplied, should contain all actors 
# that can potentially interact. Used to create the riskset. 
# @param types optional; [vector], if supplied, should contain all event types.
#
# @return edgelist [matrix] with actor IDs that run from 1 to N and types that #' run from 1 to C. 
# @return edgelistUser [dataframe] user-friendly edgelist. 
# @return riskset [matrix] with actor IDs that run from 1 to N and types that
# run from 1 to C. 
# @return risksetUser [dataframe] user-friendly riskset. 
# @return evls [matrix]. 

prepEdgelist <- function(edgelist, directed = TRUE, with_type = FALSE, 
	riskset = NULL, actors = NULL, types = NULL) {

	# Check the edgelist for missing values
	if(!with_type) {
		if(anyNA(edgelist[,1:3])) {
			warning("The `edgelist` object contains missing data: incomplete events are dropped.")
			mv <- which(is.na(edgelist[,1:3]), arr.ind = T)[,1]
			edgelist <- edgelist[-mv,]
		}
	} else {
		if(anyNA(edgelist[,1:4])) {
			warning("The `edgelist` object contains missing data: incomplete events are dropped.")
			mv <- which(is.na(edgelist[,1:4]), arr.ind = T)[,1]
			edgelist <- edgelist[-mv,]
		}
	}		

	# Preserve the user-inputted edgelist
	edgelistUser <- edgelist

	# Make sure the time column is of class numeric
	edgelist[,1] <- tryCatch(as.numeric(edgelist[,1]), 
		warning = function(w) stop("Make sure the first column of `edgelist` contains numeric values referring to event times."))

	# Check whether the time column is time-ordered
	if(any(diff(edgelist[,1]) < 0)) {
		warning("Suspect that events are not well-ordered in time: may cause unexpected behavior.")
	}

	# Get all potential actors from ...
	# ... option 1: the actors object
	if(!is.null(actors)) {
		# Make sure only unique actors are considered and that actors are 
		# sorted
		actors <- unique(sort(actors))
	} else {
		# ... option 2: the riskset
		if(!is.null(riskset)) {
			ac1 <- riskset[,1]
			ac2 <- riskset[,2]
		} else {
			# ... option 3: the edgelist
			ac1 <- edgelist[,2]
			ac2 <- edgelist[,3]
		}

		if(class(ac1) == "factor") {ac1 <- levels(ac1)} 
		if(class(ac2) == "factor") {ac2 <- levels(ac2)}
		actors <- unique(sort(c(ac1, ac2)))
	}

	# Replace actor IDs in the edgelist with integers
	edgelist[,2] <- match(edgelist[,2], actors)
	edgelist[,3] <- match(edgelist[,3], actors)

	# Check whether all actors are matched
	if(anyNA(edgelist[,2])) {
		stop("Not all actors in the `edgelist` occur in the supplied `riskset` or `actors` object.")
	}
	if(anyNA(edgelist[,3])) {
		stop("Not all actors in the `edgelist` occur in the supplied `riskset` or `actors` object.")
	}

	# If directed = FALSE, make sure that for each event actors are well-ordered
	if(!directed) {
		edgelist[,c(2,3)] <- t(apply(edgelist[,c(2,3)], 1, sort))
	}

	if(with_type) {
		# Get all potential types from ...
		# ... option 1: the types object
		if(!is.null(types)) {
			# Make sure only unique event types are considered and that 
			# event types are sorted
			types <- unique(sort(types))
		} else {
			# ... option 2: the riskset 
			if(!is.null(riskset)) {
				typ <- riskset[,3]
			} else {
				# ... option 3: the edgelist
				if(ncol(edgelist) < 4) {
					stop("Make sure the fourth column of the `edgelist` object contains the event types.")
				}
				typ <- edgelist[,4]
			}

			if(class(typ) == "factor") {typ <- levels(typ)}
			types <- unique(sort(typ))			
		}

		# Replace type IDs in the edgelist with integers
		edgelist[,4] <- match(edgelist[,4], types)

		# Check whether all types are matched
		if(anyNA(edgelist[,4])) {
			stop("Not all event types in the `edgelist` occur in the `riskset` or `types` object.")
		}
	}

	# Create a riskset, if one is not supplied, for ...
	if(is.null(riskset)) {
		# ... option 1: directed relational events
		if(directed) {
			riskset <- expand.grid(actors, actors)
			colnames(riskset) <- c("sender", "receiver")
			riskset <- riskset[riskset$sender!=riskset$receiver,]
		} 
		# .... option 2: undirected relational events
		if(!directed) {
			riskset <- t(utils::combn(actors, 2))
			colnames(riskset) <- c("actor1", "actor2")
			riskset <- t(apply(riskset, 1, sort))
		}
			
		# Add event types if requested
		if(with_type) {
			riskset <- lapply(types, function(x) {
				cbind(riskset, x)
			})
			riskset <- do.call(rbind, riskset)
			colnames(riskset)[3] <- "event_type"
		}

		rownames(riskset) <- NULL
	} 

	# Preserve the riskset in this format for the user
	risksetUser <- riskset

	# Replace actor IDs in the riskset with integers
	riskset[,1] <- match(riskset[,1], actors)
	riskset[,2] <- match(riskset[,2], actors)

	# Check whether all actors are matched
	if(anyNA(riskset[,1])) {
		stop("Not all actors in the supplied `riskset` occur in the supplied `actors` object.")
	}
	if(anyNA(riskset[,2])) {
		stop("Not all actors in the supplied `riskset` occur in the supplied `actors` object.")
	}

	if(with_type) {
		# Replace type IDs in the riskset with integers
		riskset[,3] <- match(riskset[,3], types)

		# Check whether all actors are matched
		if(anyNA(riskset[,3])) {
			stop("Not all event types in the supplied `riskset` occur in the supplied `types` object.")
		}
	}

	riskset <- as.matrix(riskset)
	riskset <- apply(riskset, 2, as.numeric)

	# Match the riskset position of events in the edgelist
	if(!with_type) {
		rs_position <- apply(edgelist, 1, function(x) {
			which(riskset[,1] == as.numeric(x[2]) & 
				riskset[,2] == as.numeric(x[3]))
		})
	} else {
		rs_position <- apply(edgelist, 1, function(x) {
			which(riskset[,1] == as.numeric(x[2]) & 
				riskset[,2] == as.numeric(x[3]) & 
				riskset[,3] == as.numeric(x[4]))
		})
	}

	if(length(unlist(rs_position)) < nrow(edgelist)) {
		stop("Not all events in the `riskset` object occur in the `edgelist`.")
	} else {
		if(!with_type) {
			edgelist <- cbind(edgelist[,1:3], rs_position)	
		} else {
			edgelist <- cbind(edgelist[,1:4], rs_position)
		}
		edgelistUser <- cbind(edgelistUser, rs_position)
	}

	edgelist <- as.matrix(edgelist)
	edgelist <- apply(edgelist, 2, as.numeric)
	rownames(edgelist) <- NULL

	# Prepare the evls
	evls <- cbind(rs_position, edgelist[,1])
	colnames(evls) <- c("rs_position", "time")
	evls <- as.matrix(evls)

	# Prepare actors output
	actorsUser <- actors
	actors <- seq_along(actors)
	
	# Output
	list(
		actors = actors,
		actorsUser = actorsUser,
		edgelist = edgelist,
		edgelistUser = edgelistUser,
		riskset = riskset,
		risksetUser = risksetUser,
		evls = evls
	)	
}

