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
	
	if(!is.null(riskset)) {
		w <- FALSE

		# Check whether all events in the edgelist exist in the riskset
		if(!all(apply(edgelist, 1, function(x) {
			any(riskset[,1] == x[2] & riskset[,2] == x[3])
		}))) {
			warning("Not all events in the edgelist exist in the riskset: new riskset created.")
			w <- TRUE
		}

		if(with_type) {
			# Check whether the riskset contains event types
			if(ncol(riskset) < 3) {
				warning("The riskset does not contain event types: new riskset created.")
				w <- TRUE
			# Check whether all types in the edgelist exist in the riskset
			} else if (!all(edgelist[,4] %in% riskset[,3])) {
				warning("Not all event types in the edgelist exist in the riskset: new riskset created.")
				w <- TRUE
			}
		}

		if(w) {riskset <- NULL}
	}
	
	# Check whether an edgelist with event types is supplied
	if(with_type & (ncol(edgelist) < 4)) {
		stop("The fourth column in the edgelist should contain event types.")
	}

	# If directed is FALSE, make sure that events in the edgelist are sorted
	if(!directed) {
		edgelist[,c(2,3)] <- t(apply(edgelist[,c(2,3)], 1, sort))
	}
	
	# (1) Get all potential actors from ...
	ac1 <- edgelist[,2]
	ac2 <- edgelist[,3]
	if(class(ac1) == "factor") {ac1 <- levels(ac1)} 
	if(class(ac2) == "factor") {ac2 <- levels(ac2)}
	
	# ... option 1: the actors object
	if(!is.null(actors)) {
		# Check whether all actors in the edgelist are in the actors object
		if(!all(ac1 %in% actors) & !all(ac2 %in% actors)) {
			warning("Not all actors in the edgelist exist in the actors object: a riskset is created with the union of unique actors in the edgelist and actors object.")
			actors <- unique(sort(c(ac1, ac2, actors)))
		} else {
			# Make sure only unique actors are considered and that actors are 
			# sorted
			actors <- unique(sort(actors))
		}
	} else {
		# ... option 2: the riskset
		if(!is.null(riskset)) {
			ac1 <- riskset[,1]
			ac2 <- riskset[,2]
			if(class(ac1) == "factor") {ac1 <- levels(ac1)} 
			if(class(ac2) == "factor") {ac2 <- levels(ac2)}
		}
		# ... option 3: the edgelist
		actors <- unique(sort(c(ac1, ac2)))
	}

	# (2) Get all potential types from ... 
	if(with_type) {
		
		typ <- edgelist[,4]
		if(class(typ) == "factor") {typ <- levels(typ)}
		
		# ... option 1: the types object
		if(!is.null(types)) {
			# Check whether all event types in the edgelist are in types
			if(!all(typ %in% types)) {
				warning("Not all event types in the edgelist exist in the types object: a riskset is created with the union of unique event types in the edgelist and types object.")
				types <- unique(sort(c(typ, types)))
			} else {
				# Make sure only unique event types are considered and that 
				# event types are sorted
				types <- unique(sort(types))
			}
		} else {
			# ... option 2: the riskset 
			if(!is.null(riskset)) {
				typ <- riskset[,3]
				if(class(typ) == "factor") {typ <- levels(typ)}
			}
			# ... option 3: the edgelist
			types <- unique(sort(typ))			
		}
	} 
	
	# (3) Create the riskset for ...
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
		attributes(riskset)$directed <- directed
		attributes(riskset)$with_type <- with_type
	} 

	# Preserve riskset in its current form to output to the user
	riskset <- as.data.frame(riskset)
	risksetUser <- riskset
	
	# Transform the riskset for use within remstats()
	riskset[,1] <- match(riskset[,1], actors)  # Let actor IDs run from 1 to n
	riskset[,2] <- match(riskset[,2], actors)  # Let actor IDs run from 1 to n
	if(with_type) {riskset[,3] <- match(risksetUser[,3], types)}
	riskset <- as.matrix(riskset)
	riskset <- apply(riskset, 2, as.numeric)

	# (4) Prepare the edgelist
	# Edgelist to output to the user
	if(!with_type) {
		rs_position <- apply(edgelist, 1, function(x) {
			which(risksetUser[,1] == x[2] & risksetUser[,2] == x[3])
		})

		edgelist <- data.frame(
			time = edgelist[,1],
			sender = edgelist[,2],
			receiver = edgelist[,3],
			rs_position = rs_position
		)
	} else {
		rs_position <- apply(edgelist, 1, function(x) {
			which(risksetUser[,1] == x[2] & risksetUser[,2] == x[3] & 
			risksetUser[,3] == x[4])
		})

		edgelist <- data.frame(
			time = edgelist[,1],
			sender = edgelist[,2],
			receiver = edgelist[,3],
			type = edgelist[,4],
			rs_position = rs_position
		)
	}

	if(!directed) {
		colnames(edgelist)[c(2,3)] <- c("actor1", "actor2")
	}

	rownames(edgelist) <- NULL
	attributes(edgelist)$directed <- directed
	attributes(edgelist)$with_type <- with_type

	# Preserve edgelist in its current form to output to the user
	edgelistUser <- edgelist

	# Transform the edgelist for use within remstats()
	edgelist[,2] <- match(edgelist[,2], actors)  # Let actor IDs run from 1 to n
	edgelist[,3] <- match(edgelist[,3], actors)  # Let actor IDs run from 1 to n
	if(with_type) {edgelist[,4] <- match(edgelist[,4], types)}
	edgelist <- as.matrix(edgelist)
	edgelist <- apply(edgelist, 2, as.numeric)

	# (5) Prepare the evls
	evls <- cbind(edgelistUser$rs_position, edgelistUser$time)
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

