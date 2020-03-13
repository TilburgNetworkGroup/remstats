#' remstatsMW
#'
#' A function to compute statistics for a relational event sequence when 
#' fitting with a moving window REM. Endogenous statistics are computed based 
#' on past events that may fall without the window.  
#' 
#' @param full_edgelist [matrix] or [dataframe], should minimally contain the 
#' time, sender/actor 1 and receiver/actor 2 in the first three columns 
#' respectively. If the riskset contains typed relational events, the fourth 
#' column should contain the event type. 
#' @param window_edgelist [matrix] or [dataframe]. See full_edgelist, with only 
#' the events that fall within the window for which a REM is estimated. 
#' @param effects [character vector], indicates the effects that are requested.
#' @param window_length [numeric value], indicates the length of the window in 
#' time units. 
#' @param directed [logical], are relational events in the riskset directional 
#' (directed = TRUE, default) or undirectional (directed = FALSE).
#' @param type [logical], do relational events in the riskset consider an 
#' action type (type = TRUE) or not (type = FALSE, default). 
#' @param timing [character value], indicates whether the full likelihood 
#' (timing = "interval", default) or ordinal likelihod (timing = "ordinal") 
#' will be used for estimation. If interval timing, a baseline statistic is 
#' added to the statistic array.
#' @param standardize [logical], indicates whether endogenous effects should be 
#' standardized (default = FALSE)
#'
#' @param riskset optional; [matrix] or [dataframe], should minimally contain 
#' sender/actor 1 and receiver/actor 2 in the first two columns, respectively. 
#' If it contains typed relational events, the third column should contain the 
#' event type. If a riskset is not supplied, it is assumed that all possible 
#' actors (and action types) are observed in the edgelist.
#' @param actors optional; [vector], if supplied, should contain all actors 
#' that can potentially interact. Used to create the riskset. 
#' @param covariates optional; [List] with the covariate values for when a 
#' sender_effect, receiver_effect, same, difference, mean, min, max, or 
#' both_equal_to effect are requested. Covariate values should be supplied to 
#' an element with the name equal to the requested effect. Covariate values 
#' should be supplied in a matrix with in the first column the actor IDs, the 
#' second column the time at which covariate values change (can be set to zero 
#' for time-invariant covariates) and in subsequent columns the unique 
#' covariate variables for which the respective effect is requested. 
#' @param event_effect optional; matrix with the values for when an 
#' event_effect is requested: one column of length edgelist per event_effect. 
#' @param full_weights optional; [vector], if supplied, should be of length 
#' edgelist and contain the weights for the events in the edgelist (to compute 
#' inertia_weighted)
#' @param equal_val optional; [vector]. Required if the "both_equal_to" effect 
#' is requested. Denotes the value(s) to which both covariate values of the 
#' actors in a dyad should be equal. 
#'
#' @return statistics [array], with three dimensions: timepoint x riskset x 
#' statistic. 
#' @return edgelist [matrix] with actor IDs that run from 1 to N and types that 
#' run from 1 to C. 
#' @return riskset [matrix], with actor IDs that run from 1 to N and types that
#' run from 1 to C. 
#' @return evls [matrix], edgelist transformed in the format that is required 
#' for estimation by relevent::rem() with in the first column the relational 
#' event ID and in the second column the time.
#' @return actors [vector], all unique actor IDs
#' 
#' @examples 
#' data(edgelistD)
#' data(covar)
#' windows <- data.frame(start = seq(0, 900, 75), end = seq(100, 1000, 75))
#' window_edgelist <- edgelistD[edgelistD$time > windows$start[2] &
#'      edgelistD$time <= windows$end[2],]
#' effects <- c("difference", "both_equal_to", "inertia", "indegree_receiver", 
#'  "outdegree_sender")
#' covariates <- list(difference = covar, both_equal_to = covar[,c(1:2, 4)])
#' out <- remstatsMW(full_edgelist = edgelistD, window_edgelist = 
#'      window_edgelist, effects = effects, window_length = 100, 
#'      covariates = covariates, equal_val = 0)
#' 
#' @export

remstatsMW <- function(full_edgelist, window_edgelist, effects, window_length, 
    directed = TRUE, type = FALSE, timing = "interval", standardize = FALSE, 
    riskset = NULL, actors = NULL, covariates = NULL, event_effect = NULL, 
    full_weights = NULL, equal_val = NULL) {

    # Prepare the edgelist, riskset and actors
    out <- prepER(full_edgelist, directed, type, riskset, actors)
    full_el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors

    out <- prepER(window_edgelist, directed, type, riskset = rs, 
        actors = ac[,2])
    window_el <- out$edgelist

 	# Prepare the evls (edgelist in relevent::rem() format)
    full_evls <- prepEvls(full_el, rs, type)
    window_evls <- prepEvls(window_el, rs, type)

    # Prepare the effects
    all_effects <- c("sender_effect", "receiver_effect", "same", "difference",  
        "mean", "min", "max", "both_equal_to", "event_effect", "type_effect", "inertia", "inertia_weighted", "inertia_type", "inertia_type_weighted", "reciprocity", "reciprocity_weighted", "indegree_sender", "indegree_receiver", "outdegree_sender", "outdegree_receiver", "totaldegree_sender", "totaldegree_receiver", "recency_send", "recency_receive", "rrank_send", "rrank_receive", "OTP", "ITP", "OSP", "ISP", "shared_partners", "unique_sp", "shared_partners_type", "unique_sp_type", "PSAB-BA", "PSAB_BY", "PSAB-XA", "PSAB-XB",  "PSAB-XY", "PSAB-AY")
    eff <- match(effects[!grepl("\\*", effects)], all_effects)

    # Add a baseline effect
    if(timing == "interval") {eff <- c(0, eff)}

    # Prepare exogenous effects
    # If requested
    # Sender_effect
    if(any(eff==1)) {
        eff <- append(eff[-which(eff==1)], 
            rep(1, ncol(covariates$sender_effect)-2), which(eff==1)-1)
        covariates$sender_effect$id <- ac$id[
            match(covariates$sender_effect$id, ac$name)]
        covariates$sender_effect <- as.matrix(covariates$sender_effect)
    }
    # Receiver_effect
    if(any(eff==2)) {
        eff <- append(eff[-which(eff==2)], 
            rep(2, ncol(covariates$receiver_effect)-2), which(eff==2)-1)
        covariates$receiver_effect$id <- ac$id[
            match(covariates$receiver_effect$id, ac$name)]
        covariates$receiver_effect <- as.matrix(covariates$receiver_effect)
    }
    # Same
    if(any(eff==3)) {
        eff <- append(eff[-which(eff==3)], rep(3, ncol(covariates$same)-2), 
            which(eff==3)-1)
        covariates$same$id <- ac$id[match(covariates$same$id, ac$name)]
        covariates$same <- as.matrix(covariates$same)
    }
    # Difference
    if(any(eff==4)) {
        eff <- append(eff[-which(eff==4)], 
            rep(4, ncol(covariates$difference)-2), which(eff==4)-1)
        covariates$difference$id <- ac$id[
            match(covariates$difference$id, ac$name)]
        covariates$difference <- as.matrix(covariates$difference)
    }
    # Mean
    if(any(eff==5)) {
        eff <- append(eff[-which(eff==5)], rep(5, ncol(covariates$mean)-2), 
            which(eff==5)-1)
        covariates$mean$id <- ac$id[match(covariates$mean$id, ac$name)]
        covariates$mean <- as.matrix(covariates$mean)
    }
    # Min
    if(any(eff==6)) {
        eff <- append(eff[-which(eff==6)], rep(6, ncol(covariates$min)-2), 
            which(eff==6)-1)
        covariates$min$id <- ac$id[match(covariates$min$id, ac$name)]
        covariates$min <- as.matrix(covariates$min)
    }
    # Max
    if(any(eff==7)) {
        eff <- append(eff[-which(eff==7)], rep(7, ncol(covariates$max)-2), 
            which(eff==7)-1)
        covariates$max$id <- ac$id[match(covariates$max$id, ac$name)]
        covariates$max <- as.matrix(covariates$max)
    }
    # Both_equal_to
    if(any(eff==8)) {
        eff <- append(eff[-which(eff==8)], 
            rep(8, ncol(covariates$both_equal_to)-2), which(eff==8)-1)
        covariates$both_equal_to$id <- ac$id[
            match(covariates$both_equal_to$id, ac$name)]
        covariates$both_equal_to <- as.matrix(covariates$both_equal_to)
    }

    # If not requested
    if(!(any(eff==1))) {covariates$sender_effect <- matrix(0, 1, 1)}
    if(!(any(eff==2))) {covariates$receiver_effect <- matrix(0, 1, 1)}
    if(!(any(eff==3))) {covariates$same <- matrix(0, 1, 1)}
    if(!(any(eff==4))) {covariates$difference <- matrix(0, 1, 1)}
    if(!(any(eff==5))) {covariates$mean <- matrix(0, 1, 1)}
    if(!(any(eff==6))) {covariates$min <- matrix(0, 1, 1)}
    if(!(any(eff==7))) {covariates$max <- matrix(0, 1, 1)}
    if(!(any(eff==8))) {covariates$both_equal_to <- matrix(0, 1, 1)}

    # Order exogenous effects
    covar <- list(covariates$sender_effect, covariates$receiver_effect, 
        covariates$same, covariates$difference, covariates$mean, 
        covariates$min, covariates$max, covariates$both_equal_to)

    # Prepare event effects
    # If requested
    if(any(eff==9)) {
        event_effect <- as.matrix(event_effect)
        eff <- append(eff[-which(eff==9)], 
            rep(9, ncol(event_effect)), which(eff==9)-1)
    } else {
        event_effect <- matrix(0, 1, 1)
    }

    # Prepare type effects
    if(any(eff==10)) {
        types <- sort(unique(c(rs[,3])))
        types <- types[-1]
        eff <- append(eff[-which(eff==10)], 
            rep(10, length(types)), which(eff==10)-1)
    } else {
        types <- 0
    }

    # Prepare interaction effects
    if(any(grepl("\\*", effects))) {
        # Which are the interaction effects?
        int_effects <- effects[grepl("\\*", effects)]
        int_effects <- sapply(int_effects, function(x) {
            strsplit(x, split = "\\*")
        })
        int_effects <- matrix(unlist(int_effects), byrow = T, ncol = 2)
        
        # Get positions interaction effects
        findpos <- function(value, effects) {
            if(grepl("[0-9]", value)) {
                temp <- strsplit(value, split = "[0-9]")[[1]]	
                pos <- which(effects == temp)[1]
                temp2 <- strsplit(value, split = "[a-z]")[[1]]
                correction <- suppressWarnings(as.numeric(temp2)[!is.na(as.numeric(temp2))])
                pos + correction-1
            } else {
                which(effects == value)
            }
        }
	
 	    temp_effects <- c("baseline", all_effects[eff])
        int_positions <- t(apply(int_effects, 1, function(x) {
            cbind(findpos(x[1], temp_effects), findpos(x[2], temp_effects))
        }))
        int_positions <- int_positions-1
	    # Case 999 in remstatsC refers to interaction effects
	    eff <- c(eff, rep(999, nrow(int_effects)))
    } else {
        int_positions <- matrix(0, 1, 1)
    }

    # Deal with event weights if not requested
    if(is.null(full_weights)) {full_weights <- rep(1, nrow(full_el))}

    # Deal with equal_val if not requested
    if(is.null(equal_val)) {equal_val <- 0}
	
	# (4) Compute statistics
    stats <- remstatsMWCpp(effects = eff, standardize = standardize,  
        full_edgelist = full_el, window_edgelist = window_el, 
        window_length = window_length, riskset = rs, actors = ac[,1], 
        covariates = covar, event_effect = event_effect, types = types, 
        full_weights = full_weights, equal_val = equal_val, int_positions = 
        int_positions)

    dimnames(stats)[[3]] <- c("baseline", all_effects[eff[!eff==999]], 
        effects[grepl("\\*", effects)])

    if(any(dimnames(stats)[[3]] == "type_effect")) {
    	for(i in 1:length(types)) {
    		dimnames(stats)[[3]][which(dimnames(stats)[[3]] == "type_effect")[1]] <- 
    			paste(dimnames(stats)[[3]][which(dimnames(stats)[[3]] == "type_effect")[1]], "_",
    					types[i], sep = "")
    	}
    }

    # (5) Return output
    list(statistics = stats, full_edgelist = full_el, 
        window_edgelist = window_el, riskset = rs, full_evls = full_evls, 
        window_evls = window_evls, actors = ac)
}