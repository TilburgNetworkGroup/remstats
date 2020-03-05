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
#' @param timing [character value], indicates whether the full likelihood 
#' (timing = "interval", default) or ordinal likelihod (timing = "ordinal") 
#' will be used for estimation. If interval timing, a baseline statistic is 
#' added to the statistic array.
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
#' @param weights optional; [vector], if supplied, should be of length edgelist 
#' and contain the weights for the events in the edgelist (to compute 
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
#' effects <- c("difference", "both_equal_to", "inertia", "indegree_receiver", 
#'  "outdegree_sender")
#' covariates <- list(difference = covar, both_equal_to = covar[,c(1:2, 4)])
#' out <- remStats(edgelistD, effects, covariates = covariates, equal_val = 0)
#' fit <- relevent::rem(out$evls, out$statistics)
#' summary(fit)
#' 
#' @export

remStats <- function(edgelist, effects, directed = TRUE, type = FALSE, 
    timing = "interval", riskset = NULL, actors = NULL, covariates = NULL, 
    weights = NULL, equal_val = NULL) {

    # Prepare the edgelist, riskset and actors
    out <- prepER(edgelist, directed, type, riskset, actors)
    el <- out$edgelist
    rs <- out$riskset
    ac <- out$actors

 	# Prepare the evls (edgelist in relevent::rem() format)
    evls <- prepEvls(el, rs, type)

    # Prepare the effects
    all_effects <- c("sender_effect", "receiver_effect", "same", "difference", 
        "mean", "min", "max", "both_equal_to", "inertia", "inertia_weighted", 
        "reciprocity", "reciprocity_weighted","indegree_sender",
        "indegree_receiver", "outdegree_sender", "outdegree_receiver", 
        "totaldegree_sender", "totaldegree_receiver", "recency_send", 
        "recency_receive", "rrank_send", "rrank_receive", "OTP", "ITP", "OSP", 
        "ISP", "shared_partners", "unique_sp", "PSAB-BA", "PSAB_BY", "PSAB-XA", 
        "PSAB-XB",  "PSAB-XY", "PSAB-AY")
    eff <- match(effects, all_effects)

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

    # Deal with event weights if not requested
    if(is.null(weights)) {weights <- rep(1, nrow(el))}

    # Deal with equal_val if not requested
    if(is.null(equal_val)) {equal_val <- 0}
	
	# (4) Compute statistics
    stats <- remStatsC(effects = eff, edgelist = el, riskset = rs, evls = evls, 
        actors = ac[,1], covariates = covar, weights = weights, equal_val = 
        equal_val)

    dimnames(stats)[[3]] <- c("baseline", all_effects[eff])

    # (5) Return output
    list(statistics = stats, edgelist = el, riskset = rs, evls = evls, 
        actors = ac)
}