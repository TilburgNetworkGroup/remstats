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
#' @param weights optional; [vector], if supplied, should be of length edgelist #' and contain the weights for the events in the edgelist (to compute 
#' inertia_weighted)
#'
#' @return statistics [array], with three dimensions: timepoint x riskset x 
#' statistic. 
#' @return edgelist [matrix] with actor IDs that run from 1 to N and types that #' run from 1 to C. 
#' @return riskset [matrix], with actor IDs that run from 1 to N and types that
#' run from 1 to C. 
#' @return evls [matrix], edgelist transformed in the format that is required 
#' for estimation by relevent::rem() with in the first column the relational 
#' event ID and in the second column the time.
#' @return actors [vector], all unique actor IDs
#' 
#' @examples 
#' data(edgelistD)
#' effects <- c("inertia", "indegree_receiver", "outdegree_sender")
#' out <- remStats(edgelistD, effects)
#' fit <- relevent::rem(out$evls, out$statistics)
#' summary(fit)
#' 
#' @export

remStats <- function(edgelist, effects, directed = TRUE, type = FALSE, 
    timing = "interval", riskset = NULL, actors = NULL, weights = NULL) {

    # (1) Prepare the edgelist and riskset
    out <- prepER(edgelist, directed, type, riskset, actors)
    el <- out$edgelist
    rs <- out$riskset

    # (2) Prepare the actors
    if(is.null(actors)) {
        ac <- sort(unique(c(rs[,1], rs[,2])))
    } else {
        ac <- sort(actors[,1])
    }
	
	# (3) Prepare the evls (edgelist in relevent::rem() format)
    evls <- prepEvls(el, rs, type)

    # (4) Prepare the effects
    all_effects <- c("inertia", "reciprocity", "indegree_sender", 
        "indegree_receiver", "outdegree_sender", "outdegree_receiver", 
        "totaldegree_sender", "totaldegree_receiver", "recency_send", 
        "recency_receive", "rrank_send", "rrank_receive", "OTP", "ITP", "OSP", 
        "ISP", "shared_partners", "PSAB-BA", "PSAB_BY", "PSAB-XA", "PSAB-XB", 
        "PSAB-XY", "PSAB-AY", "inertia_weighted", "reciprocity_weighted", "unique_sp")
    eff <- match(effects, all_effects)

    # Add a baseline effect
    if(timing == "interval") {eff <- c(0, eff)}

    # Deal with event weights
    if(is.null(weights)) {weights <- rep(1, nrow(el))}
	
	# (5) Compute statistics
    stats <- remStatsC(eff, el, rs, evls, ac, weights)
    dimnames(stats)[[3]] <- c("baseline", effects)

    # (6) Return output
    list(statistics = stats, edgelist = el, riskset = rs, evls = evls, 
        actors = ac)
}