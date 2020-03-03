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
#'
#' @return statistics [array], with three dimensions: timepoint x riskset x 
#' statistic. 
#' @return evls [matrix], edgelist transformed in the format that is required 
#' for estimation by relevent::rem() with in the first column the relational 
#' event ID and in the second column the time.
#' 
#' @export

remStats <- function(edgelist, effects, directed = TRUE, type = FALSE, 
    timing = "interval", riskset = NULL, actors = NULL) {

    # (1) Prepare the edgelist and riskset
    out <- prepER(edgelist, directed, type, riskset, actors)
    el <- out$edgelist
    rs <- out$riskset
	
	# (2) Prepare the evls (edgelist in relevent::rem() format)
    evls <- prepEvls(el, rs, type)

    # (3) Prepare the effects
    all_effects <- c("inertia")
    eff <- match(effects, all_effects)

    # Add a baseline effect
    if(timing == "interval") {eff <- c(0, eff)}
	
	# ... Compute and return statistics
    stats <- remStatsC()
    

}