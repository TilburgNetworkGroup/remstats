

#' Stack remstats for model fitting
#'
#' Stack a \code{remstats} object (\code{tomstats}; \code{aomstats}) into long format for
#' model fitting. Each row corresponds to one dyad in the risk set at one
#' event time, with a response indicating the number of observed events for
#' that dyad at that time and an offset given by the log inter-event time.
#'
#' @param stats A \code{remstats} or \code{tomstats} object containing
#'   statistics for the (active) risk set.
#' @param reh A \code{remify} object containing event ordering, risk sets,
#'   and inter-event times.
#'
#' @return A list with elements:
#' \describe{
#'   \item{remstats_stack}{A data frame in stacked (long) format containing the
#'     response \code{obs}, the statistics, and the offset \code{log_intevent}.}
#'   \item{subset}{Integer vector of length two giving the first and last
#'     event indices used for computing \code{stats}.}
#'   \item{D}{}{Number of included dyads.}
#'   \item{E}{}{Number of included events}
#' }
#'
#' @rdname stack_stats
#' @export
#'
stack_stats <- function(stats,reh) {
  UseMethod("stack_stats")
}

#' @export
#' @method stack_stats tomstats
stack_stats.tomstats <- function(stats,reh) {

  # extract the total number of active dyads
  D_remstats <- dim(stats)[2]
  # extract total number of events in tomstats object
  E <- dim(stats)[1]
  # extract first and last event used for computing tomstats
  subset_idx <- unlist(attr(stats,"subset"))

  # Construct the statistics stack for each event
  statStack <- do.call(rbind, lapply(1:E, function(e) {
  	cbind(e,stats[e, , ])
  }))
  stat_glm <- as.data.frame(statStack)
  colnames(stat_glm)[1] <- "event"
  rm(statStack)
  
  # Process time data and create log time differences for the offset
  if(reh$meta$ordinal == FALSE){
	  log_intevent <- log(reh$intereventTime)
	  stat_glm$log_intevent <- rep(log_intevent[subset_idx[1]:subset_idx[2]], each = D_remstats)
  }#else{
  #	log_intevent <- NULL
  #}

  # Create the binary response variable
  stat_glm$obs <- unlist(lapply(subset_idx[1]:subset_idx[2], function(e) {
  	if(reh$meta$riskset == "active"){ #let's also store the manual riskset in 'reh$index$dyadIDactive'
	    obs_dyads <- reh$index$dyadIDactive[[e]]
  	  tabulate(obs_dyads, nbins = D_remstats)
  	}else if(reh$meta$riskset == "full"){
  		obs_dyads <- reh$index$dyadID[[e]]
  		tabulate(obs_dyads, nbins = D_remstats)
  	}
  }))

  # Include dyad index for each row
  stat_glm$dyad <- rep(1:D_remstats, subset_idx[2]-subset_idx[1]+1)

  return(list(remstats_stack = stat_glm, subset=subset_idx, D = D_remstats, E = E))
}



#' @export
#' @method stack_stats tomstats
stack_stats.tomstats <- function(stats,reh) {
	
	# extract the total number of active dyads
	D_remstats <- dim(stats)[2]
	# extract total number of events in tomstats object
	E <- dim(stats)[1]
	# extract first and last event used for computing tomstats
	subset_idx <- unlist(attr(stats,"subset"))
	
	# Construct the statistics stack for each event
	statStack <- do.call(rbind, lapply(1:E, function(e) {
		cbind(e,stats[e, , ])
	}))
	stat_glm <- as.data.frame(statStack)
	colnames(stat_glm)[1] <- "event"
	rm(statStack)
	
	# Process time data and create log time differences for the offset
	if(reh$meta$ordinal == FALSE){
		log_intevent <- log(reh$intereventTime)
		stat_glm$log_intevent <- rep(log_intevent[subset_idx[1]:subset_idx[2]], each = D_remstats)
	}#else{
	#	log_intevent <- NULL
	#}
	
	# Create the binary response variable
	stat_glm$obs <- unlist(lapply(subset_idx[1]:subset_idx[2], function(e) {
		if(reh$meta$riskset == "active"){ #let's also store the manual riskset in 'reh$index$dyadIDactive'
			obs_dyads <- reh$index$dyadIDactive[[e]]
			tabulate(obs_dyads, nbins = D_remstats)
		}else if(reh$meta$riskset == "full"){
			obs_dyads <- reh$index$dyadID[[e]]
			tabulate(obs_dyads, nbins = D_remstats)
		}
	}))
	
	# Include dyad index for each row
	stat_glm$dyad <- rep(1:D_remstats, subset_idx[2]-subset_idx[1]+1)
	
	return(list(remstats_stack = stat_glm, subset=subset_idx, D = D_remstats, E = E))
}



