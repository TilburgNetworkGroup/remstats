#' tomstats
#'
#' Computes statistics for modeling relational event history data
#' with the tie-oriented relational event model.
#'
#' @param effects an object of class \code{"\link[stats]{formula}"} (or one
#' that can be coerced to that class): a symbolic description of the effects in
#' the model for which statistics are computed, see 'Details' for the available
#' effects and their corresponding statistics
#' @param adjmat optionally, a previously computed adjacency matrix with on the
#' rows the time points and on the columns the risk set entries
#' @param get_adjmat whether the adjmat computed by tomstats should be
#' outputted as an attribute of the statistics.
#' @inheritParams remstats
#'
#' @section Effects:
#' The statistics to be computed are defined symbolically and should be
#' supplied to the \code{effects} argument in the form \code{~ effects}. The
#' terms are separated by + operators. For example:
#' \code{effects = ~ inertia() + otp()}. Interactions between two effects
#' can be included with * operators. For example:
#' \code{effects = ~ inertia()*otp()}. A list of available effects can be
#' obtained with \code{\link{tie_effects}()}.
#'
#' The majority of the statistics can be scaled in some way, see
#' the documentation of the \code{scaling} argument in the separate effect
#' functions for more information on this.
#'
#' The majority of the statistics can account for the event type
#' included as a dependent variable, see the documentation of the
#' \code{consider_type} argument in the separate effect functions for more
#' information on this.
#'
#' Note that events in the relational event history can be directed or
#' undirected. Some statistics are only defined for either directed or
#' undirected events (see the documentation of the statistics). Note that
#' undirected events are only available for the tie-oriented model.
#'
#' @section attr_actors:
#' For the computation of the \emph{exogenous} statistics an attributes object
#' with the exogenous covariate information has to be supplied to the
#' \code{attr_actors} argument in either \code{remstats()} or in the separate
#' effect functions supplied to the \code{..._effects} arguments (e.g., see
#' \code{\link{send}}). This \code{attr_actors} object should be constructed as
#' follows: A dataframe with rows referring to the attribute value of actor
#' \emph{i} at timepoint \emph{t}. A `name` column is required that contains the
#' actor name (corresponding to the actor names in the relational event
#' history). A `time` column is required that contains the time when attributes
#' change (set to zero if none of the attributes vary over time). Subsequent
#' columns contain the attributes that are called in the specifications of
#' exogenous statistics (column name corresponding to the string supplied to
#' the \code{variable} argument in the effect function). Note that the
#' procedure for the exogenous effects `tie' and `event' deviates from this,
#' here the exogenous covariate information has to be specified in a different
#' way, see \code{\link{tie}} and \code{\link{event}}.
#'
#' @section attr_dyads:
#' For the computation of the \emph{dyad exogenous} statistics with \code{tie()}, an attributes object with the exogenous covariates information per dyad has to be supplied. This is a \code{data.frame} or \code{matrix} containing attribute information for dyads. If \code{attr_dyads} is a \code{data.frame}, the first two columns should represent "actor1" and "actor2" (for directed events, "actor1" corresponds to the sender, and "actor2" corresponds to the receiver). Additional columns can represent dyads' exogenous attributes. If attributes vary over time, include a column named "time". If \code{attr_dyads} is a \code{matrix}, the rows correspond to "actor1", columns to "actor2", and cells contain dyads' exogenous attributes.
#'
#' @section Memory:
#' The default `memory` setting is `"full"`, which implies that at each time
#' point $t$ the entire event history before $t$ is included in the computation
#' of the statistics. Alternatively, when `memory` is set to `"window"`, only
#' the past event history within a given time window is considered (see
#' Mulders & Leenders, 2019). This length of this time window is set by the
#' `memory_value` parameter. For example, when `memory_value = 100` and `memory
#' = "window"`, at time point $t$ only the past events that happened at most
#' 100 time units ago are included in the computation of the statistics.
#' A third option is to set `memory` to `"interval"`. In this case, the past
#' event history within a given time interval is considered. For example, when
#' `"memory_value" = c(50, 100)` and `memory = "window"`, at time point $t$
#' only the past events that happened between 50 and 100 time units ago are
#' included in the computation of the statistics. Finally, the fourth option is
#' to set `memory` to `"decay"`. In this case, the weight of the past event in
#' the computation of the statistics depend on the elapsed time between $t$ and
#' the past event. This weight is determined based on an exponential decay
#' function with half-life parameter `memory_value` (see Brandes et al., 2009).
#'
#' @section Event weights:
#' Note that if the relational event history contains a column that is named
#' ``weight'', it is assumed that these affect the endogenous statistics. These
#' affect the computation of all endogenous statistics with a few exceptions
#' that follow logically from their definition (e.g., the recenyContinue
#' statistic does depend on time since the event and not on event weights).
#'
#' @section Subset the event history using 'start' and 'stop':
#' It is possible to compute statistics for a segment of the relational event 
#' sequence, based on the entire event history. This is done by specifying the 
#' 'start' and 'stop' values as the indices for the first and last event times 
#' for which statistics are needed. For instance, setting 'start = 5' and 'stop 
#' = 5' calculates statistics for the 5th event in the relational event 
#' sequence, considering events 1-4 in the history. Note that in cases of 
#' simultaneous events with the 'method' set to 'pt' (per timepoint), 'start' 
#' and 'stop' should correspond to the indices of the first and last 
#' \emph{unique} event timepoints for which statistics are needed. For example, 
#' if 'start = 5' and 'stop = 5', statistics are computed for the 5th unique 
#' timepoint in the relational event sequence, considering all events occurring 
#' at unique timepoints 1-4.
#'
#' @section Adjacency matrix:
#' Optionally, a previously computed adjacency matrix can be supplied. Note
#' that the endogenous statistics will be computed based on this adjacency
#' matrix. Hence, supplying a previously computed adjacency matrix can reduce
#' computation time but the user should be absolutely sure the adjacency matrix
#' is accurate.
#'
#' @return An object of class 'tomstats'. Array with the computed statistics,
#' where rows refer to time points, columns refer to potential relational event
#' (i.e., potential edges) in the risk set and slices refer to statistics. The
#' 'tomstats' object has the following attributes:
#'   \describe{
#'     \item{\code{model}}{Type of model that is estimated.}
#'     \item{\code{formula}}{Model formula, obtained from the formula inputted to 'tie_effects'.}
#'     \item{\code{riskset}}{The risk set used to construct the statistics.}
#'     \item{\code{adjmat}}{[Optional], if "get_adjmat = TRUE", the matrix with the accumulated event weights for each time point (on the rows) and each dyad (in the columns).}
#'   }
#'
#' @examples
#' library(remstats)
#' 
#' # Load data
#' data(history)
#' data(info)
#' 
#' # Prepare data
#' reh <- remify::remify(edgelist = history, model = "tie")
#' 
#' # Compute effects
#' effects <- ~ inertia():send("extraversion") + otp()
#' tomstats(effects, reh = reh, attr_actors = info)
#'
#' @references Butts, C. T. (2008). A relational event framework for social
#' action. Sociological Methodology, 38(1), 155–200.
#' \doi{10.1111/j.1467-9531.2008.00203.x}
#'
#' @export
tomstats <- function(effects, reh, attr_actors = NULL, attr_dyads = NULL,
                     method = c("pt", "pe"), 
                     memory = c("full", "window", "decay", "interval"),
                     memory_value = NA, start = 1, stop = Inf,
                     display_progress = FALSE,
                     adjmat = NULL, get_adjmat = FALSE,
                     attr_data, attributes, edgelist) {
  # Check if the deprecated argument "attributes" is used
  if (!missing(attributes)) {
    warning("Deprecated argument: Use 'attr_actors' instead of 'attributes'")
    attr_actors <- attributes
  }
  if (!missing(attr_data)) {
    warning("Deprecated argument: Use 'attr_actors' instead of 'attr_data'")
    attr_actors <- attr_data
  }

  # Check if the deprecated "id" column is used in attr_actors
  if (!is.null(attr_actors)) {
    if (("id" %in% colnames(attr_actors)) & !("name" %in% colnames(attr_actors))) {
      warning("use 'name' instead of 'id' in 'attr_actors'")
      colnames(attr_actors)[which(colnames(attr_actors) == "id")] <- "name"
    }
  }

  # Check if the deprecated argument "edgelist" is used
  if (!missing(edgelist)) {
    warning("Deprecated argument: Use 'reh' instead of 'edgelist'")
    reh <- edgelist
  }

  # Prepare all required inputs
  inputs <- prepare_tomstats(
    effects = effects, reh = reh,
    attr_actors = attr_actors, attr_dyads = attr_dyads,
    memory = memory, memory_value = memory_value,
    start = start, stop = stop, method = method
  )

  form <- inputs$form
  effects <- inputs$effects
  effectNames <- inputs$effectNames
  edgelist <- inputs$edgelist
  weights <- inputs$weights
  actors <- inputs$actor
  types <- inputs$types
  riskset <- inputs$riskset
  risksetMatrix <- inputs$risksetMatrix
  memory <- inputs$memory
  memory_value <- inputs$memory_value
  scaling <- inputs$scaling
  consider_type <- inputs$consider_type
  covar <- inputs$covar
  interactions <- inputs$interactions
  start <- inputs$start
  stop <- inputs$stop
  method <- inputs$method

  # Compute the inertia building block
  if (is.null(adjmat)) {
    if (any(grepl("degree", effectNames)) | any(effectNames %in% c("inertia", "reciprocity", "otp", "itp", "osp", "isp", "sp"))) {
      inertia <- calculate_inertia(edgelist, weights, risksetMatrix, memory,
        memory_value, start, stop, display_progress, method
      )
    } else {
      inertia <- matrix()
    }
  }

  # Compute statistics
  statistics <- compute_stats_tie(effectNames, edgelist, riskset, 
    risksetMatrix, inertia, covar, interactions, memory, memory_value, scaling, 
    consider_type, start, stop, attr(reh, "directed"), display_progress, method)

  # Add variable names to the statistics dimnames
  statistics <- add_variable_names(
    statistics, effectNames, effects,
    interactions
  )

  # Modify riskset output
  riskset <- modify_riskset(riskset, reh, actors, types)

  # Format output
  class(statistics) <- c("tomstats", "remstats")
  attr(statistics, "model") <- "tie"
  attr(statistics, "formula") <- form
  attr(statistics, "riskset") <- riskset
  attr(statistics, "subset") <- data.frame(start = start + 1, stop = stop + 1)
  attr(statistics, "method") <- method
  if (get_adjmat) {
    attr(statistics, "adjmat") <- adjmat
  }

  # Output
  statistics
}



#' tomstats2
#'
#' Computes statistics for modeling relational event history data
#' with the tie-oriented relational event model.
#'
#' @param effects an object of class \code{"\link[stats]{formula}"} (or one
#' that can be coerced to that class): a symbolic description of the effects in
#' the model for which statistics are computed, see 'Details' for the available
#' effects and their corresponding statistics
#' @param sampling Logical. If \code{TRUE}, statistics are computed using
#'   case–control (dyad) sampling rather than the full risk set. Default \code{FALSE}.
#' @param samp_num Integer. Number of dyads to include per event when
#'   \code{sampling = TRUE}. Must be smaller than or equal to the size of the
#'   active risk set. Ignored when \code{sampling = FALSE}.
#' @param seed Optional integer. Random seed used for dyad sampling. Setting
#'   this ensures reproducible sampling across calls. If \code{NULL}, the
#'   current RNG state is used.
#' @param adjmat optionally, a previously computed adjacency matrix with on the
#' rows the time points and on the columns the risk set entries
#' @param get_adjmat whether the adjmat computed by tomstats should be
#' outputted as an attribute of the statistics.
#' @inheritParams remstats
#'
#' @section Effects:
#' The statistics to be computed are defined symbolically and should be
#' supplied to the \code{effects} argument in the form \code{~ effects}. The
#' terms are separated by + operators. For example:
#' \code{effects = ~ inertia() + otp()}. Interactions between two effects
#' can be included with * operators. For example:
#' \code{effects = ~ inertia()*otp()}. A list of available effects can be
#' obtained with \code{\link{tie_effects}()}.
#'
#' The majority of the statistics can be scaled in some way, see
#' the documentation of the \code{scaling} argument in the separate effect
#' functions for more information on this.
#'
#' The majority of the statistics can account for the event type
#' included as a dependent variable, see the documentation of the
#' \code{consider_type} argument in the separate effect functions for more
#' information on this.
#'
#' Note that events in the relational event history can be directed or
#' undirected. Some statistics are only defined for either directed or
#' undirected events (see the documentation of the statistics). Note that
#' undirected events are only available for the tie-oriented model.
#'
#' @section attr_actors:
#' For the computation of the \emph{exogenous} statistics an attributes object
#' with the exogenous covariate information has to be supplied to the
#' \code{attr_actors} argument in either \code{remstats()} or in the separate
#' effect functions supplied to the \code{..._effects} arguments (e.g., see
#' \code{\link{send}}). This \code{attr_actors} object should be constructed as
#' follows: A dataframe with rows referring to the attribute value of actor
#' \emph{i} at timepoint \emph{t}. A `name` column is required that contains the
#' actor name (corresponding to the actor names in the relational event
#' history). A `time` column is required that contains the time when attributes
#' change (set to zero if none of the attributes vary over time). Subsequent
#' columns contain the attributes that are called in the specifications of
#' exogenous statistics (column name corresponding to the string supplied to
#' the \code{variable} argument in the effect function). Note that the
#' procedure for the exogenous effects `tie' and `event' deviates from this,
#' here the exogenous covariate information has to be specified in a different
#' way, see \code{\link{tie}} and \code{\link{event}}.
#'
#' @section attr_dyads:
#' For the computation of the \emph{dyad exogenous} statistics with \code{tie()}, an attributes object with the exogenous covariates information per dyad has to be supplied. This is a \code{data.frame} or \code{matrix} containing attribute information for dyads. If \code{attr_dyads} is a \code{data.frame}, the first two columns should represent "actor1" and "actor2" (for directed events, "actor1" corresponds to the sender, and "actor2" corresponds to the receiver). Additional columns can represent dyads' exogenous attributes. If attributes vary over time, include a column named "time". If \code{attr_dyads} is a \code{matrix}, the rows correspond to "actor1", columns to "actor2", and cells contain dyads' exogenous attributes.
#'
#' @section Memory:
#' The default `memory` setting is `"full"`, which implies that at each time
#' point $t$ the entire event history before $t$ is included in the computation
#' of the statistics. Alternatively, when `memory` is set to `"window"`, only
#' the past event history within a given time window is considered (see
#' Mulders & Leenders, 2019). This length of this time window is set by the
#' `memory_value` parameter. For example, when `memory_value = 100` and `memory
#' = "window"`, at time point $t$ only the past events that happened at most
#' 100 time units ago are included in the computation of the statistics.
#' A third option is to set `memory` to `"interval"`. In this case, the past
#' event history within a given time interval is considered. For example, when
#' `"memory_value" = c(50, 100)` and `memory = "window"`, at time point $t$
#' only the past events that happened between 50 and 100 time units ago are
#' included in the computation of the statistics. Finally, the fourth option is
#' to set `memory` to `"decay"`. In this case, the weight of the past event in
#' the computation of the statistics depend on the elapsed time between $t$ and
#' the past event. This weight is determined based on an exponential decay
#' function with half-life parameter `memory_value` (see Brandes et al., 2009).
#'
#' @section Event weights:
#' Note that if the relational event history contains a column that is named
#' ``weight'', it is assumed that these affect the endogenous statistics. These
#' affect the computation of all endogenous statistics with a few exceptions
#' that follow logically from their definition (e.g., the recenyContinue
#' statistic does depend on time since the event and not on event weights).
#'
#' @section Subset the event history using 'start' and 'stop':
#' It is possible to compute statistics for a segment of the relational event 
#' sequence, based on the entire event history. This is done by specifying the 
#' 'start' and 'stop' values as the indices for the first and last event times 
#' for which statistics are needed. For instance, setting 'start = 5' and 'stop 
#' = 5' calculates statistics for the 5th event in the relational event 
#' sequence, considering events 1-4 in the history. Note that in cases of 
#' simultaneous events with the 'method' set to 'pt' (per timepoint), 'start' 
#' and 'stop' should correspond to the indices of the first and last 
#' \emph{unique} event timepoints for which statistics are needed. For example, 
#' if 'start = 5' and 'stop = 5', statistics are computed for the 5th unique 
#' timepoint in the relational event sequence, considering all events occurring 
#' at unique timepoints 1-4.
#'
#' @section Adjacency matrix:
#' Optionally, a previously computed adjacency matrix can be supplied. Note
#' that the endogenous statistics will be computed based on this adjacency
#' matrix. Hence, supplying a previously computed adjacency matrix can reduce
#' computation time but the user should be absolutely sure the adjacency matrix
#' is accurate.
#'
#' @return An object of class 'tomstats'. Array with the computed statistics,
#' where rows refer to time points, columns refer to potential relational event
#' (i.e., potential edges) in the risk set and slices refer to statistics. The
#' 'tomstats' object has the following attributes:
#'   \describe{
#'     \item{\code{model}}{Type of model that is estimated.}
#'     \item{\code{formula}}{Model formula, obtained from the formula inputted to 'tie_effects'.}
#'     \item{\code{riskset}}{The risk set used to construct the statistics.}
#'     \item{\code{adjmat}}{[Optional], if "get_adjmat = TRUE", the matrix with the accumulated event weights for each time point (on the rows) and each dyad (in the columns).}
#'   }
#' @export
tomstats2 <- function(
		effects,
		reh,
		attr_actors = NULL,
		attr_dyads = NULL,
		memory = c("full", "window", "decay", "interval"),
		memory_value = NA,
		start = 1,
		stop = Inf,
		display_progress = FALSE,
		# new
		sampling = FALSE, # sampling = TRUE
		samp_num = NULL, # samp_num = 20
		seed = NULL,
		# deprecated passthrough
		method <- "pt",
		adjmat <- NULL,
		get_adjmat <- FALSE,
		attr_data,
		attributes,
		edgelist
) {
	#remove not needed arguments

	
	if (!is.null(adjmat)) {
		warning(
			"`adjmat` is deprecated and ignored and will be removed in a future release.",
			call. = FALSE
		)
	}
	
	if (!identical(method, "pt")) {
		warning(
			"`method` is deprecated and fixed to 'pt' and will be removed in a future release.",
			call. = FALSE
		)
		method <- "pt"
	}
	
	if (isTRUE(get_adjmat)) {
		warning(
			"`get_adjmat` is deprecated and will be removed in a future release.",
			call. = FALSE
		)
		get_adjmat <- FALSE
	}
	
	method <- match.arg(method)
	memory <- match.arg(memory)
	
	if (!isTRUE(sampling)) {
		# original behavior
		return(remstats::tomstats(
			effects = effects,
			reh = reh,
			attr_actors = attr_actors,
			attr_dyads = attr_dyads,
			method = method,
			memory = memory,
			memory_value = memory_value,
			start = start,
			stop = stop,
			display_progress = display_progress,
			adjmat = adjmat,
			get_adjmat = get_adjmat,
			attr_data = attr_data,
			attributes = attributes, edgelist = edgelist
		))
		
	}else{
	
	# ---- sampling mode checks ----
	if (!is.null(adjmat) || isTRUE(get_adjmat)) {
		stop("sampling=TRUE: 'adjmat' and 'get_adjmat' are disabled (by design).")
	}
	if (is.null(samp_num) || length(samp_num) != 1L || is.na(samp_num)) {
		stop("sampling=TRUE requires a single non-missing integer 'samp_num'.")
	}
	samp_num <- as.integer(samp_num)
	if (samp_num <= 0L) stop("'samp_num' must be > 0.")
	
	if (!is.null(seed)) set.seed(as.integer(seed))
	
	# ---- prepare inputs exactly as current tomstats() does ----
	inputs <- prepare_tomstats(
		effects = effects,
		reh = reh,
		attr_actors = attr_actors,
		attr_dyads = attr_dyads,
		memory = memory,
		memory_value = memory_value,
		start = start,
		stop = stop,
		method = method
	)
	
	form <- inputs$form
	effectNames <- inputs$effectNames
	edgelist <- inputs$edgelist
	weights <- inputs$weights
	actors <- inputs$actor
	types <- inputs$types
	riskset <- inputs$riskset                 # base dyad list (size D_base)
	risksetMatrix <- inputs$risksetMatrix
	memory <- inputs$memory
	memory_value <- inputs$memory_value
	scaling <- inputs$scaling
	consider_type <- inputs$consider_type
	covar <- inputs$covar
	interactions <- inputs$interactions
	start0 <- inputs$start                    # 0-based for C++
	stop0 <- inputs$stop                      # 0-based for C++
	method <- inputs$method
	
	# ---- construct row index -> event indices, and cases per row ----
	ed <- reh$edgelist
	if (is.null(ed) || !all(c("time","actor1","actor2") %in% names(ed))) {
		stop("reh$edgelist must contain columns time, actor1, actor2.")
	}
	
	# determine row timepoints/events used by compute_stats_tie (C++ uses start0/stop0 on time_points)
	if (method == "pt") {
		time_points_all <- sort(unique(ed$time))
		# start0/stop0 are 0-based indices into time_points_all
		row_times <- time_points_all[(start0 + 1L):(stop0 + 1L)]
		events_by_row <- lapply(row_times, function(tt) which(ed$time == tt))
	} else { # pe
		# C++ uses time_points = edgelist.col(0) and then subvec(start0, stop0)
		# That corresponds to event rows (start0+1):(stop0+1) in the edgelist passed into prepare_tomstats.
		ev_idx <- (start0 + 1L):(stop0 + 1L)
		events_by_row <- as.list(ev_idx)
	}
	
	M <- length(events_by_row)
	D_base <- nrow(riskset)
	if (samp_num > D_base) stop("samp_num cannot exceed base riskset size.")
	
	# map observed (sender,receiver) -> base dyad id
	directed <- isTRUE(attr(reh, "directed"))
	make_key <- function(s, r) {
		s <- as.character(s); r <- as.character(r)
		if (directed) paste0(s,"|",r) else ifelse(s <= r, paste0(s,"|",r), paste0(r,"|",s))
	}
	riskset_named <- modify_riskset(riskset, reh, actors, types)
	rs_key <- paste0(riskset_named$sender, "|", riskset_named$receiver)
	key_to_base <- setNames(seq_len(nrow(riskset_named)), rs_key)
	
	# ---- sample_map, case_pos, pi, log_pi ----
	sample_map <- matrix(NA_integer_, nrow = M, ncol = samp_num)
	pi <- matrix(NA_real_, nrow = M, ncol = samp_num)
	log_pi <- matrix(NA_real_, nrow = M, ncol = samp_num)
	case_pos <- case_mult <- vector("list", M)
	
	for (m in seq_len(M)) {
		ev_idx <- events_by_row[[m]]
		keys <- make_key(ed$actor1[ev_idx], ed$actor2[ev_idx])
		cases_all <- as.integer(unname(key_to_base[keys]) - 1L)
		tab <- table(cases_all)
		cases <- as.integer(names(tab))          # unique case dyads (0-based)
		case_mult[[m]] <- as.integer(tab)        # multiplicities
		m_t <- length(cases)
		
		if (samp_num < m_t) {
			#### then the number of events in a time point is more than allowed in the sampling
			idx <- sample.int(m_t, size = samp_num, replace = FALSE)
			cases <- cases[idx]
			case_mult[[m]] <- case_mult[[m]][idx]
			m_t <- length(cases)
		}
		
		c_m <- samp_num - m_t
		if (c_m > 0L) {
			pool <- setdiff(0:(D_base - 1L), cases)
			controls <- sample(pool, size = c_m, replace = FALSE)
			S_m <- c(cases, controls) # sampled riskset starting with observed dyad(s)
			pi_ctrl <- c_m / (D_base - m_t) # sampling probabilities for sampled dyads
			pi_m <- c(rep(1, m_t), rep(pi_ctrl, c_m)) # sampling probabilities in riskset.
		} else { #all observed dyads are included in riskset
			S_m <- cases
			pi_m <- rep(1, m_t)
		}
		
		sample_map[m, ] <- as.integer(S_m)
		case_pos[[m]] <- seq_len(m_t) #position of observed dyad(s) in sampled riskset
		pi[m, ] <- pi_m
		log_pi[m, ] <- log(pi_m)
	}
	storage.mode(sample_map) <- "integer"
	
	# ---- compute sampled statistics in C++ ----
	statistics <- compute_stats_tie_sampled(
		effects = effectNames,
		edgelist = edgelist,
		weights = weights,
		riskset = riskset,
		risksetMatrix = risksetMatrix,
		covariates = covar,
		interactions = interactions,
		memory = memory,
		memory_value = memory_value,
		scaling = scaling,
		consider_type = consider_type,
		start = start0,
		stop = stop0,
		directed = attr(reh, "directed"),
		display_progress = display_progress,
		method = method,
		sample_map = sample_map
	)
	
	# name slices as current code does
	statistics <- add_variable_names(statistics, effectNames, inputs$effects, interactions)
	
	# base riskset output as usual (static reference)
	riskset_out <- modify_riskset(riskset, reh, actors, types)
	
	class(statistics) <- c("tomstats_sampled", "remstats")
	attr(statistics, "model") <- "tie"
	attr(statistics, "formula") <- form
	attr(statistics, "riskset") <- riskset_out
	attr(statistics, "riskset_active") <- sample_map
	attr(statistics, "samp_num") <- samp_num
	attr(statistics, "events_mult") <- case_mult
	attr(statistics, "subset") <- data.frame(start = start0 + 1L, stop = stop0 + 1L)
	attr(statistics, "method") <- method
	
	# sampling metadata for remstimate
	attr(statistics, "sampling") <- TRUE
	attr(statistics, "samp_num") <- samp_num
	attr(statistics, "dyad_keys") <- key_to_base
	attr(statistics, "sample_map") <- sample_map
	attr(statistics, "case_pos") <- case_pos
	attr(statistics, "pi") <- pi
	attr(statistics, "log_pi") <- log_pi
	attr(statistics, "seed") <- seed
	attr(statistics, "sampling_scheme") <- "uniform_wor_static_riskset"
	
	return(statistics)
	}
}

