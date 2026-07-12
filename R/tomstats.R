
#tomstats without case control sampling
tomstats0 <- function(effects, reh, attr_actors = NULL, attr_dyads = NULL,
                     memory = c("full", "window", "decay", "interval"),
                     memory_value = NA, start = 2, stop = Inf,
                     display_progress = FALSE) {
  # Check if the deprecated argument "attributes" is used

	method <- "pt"
	
  # Check if the deprecated "id" column is used in attr_actors
  if (!is.null(attr_actors)) {
    if (("id" %in% colnames(attr_actors)) & !("name" %in% colnames(attr_actors))) {
      warning("use 'name' instead of 'id' in 'attr_actors'")
      colnames(attr_actors)[which(colnames(attr_actors) == "id")] <- "name"
    }
  }

  if (!("remify" %in% class(reh))) stop("Expected a reh object of class remify")
  reh <- normalize_reh(reh)

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
  consider_type     <- inputs$consider_type
  consider_type_cpp <- inputs$consider_type_cpp
  covar <- inputs$covar
  interactions <- inputs$interactions
  start <- inputs$start
  stop <- inputs$stop
  method <- inputs$method
  with_type_riskset <- inputs$with_type_riskset
  needs_typed_riskset <- inputs$needs_typed_riskset
  C <- inputs$C

  # Inertia-needing effect names
  needs_inertia <- any(grepl("degree", effectNames)) |
    any(effectNames %in% c("inertia", "reciprocity", "otp", "itp", "osp", "isp", "sp"))

  # Compute the inertia building block (aggregated, all types)
  if (needs_inertia) {
  	inertia <- calculate_inertia(edgelist, weights, risksetMatrix, memory,
  															 memory_value, start, stop, display_progress, method
  	)
  } else {
  	inertia <- matrix()
  }

  # Compute statistics (unfiltered pass — correct for "ignore" effects)
  statistics <- compute_stats_tie(effectNames, edgelist, riskset,
    risksetMatrix, inertia, covar, interactions, memory, memory_value, scaling,
    consider_type_cpp, start, stop, reh$meta$directed, display_progress, method)

  # Add variable names to the statistics dimnames
  statistics <- add_variable_names(
    statistics, effectNames, effects,
    interactions
  )

  # For "separate"/"interact" effects with ext=TRUE and C>1: run C additional
  # filtered passes (one per past-event-type) to get per-type statistics.
  # Each pass filters the edgelist and inertia building block to type c only,
  # then computes only the "separate"/"interact" effects.
  stats_by_type <- NULL
  # Multi-pass needed for "separate" or "interact" with C>1, regardless of ext
  needs_multipass <- C > 1 &&
    any(consider_type[seq_along(effects)] %in% c(1L, 2L))

  if (needs_multipass) {
    # Indices of effects needing multi-pass (separate or interact)
    mp_idx <- which(consider_type[seq_along(effects)] %in% c(1L, 2L))
    mp_effectNames   <- effectNames[mp_idx]
    mp_scaling       <- scaling[mp_idx]
    mp_consider_type <- consider_type[mp_idx]  # 1L=separate, 2L=interact
    # consider_type_cpp for multi-pass effects: always FALSE (we filter edgelist)
    mp_ct_cpp <- rep(FALSE, length(mp_idx))

    stats_by_type <- vector("list", C)
    for (ci in seq_len(C)) {
      type_id_c <- types$typeID[ci]  # 0-based
      # Zero weights for non-type-c events — keeps full edgelist structure
      # intact (time indices must match) but only counts type-c events
      mask_c <- edgelist[, 4] == type_id_c
      wt_c <- weights * mask_c

      # Inertia building block for type c only (via zeroed weights)
      if (needs_inertia && length(mp_effectNames) > 0) {
        inertia_c <- calculate_inertia(edgelist, wt_c, risksetMatrix, memory,
          memory_value, start, stop, display_progress, method)
      } else {
        inertia_c <- matrix()
      }

      # Compute statistics using type-c inertia building block
      stats_c <- compute_stats_tie(
        mp_effectNames, edgelist, riskset,
        risksetMatrix, inertia_c, covar, interactions, memory, memory_value,
        mp_scaling, mp_ct_cpp, start, stop, reh$meta$directed,
        display_progress, method)

      # Add names (set directly to avoid length mismatch with interactions)
      dimnames(stats_c)[[3]] <- mp_effectNames

      # Pshift effects: overwrite with correct type-c values.
      # The masked-weights approach above does not affect pshifts because
      # compute_stats_tie ignores weights for pshift computation. Instead,
      # use R-level computation with correct method="pt" semantics (previous
      # time point events only).
      # ext=FALSE: always type-filter (separate dyads have no type column).
      # ext=TRUE + interact: type-filter rows (past-type x dyad-type semantics).
      # ext=TRUE + separate: leave as-is — base C++ stat is correct; all type
      #   slices share the same dyad pattern (defensible: "did ABAB occur,
      #   regardless of which dyad type you are?").
      ps_in_mp <- which(mp_effectNames %in% PSHIFT_NAMES)
      if (length(ps_in_mp) > 0) {
        event_type_ids_0 <- edgelist[, 4]
        M_out_ps <- stop - start + 1L
        for (ei in ps_in_mp) {
          pshift_num  <- PSHIFT_TYPE_NUM[mp_effectNames[ei]]
          is_interact <- mp_consider_type[ei] == 2L
          if (!is.null(inputs$prepR_untyped)) {
            # ext=FALSE: always type-filter
            rs_a1 <- inputs$prepR_untyped[, 1]
            rs_a2 <- inputs$prepR_untyped[, 2]
            correct_vals <- compute_pshift_type_c_tie(
              pshift_num, edgelist, event_type_ids_0, type_id_c,
              rs_a1, rs_a2, M_out_ps, start, stop
            )
            type_ids_riskset <- inputs$riskset[, 3]
            type_rows_ci <- which(type_ids_riskset == type_id_c)
            key_untyped <- paste(rs_a1, rs_a2, sep = "|")
            key_typed_c <- paste(inputs$riskset[type_rows_ci, 1],
                                 inputs$riskset[type_rows_ci, 2], sep = "|")
            dst <- match(key_typed_c, key_untyped)
            valid <- !is.na(dst)
            stats_c[, type_rows_ci[valid], ei] <- correct_vals[, dst[valid]]
          } else if (is_interact) {
            # ext=TRUE + interact: type-filter rows
            rs_a1 <- inputs$riskset[, 1]
            rs_a2 <- inputs$riskset[, 2]
            correct_vals <- compute_pshift_type_c_tie(
              pshift_num, edgelist, event_type_ids_0, type_id_c,
              rs_a1, rs_a2, M_out_ps, start, stop
            )
            stats_c[, , ei] <- correct_vals
          }
          # ext=TRUE + separate: leave stats_c as-is (base C++ stat)
        }
      }

      stats_by_type[[ci]] <- stats_c
    }
  }

  # Split consider_type stats into type-specific slices when needed
  if (needs_typed_riskset && C > 1) {
    statistics <- split_type_slices(
      statistics        = statistics,
      effects           = effects,
      consider_type     = consider_type,
      types             = types,
      prepR_typed       = inputs$riskset,
      with_type_riskset = with_type_riskset,
      prepR_untyped     = inputs$prepR_untyped,
      stats_by_type     = stats_by_type
    )
  }

  # Modify riskset output
  if (!with_type_riskset && needs_typed_riskset) {
    riskset <- build_untyped_riskset(reh, actors, types)
  } else {
    riskset <- modify_riskset(riskset, reh, actors, types)
  }

  # Format output
  class(statistics) <- c("tomstats", "remstats")
  attr(statistics, "model") <- "tie"
  attr(statistics, "formula") <- form
  attr(statistics, "riskset") <- riskset
  attr(statistics, "subset") <- data.frame(start = start + 1, stop = stop + 1)
  attr(statistics, "method") <- method
  # if (get_adjmat) {
  #   attr(statistics, "adjmat") <- adjmat
  # }
  rs <- attr(statistics, "riskset")  # data.frame with sender/receiver/id
  
  sender   <- as.character(rs$sender)
  receiver <- as.character(rs$receiver)
  
  keys <- paste0(sender, "|", receiver)
  
  idx <- seq_along(keys)
  if (!is.null(rs$id) && all(rs$id == idx)) idx <- rs$id
  
  dyad_keys <- stats::setNames(idx, keys)
  
  attr(statistics, "dyad_keys") <- dyad_keys
  

  # Output
  statistics
}


#' tomstats
#'
#' Computes statistics for modeling relational event history data
#' with the tie-oriented relational event model.
#'
#' @param tie_effects an object of class \code{"\link[stats]{formula}"} (or one
#' that can be coerced to that class): a symbolic description of the tie_effects in
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
#' @inheritParams remstats
#'
#' @section Effects:
#' The statistics to be computed are defined symbolically and should be
#' supplied to the \code{effects} argument in the form \code{~ effects}. The
#' terms are separated by + operators. For example:
#' \code{tie_effects = ~ inertia() + otp()}. Interactions between two effects
#' can be included with * operators. For example:
#' \code{tie_effects = ~ inertia()*otp()}. A list of available effects can be
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
#' @section Subset the event history using 'first' and 'last':
#' It is possible to compute statistics for a segment of the relational event 
#' sequence, based on the entire event history. This is done by specifying the 
#' 'first' and 'last' values as the indices for the first and last event times 
#' for which statistics are needed. For instance, setting 'first = 5' and 'last 
#' = 5' calculates statistics for the 5th event in the relational event 
#' sequence. Note that in cases of simultaneous events, 'first' and 'last'
#' correspond to the indices of the first and last unique event timepoints.
#' \emph{unique} event timepoints for which statistics are needed. For example, 
#' if 'first = 5' and 'last = 5', statistics are computed for the 5th unique 
#' timepoint in the relational event sequence, considering all events occurring 
#' at unique timepoints 1-4.
#'
#' @return An object of class 'tomstats'. Array with the computed statistics,
#' where rows refer to time points, columns refer to potential relational event
#' (i.e., potential edges) in the risk set and slices refer to statistics.
#' Statistics are computed once per unique time point (per-timepoint "pt"
#' method), so the number of rows in the output equals \code{reh$M} (the number 
#' of  unique time points), which may be less than the total number of observed 
#' events when simultaneous events are present. The 'tomstats' object has the 
#' following attributes:
#'   \describe{
#'     \item{\code{model}}{Type of model that is estimated.}
#'     \item{\code{formula}}{Model formula, obtained from the formula inputted to 'tie_effects'.}
#'     \item{\code{riskset}}{The risk set used to construct the statistics.}
#'   }
#' @export
tomstats <- function(
		tie_effects,
		reh,
		memory = c("full", "window", "decay", "interval"),
		memory_value = NA,
		first = 2,
		last = Inf,
		display_progress = FALSE,
		# new
		sampling = FALSE, # sampling = TRUE
		samp_num = 10L, # samp_num = 20
		seed = NULL,
		attr_actors = NULL,
		attr_dyads = NULL
) {
	#remove not needed arguments
	
	if (!is.null(attr_actors)) {
		warning("'attr_actors' is deprecated. Supply attributes directly in the stat functions (e.g., send()). This argument is ignored.",
						call. = FALSE)
	}
	if (!is.null(attr_dyads)) {
		warning("'attr_dyads' is deprecated. Supply attributes directly in the stat functions (e.g., send()). This argument is ignored.",
						call. = FALSE)
	}
	
	#change of argument names
	method <- "pt"
	start <- first
	stop <- last
	effects <- tie_effects
	
	memory <- match.arg(memory)
	
	if (!isTRUE(sampling)) {
		# original behavior
		return(tomstats0(
			effects = effects,
			reh = reh,
			attr_actors = attr_actors,
			attr_dyads = attr_dyads,
			memory = memory,
			memory_value = memory_value,
			start = start,
			stop = stop,
			display_progress = display_progress
		))
		
	}else{
	
	# ---- sampling mode checks ----
	reh <- normalize_reh(reh)  # class already checked above in non-sampling dispatch
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
	consider_type     <- inputs$consider_type
	consider_type_cpp <- inputs$consider_type_cpp
	covar <- inputs$covar
	interactions <- inputs$interactions
	start0 <- inputs$start                    # 0-based for C++
	stop0 <- inputs$stop                      # 0-based for C++
	method <- inputs$method
	with_type_riskset <- inputs$with_type_riskset
	needs_typed_riskset <- inputs$needs_typed_riskset
	C <- inputs$C
	
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

	# For ext=FALSE with typed stats (needs_typed_riskset=TRUE, with_type_riskset=FALSE):
	# sample from the UNTYPED riskset so sample_map indexes ts_full columns directly.
	# C++ still receives the typed riskset; sample_map is expanded to typed positions
	# before the C++ call and collapsed back afterwards.
	# All other cases: sample from the base (possibly typed) riskset as before.
	sample_untyped <- needs_typed_riskset && !with_type_riskset
	sample_riskset <- if (sample_untyped) inputs$prepR_untyped else riskset

	D_base <- nrow(sample_riskset)
	if (samp_num > D_base) stop("samp_num cannot exceed base riskset size.")

	directed <- isTRUE(reh$meta$directed)

	make_key_untyped <- function(a1, a2) {
		if (!directed) { lo <- pmin(a1,a2); hi <- pmax(a1,a2) } else { lo <- a1; hi <- a2 }
		paste(lo, hi, sep="|")
	}
	make_key_typed <- function(a1, a2, ty) {
		if (!directed) { lo <- pmin(a1,a2); hi <- pmax(a1,a2) } else { lo <- a1; hi <- a2 }
		paste(lo, hi, ty, sep="|")
	}

	if (sample_untyped) {
		# Untyped sampling: build key directly from 0-based integer cols of sample_riskset.
		# Avoid modify_riskset — name lookup is fragile when prepR_untyped and the typed
		# riskset have different dyad subsets (e.g. active riskset where not all actor pairs
		# appear in both types). Use row index as 1-based id into sample_riskset.
		rs_key      <- make_key_untyped(sample_riskset[, 1], sample_riskset[, 2])
		key_to_base <- setNames(seq_len(nrow(sample_riskset)), rs_key)
	} else if (C > 1L && ncol(edgelist) >= 4L) {
		# Typed sampling: 3-part key — must match the typed case keys below.
		rs_key      <- make_key_typed(riskset[, 1], riskset[, 2], riskset[, 3])
		key_to_base <- setNames(riskset[, 4] + 1L, rs_key)
	} else {
		# Untyped model (C == 1): 2-part key — must match make_key_untyped case keys.
		rs_key      <- make_key_untyped(riskset[, 1], riskset[, 2])
		key_to_base <- setNames(riskset[, 4] + 1L, rs_key)
	}

	# typed riskset ids should be 0..D_typed-1
	stopifnot(max(riskset[,4]) == nrow(riskset) - 1L)

	# ---- sample_map, case_pos, pi, log_pi ----
	# sample_map is 0-based, indexes sample_riskset (untyped when sample_untyped=TRUE)
	sample_map <- matrix(NA_integer_, nrow = M, ncol = samp_num)
	pi         <- matrix(NA_real_,    nrow = M, ncol = samp_num)
	log_pi     <- matrix(NA_real_,    nrow = M, ncol = samp_num)
	case_pos   <- case_mult <- vector("list", M)

	for (m in seq_len(M)) {
		ev_idx <- events_by_row[[m]]
		if (sample_untyped) {
			# Use 0-based actor IDs from the prepared edgelist matrix (cols 2,3)
			# to match the 0-based integer keys built from sample_riskset.
			keys <- make_key_untyped(edgelist[ev_idx, 2], edgelist[ev_idx, 3])
		} else if (C > 1L && ncol(edgelist) >= 4L) {
			keys <- make_key_typed(edgelist[ev_idx, 2], edgelist[ev_idx, 3], edgelist[ev_idx, 4])
		} else {
			keys <- make_key_untyped(edgelist[ev_idx, 2], edgelist[ev_idx, 3])
		}
		cases_all <- as.integer(unname(key_to_base[keys]) - 1L)
		tab <- table(cases_all)
		cases <- as.integer(names(tab))          # unique case dyads (0-based)
		case_mult[[m]] <- as.integer(tab)        # multiplicities
		m_t <- length(cases)

		if (samp_num < m_t) {
			idx <- sample.int(m_t, size = samp_num, replace = FALSE)
			cases <- cases[idx]
			case_mult[[m]] <- case_mult[[m]][idx]
			m_t <- length(cases)
		}

		c_m <- samp_num - m_t
		if (c_m > 0L) {
			pool     <- setdiff(0:(D_base - 1L), cases)
			controls <- sample(pool, size = c_m, replace = FALSE)
			S_m      <- c(cases, controls)
			pi_ctrl  <- c_m / (D_base - m_t)
			pi_m     <- c(rep(1, m_t), rep(pi_ctrl, c_m))
		} else {
			S_m  <- cases
			pi_m <- rep(1, m_t)
		}

		sample_map[m, ] <- as.integer(S_m)
		case_pos[[m]]   <- seq_len(m_t)
		pi[m, ]         <- pi_m
		log_pi[m, ]     <- log(pi_m)
	}
	storage.mode(sample_map) <- "integer"

	# ---- build sample_map_cpp for C++ (ext=FALSE only) ----
	# C++ needs 0-based typed riskset row indices.
	#
	# For sample_untyped=TRUE: run C separate C++ calls, one per type.
	# Each call uses sample_map_cpp_c [M x samp_num] pointing to the typed
	# riskset row for type c for each sampled untyped dyad (or the first
	# available row as fallback when that type doesn't exist for a dyad).
	# Results are assembled into named type slices in R without any zeroing:
	# the value for type ci comes from call ci, so it's always correct.
	# This avoids expanding sample_map to [M x samp_num*C] and handles both
	# Scenario A (1 type per dyad) and Scenario B (C types per dyad) correctly.
	#
	# For sample_untyped=FALSE: single C++ call as before.
	if (sample_untyped) {
		key_typed_all <- make_key_untyped(riskset[, 1], riskset[, 2])

		# For each untyped dyad and each type, find the typed riskset row (1-based).
		# If a dyad doesn't have a row for that type, fall back to any existing row
		# so C++ never receives NA. The value will be overridden to 0 in assembly.
		type_ids_0based <- types$typeID  # 0-based, length C

		# typed_row_map[[d]][ci] = 1-based riskset row for untyped dyad d, type ci
		# type_exists[[d]][ci]   = TRUE if that type actually exists for dyad d
		typed_row_map <- vector("list", D_base)
		type_exists   <- vector("list", D_base)
		for (d in seq_len(D_base)) {
			rows   <- which(key_typed_all == rs_key[d])
			exists <- sapply(type_ids_0based, function(tid) any(riskset[rows, 3] == tid))
			rows_c <- sapply(type_ids_0based, function(tid) {
				r <- rows[riskset[rows, 3] == tid]
				if (length(r) == 0L) rows[1L] else r[1L]  # fallback to any row
			})
			typed_row_map[[d]] <- rows_c
			type_exists[[d]]   <- exists
		}

		# Run one C++ call per type, each with [M x samp_num] sample_map
		# For "separate"/"interact": weight-mask to type-c events per pass
		needs_mp_samp <- any(consider_type[seq_along(inputs$effects)] %in% c(1L, 2L))
		mp_idx_samp  <- which(consider_type[seq_along(inputs$effects)] %in% c(1L, 2L))
		mp_eff_samp  <- if (needs_mp_samp) effectNames[mp_idx_samp] else effectNames
		mp_scal_samp <- if (needs_mp_samp) scaling[mp_idx_samp] else scaling
		mp_ct_samp   <- rep(FALSE, length(mp_eff_samp))

		stats_by_type <- vector("list", C)
		for (ci in seq_len(C)) {
			smap_c <- matrix(NA_integer_, nrow = M, ncol = samp_num)
			for (m in seq_len(M)) {
				for (s in seq_len(samp_num)) {
					d <- sample_map[m, s] + 1L          # 1-based untyped index
					smap_c[m, s] <- typed_row_map[[d]][ci] - 1L  # 0-based for C++
				}
			}
			storage.mode(smap_c) <- "integer"
			type_id_c <- types$typeID[ci]
			wt_c   <- if (needs_mp_samp) weights * (edgelist[, 4] == type_id_c) else weights
			eff_c  <- if (needs_mp_samp) mp_eff_samp  else effectNames
			scal_c <- if (needs_mp_samp) mp_scal_samp else scaling
			ct_c   <- if (needs_mp_samp) mp_ct_samp   else consider_type_cpp
			stats_by_type[[ci]] <- compute_stats_tie_sampled(
				effects = eff_c,
				edgelist = edgelist,
				weights = wt_c,
				riskset = riskset,
				risksetMatrix = risksetMatrix,
				covariates = covar,
				interactions = interactions,
				memory = memory,
				memory_value = memory_value,
				scaling = scal_c,
				consider_type = ct_c,
				start = start0,
				stop = stop0,
				directed = reh$meta$directed,
				display_progress = display_progress,
				method = method,
				sample_map = smap_c
			)

			# Pshift override: C++ ignores weights so weight masking has no effect.
			# Use R-level compute_pshift_type_c_tie indexed through sample_map.
			if (needs_mp_samp && !is.null(inputs$prepR_untyped)) {
				ps_in_mp_samp <- which(mp_eff_samp %in% PSHIFT_NAMES)
				if (length(ps_in_mp_samp) > 0) {
					rs_a1    <- inputs$prepR_untyped[, 1]
					rs_a2    <- inputs$prepR_untyped[, 2]
					M_out_ps <- stop0 - start0 + 1L
					sbt      <- stats_by_type[[ci]]
					idx1     <- sample_map + 1L   # [M x samp_num], 1-based into D_untyped
					for (ei in ps_in_mp_samp) {
						pshift_num   <- PSHIFT_TYPE_NUM[mp_eff_samp[ei]]
						correct_vals <- compute_pshift_type_c_tie(
							pshift_num, edgelist, edgelist[, 4], type_id_c,
							rs_a1, rs_a2, M_out_ps, start0, stop0
						)
						sbt[, , ei] <- matrix(
							correct_vals[cbind(rep(seq_len(M), samp_num), as.vector(idx1))],
							nrow = M, ncol = samp_num
						)
					}
					stats_by_type[[ci]] <- sbt
				}
			}
		}

		# Apply variable names to all per-type results.
		# When needs_mp_samp: stats_by_type[[ci]] has only mp_eff_samp effects.
		# Run one additional unmasked pass (all effects, full weights) then merge:
		# "ignore" effects come from the unmasked pass, "separate"/"interact" from
		# the type-c masked passes.
		if (needs_mp_samp) {
			# Unmasked pass using type-1 sample_map (smap for ci=1)
			smap_unmasked <- matrix(NA_integer_, nrow = M, ncol = samp_num)
			for (m in seq_len(M)) {
				for (s in seq_len(samp_num)) {
					d <- sample_map[m, s] + 1L
					smap_unmasked[m, s] <- typed_row_map[[d]][1L] - 1L
				}
			}
			storage.mode(smap_unmasked) <- "integer"
			full_stats <- compute_stats_tie_sampled(
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
				consider_type = consider_type_cpp,
				start = start0,
				stop = stop0,
				directed = reh$meta$directed,
				display_progress = display_progress,
				method = method,
				sample_map = smap_unmasked
			)
			# Merge: for each type-c pass, replace mp_eff_samp slices with
			# the type-c filtered values; keep all other slices from full_stats
			for (ci in seq_len(C)) {
				sbt <- full_stats
				mp_arr <- stats_by_type[[ci]]
				for (ei in seq_along(mp_eff_samp)) {
					en <- mp_eff_samp[ei]
					p_full <- which(effectNames == en)
					if (length(p_full) > 0 && ei <= dim(mp_arr)[3]) {
						sbt[, , p_full] <- mp_arr[, , ei]
					}
				}
				stats_by_type[[ci]] <- add_variable_names(
					sbt, effectNames, inputs$effects, interactions)
			}
		} else {
			stats_by_type <- lapply(stats_by_type, function(s)
				add_variable_names(s, effectNames, inputs$effects, interactions))
		}
		statistics <- stats_by_type[[1]]

	} else {
		sample_map_cpp <- sample_map

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
			consider_type = consider_type_cpp,
			start = start0,
			stop = stop0,
			directed = reh$meta$directed,
			display_progress = display_progress,
			method = method,
			sample_map = sample_map_cpp
		)
		statistics <- add_variable_names(statistics, effectNames, inputs$effects, interactions)

		# Multi-pass for "separate"/"interact" with ext=TRUE
		if (C > 1 && any(consider_type[seq_along(inputs$effects)] %in% c(1L, 2L))) {
			mp_idx_ext       <- which(consider_type[seq_along(inputs$effects)] %in% c(1L, 2L))
			mp_eff_ext       <- effectNames[mp_idx_ext]
			mp_scal_ext      <- scaling[mp_idx_ext]
			mp_ct_ext        <- rep(FALSE, length(mp_eff_ext))
			mp_consider_type_ext <- consider_type[mp_idx_ext]  # 1L=separate, 2L=interact
			stats_by_type <- vector("list", C)
			for (ci in seq_len(C)) {
				type_id_c <- types$typeID[ci]
				wt_c <- weights * (edgelist[, 4] == type_id_c)
				stats_c <- compute_stats_tie_sampled(
					effects = mp_eff_ext,
					edgelist = edgelist,
					weights = wt_c,
					riskset = riskset,
					risksetMatrix = risksetMatrix,
					covariates = covar,
					interactions = interactions,
					memory = memory,
					memory_value = memory_value,
					scaling = mp_scal_ext,
					consider_type = mp_ct_ext,
					start = start0,
					stop = stop0,
					directed = reh$meta$directed,
					display_progress = display_progress,
					method = method,
					sample_map = sample_map_cpp
				)
				dimnames(stats_c)[[3]] <- mp_eff_ext
				# Pshift override for ext=TRUE + interact only.
				# ext=TRUE + separate: base C++ stat is correct (all type slices
				# share the same dyad pattern — defensible for separate semantics).
				ps_in_ext <- which(mp_eff_ext %in% PSHIFT_NAMES &
				                   mp_consider_type_ext == 2L)
				if (length(ps_in_ext) > 0) {
					rs_a1    <- riskset[, 1]
					rs_a2    <- riskset[, 2]
					M_out_ps <- stop0 - start0 + 1L
					S        <- ncol(sample_map_cpp)
					idx1     <- sample_map_cpp + 1L
					for (ei in ps_in_ext) {
						pshift_num   <- PSHIFT_TYPE_NUM[mp_eff_ext[ei]]
						correct_vals <- compute_pshift_type_c_tie(
							pshift_num, edgelist, edgelist[, 4], type_id_c,
							rs_a1, rs_a2, M_out_ps, start0, stop0
						)
						stats_c[, , ei] <- matrix(
							correct_vals[cbind(rep(seq_len(M_out_ps), S), as.vector(idx1))],
							nrow = M_out_ps, ncol = S
						)
					}
				}
				stats_by_type[[ci]] <- stats_c
			}
		}
	}

	# ---- post-processing: type splitting ----
	#
	# For ext=FALSE (sample_untyped=TRUE): assemble type-split slices from
	# stats_by_type. For consider_type=TRUE effects, slice ci gets its value
	# from stats_by_type[[ci]], zeroed where type ci doesn't exist for the dyad.
	# For consider_type=FALSE and non-typed effects, all C calls return the same
	# aggregated value so we just use the first call.
	#
	# For ext=TRUE (with_type_riskset=TRUE): split_type_slices as before.
	if (needs_typed_riskset && C > 1) {
		if (sample_untyped) {
			statistics <- assemble_type_slices_sampled(
				stats_by_type = stats_by_type,
				effects       = inputs$effects,
				consider_type = consider_type,
				types         = types,
				sample_map    = sample_map,       # 0-based untyped indices
				type_exists   = type_exists
			)
		} else if (with_type_riskset) {
			statistics <- split_type_slices_sampled(
				statistics    = statistics,
				effects       = inputs$effects,
				consider_type = consider_type,
				types         = types,
				riskset       = riskset,
				sample_map    = sample_map + 1L,  # 1-based row index into riskset
				stats_by_type = if (exists("stats_by_type")) stats_by_type else NULL
			)
		}
	}

	# base riskset output: untyped when ext=FALSE, typed otherwise
	if (!with_type_riskset && needs_typed_riskset) {
		riskset_out <- build_untyped_riskset(reh, actors, types)
	} else {
		riskset_out <- modify_riskset(riskset, reh, actors, types)
	}
	
	class(statistics) <- c("tomstats_sampled", "remstats")
	attr(statistics, "model")           <- "tie"
	attr(statistics, "formula")         <- form
	attr(statistics, "riskset")         <- riskset_out
	attr(statistics, "sample_map")      <- sample_map + 1
	attr(statistics, "samp_num")        <- samp_num
	attr(statistics, "events_mult")     <- case_mult
	attr(statistics, "subset")          <- data.frame(start = start0 + 1L, stop = stop0 + 1L)
	attr(statistics, "method")          <- method
	attr(statistics, "dyad_keys")       <- key_to_base
	attr(statistics, "case_pos")        <- case_pos
	attr(statistics, "samp_prob")       <- pi
	attr(statistics, "log_samp_prob")   <- log_pi
	attr(statistics, "seed")            <- seed
	attr(statistics, "sampling_scheme") <- "uniform_wor_static_riskset"
	
	return(statistics)
	}
}

