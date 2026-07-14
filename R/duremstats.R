# ─────────────────────────────────────────────────────────────────────────────
# duremstats.R
# Active-state statistics for Duration Relational Event Models
#
# These statistics capture properties of the currently active event network
# at each time point — events that have started but not yet ended.
# Unlike the history-weighted statistics computed via tomstats (which can be
# made duration-aware through psi-weighting), active-state statistics require
# explicit tracking of which events are currently open and cannot be derived
# from weighted event history alone.
#
# Entry point: called internally by .remstats_durem_dispatch() when the
#   formula contains active-state effects.
# Returns:     a list with $start_stats and $end_stats (same shape as
#              remstats_durem, so they can be combined at estimation time)
# ─────────────────────────────────────────────────────────────────────────────


# ── Effect name → stat_type integer ──────────────────────────────────────────

# Sentinel values >= 96L flag R-level post-processing (no direct C++ stat_type):
#   98L = activeReciprocalTie  : look up reversed dyad from activeTie (stat_type 1)
#   97L = activeTotaldegreeDyad: sum of activeTotaldegreeSender + activeTotaldegreeReceiver
#   (undirected)
#   98L = activeDegreeMin      : min(stat_type 2, stat_type 3)
#   97L = activeDegreeMax      : max(stat_type 2, stat_type 3)
#   96L = activeDegreeDyad     : stat_type 2 + stat_type 3
.durem_stat_type_directed <- c(
	activeTie                   = 1L,
	activeOutdegreeSender       = 2L,
	activeIndegreeReceiver      = 3L,
	activeTotaldegreeSender     = 4L,
	activeTotaldegreeReceiver   = 5L,
	activeSharedPartners_otp    = 6L,
	activeSharedPartners_itp    = 7L,
	activeSharedPartners_osp    = 8L,
	activeSharedPartners_isp    = 9L,
	activeReciprocalTie         = 98L,   # R-derived: activeTie on reversed dyad
	activeTotaldegreeDyad       = 97L    # R-derived: totaldegreeSender + Receiver
)

.durem_stat_type_undirected <- c(
	activeTie                   = 1L,
	activeDegreeMin             = 98L,   # R-derived: min(stat_type 2, stat_type 3)
	activeDegreeMax             = 97L,   # R-derived: max(stat_type 2, stat_type 3)
	activeDegreeDyad            = 96L,   # R-derived: stat_type 2 + stat_type 3
	activeSharedPartners        = 4L
)


# ── Internal helper: encode dual edgelist for C++ ────────────────────────────

#' Prepare the dual edgelist for \code{calculate_active_stats}
#'
#' Converts actor names to 0-based integer IDs and status to 0/1.
#' Remify stores actor IDs as 1-based integers; C++ expects 0-based.
#'
#' @param edgelist_dual  The \code{$edgelist_dual} data.frame from a
#'   \code{remify_durem} object.
#' @param actor_ids      Named integer vector mapping actor name → 0-based ID.
#' @return A numeric matrix with columns [time, actor1_id, actor2_id, status].
#' @keywords internal
.prepare_dual_edgelist <- function(edgelist_dual, actor_ids) {
	matrix(
		c(edgelist_dual$time,
			actor_ids[edgelist_dual$actor1],
			actor_ids[edgelist_dual$actor2],
			ifelse(edgelist_dual$status == "start", 0L, 1L)),
		ncol = 4L,
		dimnames = list(NULL, c("time", "actor1", "actor2", "status"))
	)
}


#' Build the riskset matrix for active-state stats
#'
#' Uses the exported C++ helpers \code{get_riskset} and
#' \code{convert_to_risksetMatrix} with 0-based actor IDs, matching the
#' convention used internally by \code{tomstats}.
#'
#' @param N        Number of actors.
#' @param directed Logical.
#' @param C        Number of event types (default 1).
#' @return Numeric matrix of dyad IDs (0-based, -999 for absent dyads).
#'   Dimensions N × N when C = 1; N*C × N*C when C > 1.
#' @keywords internal
.build_riskset_matrix <- function(N, directed, C = 1L) {
	actor_ids_0 <- as.integer(seq_len(N) - 1L)
	type_ids    <- as.integer(seq_len(C) - 1L)
	riskset     <- get_riskset(actor_ids_0, type_ids, directed)
	convert_to_risksetMatrix(riskset, N = N, C = C)
}


# ── Sentinel helpers ──────────────────────────────────────────────────────────

# Return the pair of real C++ stat_types needed to compute a sentinel effect.
# Returns a list of two integers, or a single integer if only one call needed.
#
# Directed sentinels:
#   98L = activeReciprocalTie  : needs stat_type 1 (activeTie)
#   97L = activeTotaldegreeDyad: needs stat_type 4 + stat_type 5
# Undirected sentinels:
#   98L = activeDegreeMin      : needs stat_type 2 + stat_type 3
#   97L = activeDegreeMax      : needs stat_type 2 + stat_type 3
#   96L = activeDegreeDyad     : needs stat_type 2 + stat_type 3
.sentinel_base_stypes <- function(stype, directed) {
	if (directed) {
		if (stype == 98L) return(1L)          # activeReciprocalTie: one call
		if (stype == 97L) return(list(4L, 5L)) # activeTotaldegreeDyad: two calls
	} else {
		if (stype %in% c(98L, 97L, 96L)) return(list(2L, 3L))
	}
	stop("Unknown sentinel stat_type: ", stype, call. = FALSE)
}

# Given a cached result (matrix or list with $a/$b) and a sentinel stype,
# return the final [M × D] matrix.
.apply_sentinel <- function(cached, stype, directed, riskset_mat, rev_lookup) {
	if (directed && stype == 98L) {
		# activeReciprocalTie: permute columns of activeTie matrix
		mat <- if (is.matrix(cached)) cached else cached
		out <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
		for (d in seq_len(ncol(mat))) {
			rd <- rev_lookup[d]
			if (!is.null(rd) && length(rd) == 1L && rd > 0L)
				out[, d] <- mat[, rd]
		}
		return(out)
	}
	# Two-component sentinels
	a <- cached$a; b <- cached$b
	if (directed && stype == 97L) return(a + b)  # activeTotaldegreeDyad
	if (!directed) {
		if (stype == 98L) return(pmin(a, b))       # activeDegreeMin
		if (stype == 97L) return(pmax(a, b))       # activeDegreeMax
		if (stype == 96L) return(a + b)            # activeDegreeDyad
	}
	stop("Unknown sentinel: ", stype, call. = FALSE)
}

# Compute the [M × D] matrix for a sentinel effect from one C++ batch.
# full_timeline: if TRUE the C++ call covers the full type-filtered edgelist
#   and the caller will forward-fill; if FALSE start/stop are used directly
#   and the extra trailing row is dropped.
.compute_sentinel <- function(stype, directed, edgelist_mat, riskset_mat,
                               start_0, stop_0, M, rev_lookup,
                               full_timeline = FALSE) {
	base <- .sentinel_base_stypes(stype, directed)

	run_one <- function(st) {
		mat <- calculate_active_stats(
			edgelist         = edgelist_mat,
			risksetMatrix    = riskset_mat,
			stat_type        = st,
			directed         = directed,
			start            = as.integer(start_0),
			stop             = as.integer(stop_0),
			display_progress = FALSE
		)
		if (full_timeline) mat else mat[seq_len(M), , drop = FALSE]
	}

	if (is.list(base)) {
		cached <- list(a = run_one(base[[1L]]), b = run_one(base[[2L]]),
		               sentinel = stype)
	} else {
		cached <- run_one(base)
	}
	.apply_sentinel(cached, stype, directed, riskset_mat, rev_lookup)
}




#' Prepend a baseline column of 1s to a 3-D stats array
#'
#' Checks whether the formula includes an intercept (no \code{-1}).
#' If so, prepends a \code{baseline{suffix}} slice of 1s along dim 3.
#'
#' @param arr    3-D array \[M × D × P\] or \code{NULL}.
#' @param formula Formula that produced the array.
#' @param suffix  \code{".start"} or \code{".end"}.
#' @return Updated array with P+1 slices, or the original if no intercept.
#' @keywords internal
.maybe_prepend_baseline <- function(arr, formula, suffix) {
	if (is.null(arr) || is.null(formula)) return(arr)
	has_intercept <- attr(terms(formula), "intercept") == 1L
	if (!has_intercept) return(arr)
	d   <- dim(arr)
	bl  <- array(1, dim = c(d[1L], d[2L], 1L),
							 dimnames = list(NULL, NULL, paste0("baseline", suffix)))
	out <- array(0, dim = c(d[1L], d[2L], d[3L] + 1L),
							 dimnames = list(NULL, NULL,
							 								c(dimnames(bl)[[3L]], dimnames(arr)[[3L]])))
	out[, , 1L]              <- bl
	out[, , 2L:(d[3L] + 1L)] <- arr
	out
}


# ── Argument helpers ─────────────────────────────────────────────────────────

#' Normalise and validate consider_type for active-state effects
#'
#' Accepts TRUE/FALSE aliases: TRUE -> "separate", FALSE -> "ignore".
#' @return The normalised character value.
#' @keywords internal
.validate_consider_type_durem <- function(consider_type) {
	if (isTRUE(consider_type))  consider_type <- "separate"
	if (isFALSE(consider_type)) consider_type <- "ignore"
	valid <- c("ignore", "separate", "interact")
	if (!consider_type %in% valid)
		stop(
			"`consider_type = \"", consider_type, "\"` is not supported for ",
			"active-state statistics.\n",
			"Valid values: \"ignore\", \"separate\", \"interact\" ",
			"(or TRUE / FALSE as aliases for \"separate\" / \"ignore\").",
			call. = FALSE
		)
	consider_type
}


# ── Parse formula into effect configs ────────────────────────────────────────

#' Evaluate terms in a \code{duremstats} formula into effect config lists
#'
#' @param formula  Formula such as
#'   \code{~ activeTie() + activeOutdegreeSender(scaling = "std")}
#' @return A list of named lists, each with \code{$effect}, \code{$scaling},
#'   and \code{$consider_type}.
#' @keywords internal
.parse_active_effects <- function(formula) {
	if (is.null(formula)) return(list())
	tt     <- terms(formula)
	labels <- attr(tt, "term.labels")
	lapply(labels, function(lbl) {
		fname <- sub("[(].*$", "", lbl)
		# Distinguish a genuinely unknown effect (the stub function does not
		# exist) from a stub that exists but errors while evaluating (e.g. a
		# missing internal helper). The old handler blamed the effect name for
		# ANY "could not find function" error, masking the real cause.
		if (!exists(fname, mode = "function")) {
			stop("Unknown active-state effect '", fname, "'. ",
					 "See ?active_effects for available effects.", call. = FALSE)
		}
		tryCatch(
			eval(parse(text = lbl)),
			error = function(e)
				stop("Could not evaluate active-state effect term '", lbl,
						 "': ", conditionMessage(e), call. = FALSE)
		)
	})
}


# ── Compute active-state stats for one sub-model ─────────────────────────────

#' Compute active-state statistics for a single sub-model
#'
#' @param effect_configs A list of effect config lists, each with \code{$effect},
#'   \code{$scaling}, and \code{$consider_type} (output of
#'   \code{.parse_active_effects}).
#' @param reh        A \code{remify_durem} object.
#' @param directed   Logical. Whether the sub-model is directed.
#' @param start      First time-point index (0-based).
#' @param stop       Last  time-point index (0-based).
#' @param suffix     ".start" or ".end".
#' @param display_progress Logical.
#' @return 3-D array \[M × D × P\] or \code{NULL} if no effects.
#' @keywords internal
.compute_active_stats <- function(effect_configs, reh, directed,
																	start, stop, suffix,
																	display_progress) {
	if (length(effect_configs) == 0L) return(NULL)
	
	stat_map <- if (directed) .durem_stat_type_directed
	else           .durem_stat_type_undirected
	
	# Extract names and validate
	eff_names <- vapply(effect_configs, `[[`, character(1L), "effect")
	unknown   <- setdiff(eff_names, names(stat_map))
	if (length(unknown) > 0L)
		stop("Unknown active-state effect(s): ",
				 paste(unknown, collapse = ", "), ".\n",
				 "Available: ", paste(names(stat_map), collapse = ", "),
				 call. = FALSE)
	
	# Validate consider_type
	for (cfg in effect_configs)
		.validate_consider_type_durem(cfg$consider_type)
	
	# ── Warn if activeTie appears in end_effects ─────────────────────────────
	if (suffix == ".end" && any(eff_names == "activeTie"))
		warning(
			"'activeTie' in end_effects is always 1 for all risk-set dyads ",
			"(all end-risk dyads are active by definition). ",
			"It carries no information as a predictor and will produce a ",
			"perfectly collinear column. Consider removing it.",
			call. = FALSE
		)

	# Build actor ID lookup (0-based).
	# reh$meta$dictionary$actors$actorID is 1-based; subtract 1 for C++.
	actor_dict  <- reh$meta$dictionary$actors
	actor_ids   <- setNames(actor_dict$actorID - 1L, actor_dict$actorName)
	N           <- reh$N
	
	# Number of event types.
	# Use C > 1 (from reh) only when extend_riskset_by_type = TRUE, so that
	# D in the output array is consistent with reh$D for "ignore" effects.
	# For "separate" effects we always use C = 1 per per-type call (see below).
	has_types    <- isTRUE(reh$meta$with_type)
	type_riskset <- isTRUE(reh$meta$with_type_riskset)
	C_reh        <- if (!is.null(reh$C)) reh$C else 1L
	
	# For "ignore" effects respect the typed riskset when requested.
	C_ignore     <- if (type_riskset) C_reh else 1L
	
	# Riskset matrix and D for "ignore" effects
	riskset_mat_ignore <- .build_riskset_matrix(N, directed, C_ignore)
	D_ignore <- as.integer(max(riskset_mat_ignore[riskset_mat_ignore >= 0]) + 1L)
	
	# Riskset matrix for "separate" per-type calls (always C = 1)
	riskset_mat_sep <- if (C_ignore == 1L) riskset_mat_ignore
	else .build_riskset_matrix(N, directed, 1L)
	D_sep <- as.integer(max(riskset_mat_sep[riskset_mat_sep >= 0]) + 1L)
	
	# Dual edgelist encoded for C++ (full, for "ignore" effects)
	ed_mat_full <- .prepare_dual_edgelist(reh$edgelist_dual, actor_ids)
	
	# Per-type encoded edgelists (for "separate" and "interact" effects)
	type_levels <- if (has_types) sort(unique(reh$edgelist$type)) else character(0L)
	
	needs_per_type <- has_types && any(vapply(effect_configs,
																						function(cfg) cfg$consider_type %in% c("separate", "interact"),
																						logical(1L)))
	
	ed_mat_per_type <- if (needs_per_type) {
		lapply(type_levels, function(tc) {
			ed_tc <- reh$edgelist_dual[reh$edgelist_dual$type == tc, ]
			.prepare_dual_edgelist(ed_tc, actor_ids)
		})
	} else NULL
	
	# D for "interact": one D_sep-wide block per type, concatenated
	D_interact <- as.integer(D_sep * C_reh)
	
	# Number of output time points (relative to the full dual edgelist)
	M <- as.integer(stop - start + 1L)
	
	# Full timeline — used for forward-filling per-type results
	full_utimes <- sort(unique(reh$edgelist_dual$time))
	
	# Helper: expand a per-type C++ result (n_tc rows) into M rows.
	#
	# C++ computes the active state BEFORE each unique time in the type-tc
	# edgelist:
	#   mat_tc[k, ] = active state before tc_utimes[k]
	#
	# For each time t in the full output window:
	#   - If t IS a tc time point (t == tc_utimes[k]): use row k — the state
	#     before that tc event is the correct "before t" statistic.
	#   - If t is BETWEEN tc events (tc_utimes[k] < t < tc_utimes[k+1]): use
	#     row k+1 — the state "before tc_utimes[k+1]" already incorporates
	#     all tc events up through tc_utimes[k], which is exactly the active
	#     state at time t.
	#   - If t is before any tc event: the active state is empty (all zeros).
	.expand_to_M <- function(mat_tc, tc_edgelist_mat) {
		tc_utimes  <- sort(unique(tc_edgelist_mat[, "time"]))
		n_tc       <- length(tc_utimes)
		segment    <- full_utimes[(start + 1L):(stop + 1L)]  # 1-based R indexing
		k          <- findInterval(segment, tc_utimes)        # last tc_time <= t
		is_tc_time <- segment %in% tc_utimes
		# tc times: row k; between tc times: row k+1 (capped at n_tc)
		row_map 	 <- ifelse(is_tc_time, k, pmin(k + 1L, n_tc + 1L))
		mat_full   <- matrix(0, nrow = M, ncol = ncol(mat_tc))
		nz         <- row_map > 0L
		if (any(nz))
			mat_full[nz, ] <- mat_tc[row_map[nz], , drop = FALSE]
		mat_full
	}
	
	# ── Expand effect configs ──────────────────────────────────────────────────
	# Each entry carries:
	#   $type_label  — name suffix for dimnames
	#   $event_type  — filter active ties by this event type (interact only)
	#   $dyad_type   — column block index in the D*C output (interact only)
	expanded <- list()
	for (cfg in effect_configs) {
		if (cfg$consider_type == "separate" && has_types && length(type_levels) > 0L) {
			# "separate": one output stat per type
			for (tc in type_levels) {
				e            <- cfg
				e$type_label <- tc
				expanded     <- c(expanded, list(e))
			}
		} else if (cfg$consider_type == "interact" && (!has_types || length(type_levels) == 0L)) {
			# "interact" on untyped data: degrade silently to "ignore"
			cfg$consider_type <- "ignore"
			cfg$type_label    <- NA_character_
			expanded          <- c(expanded, list(cfg))
		} else if (cfg$consider_type == "interact" && has_types && !type_riskset) {
			# "interact" requires extend_riskset_by_type = TRUE; coerce to
			# "separate" (same behavior as tomstats for non-duration models)
			warning("\"interact\" requires extend_riskset_by_type = TRUE; ",
							"coercing to \"separate\".", call. = FALSE)
			cfg$consider_type <- "separate"
			for (tc in type_levels) {
				e            <- cfg
				e$type_label <- tc
				expanded     <- c(expanded, list(e))
			}
		} else if (cfg$consider_type == "interact" && has_types && type_riskset) {
			# "interact" with extended riskset: C×C entries
			# Convention follows tomstats: stat.eventType.dyadType
			for (et in type_levels) {
				for (dt in type_levels) {
					e            <- cfg
					e$type_label <- paste0(et, ".", dt)
					e$event_type <- et    # filter active ties by this type
					e$dyad_type  <- dt    # place result in this column block
					expanded     <- c(expanded, list(e))
				}
			}
		} else {
			# "ignore": single output stat
			cfg$type_label <- NA_character_
			expanded       <- c(expanded, list(cfg))
		}
	}
	
	# Output dimension names
	out_names <- vapply(expanded, function(e) {
		base <- e$effect
		if (!is.na(e$type_label)) paste0(base, ".", e$type_label) else base
	}, character(1L))
	
	# Choose D for each expanded effect
	# When type_riskset=TRUE, "separate" also uses D_interact (the 6-col result
	# is replicated across all C type blocks, matching tomstats behavior).
	out_D <- vapply(expanded, function(e) {
		if (e$consider_type == "interact")               D_interact
		else if (!is.na(e$type_label) && type_riskset)   D_interact
		else if (!is.na(e$type_label))                   D_sep
		else                                             D_ignore
	}, integer(1L))
	
	# All effects should agree on D; mixing consider_type values with different
	# D dimensions (e.g. "ignore" + "interact") in one formula is unsupported.
	D_out <- out_D[1L]
	if (length(unique(out_D)) > 1L)
		warning("Effects in this formula produce different D dimensions ",
						"(mixing \"ignore\", \"separate\", and/or \"interact\" with ",
						"typed risksets). Output array uses D = ", D_out,
						" from the first effect; other effects may be misaligned.",
						call. = FALSE)
	
	# ── Allocate output array [M × D × P_expanded] ───────────────────────────
	P_out <- length(expanded)
	out   <- array(0, dim      = c(M, D_out, P_out),
								 dimnames = list(NULL, NULL, paste0(out_names, suffix)))
	
	# ── Pre-compute per-type active stats for interact effects ──────────────
	# Cache keyed by (effect, type) to avoid redundant C++ calls across C×C
	# entries that share the same event_type.
	interact_cache <- list()
	
	# ── Helper: run one C++ call and return [M × D] ───────────────────────────
	# For "ignore": uses the full edgelist and ignore riskset, drops extra row.
	# For "separate"/"interact": caller passes type-filtered edgelist and untyped
	# riskset; result is forward-filled.
	.call_cpp <- function(stype, edgelist_mat, riskset_mat, start_0, stop_0,
	                      full_timeline = FALSE) {
		n_times <- length(unique(edgelist_mat[, "time"]))
		mat <- calculate_active_stats(
			edgelist         = edgelist_mat,
			risksetMatrix    = riskset_mat,
			stat_type        = stype,
			directed         = directed,
			start            = as.integer(start_0),
			stop             = as.integer(stop_0),
			display_progress = display_progress
		)
		if (full_timeline) mat else mat[seq_len(M), , drop = FALSE]
	}

	# ── Pre-build reverse-dyad lookup for activeReciprocalTie (directed) ──────
	# For each column d (dyad i→j in riskset_mat_ignore), find the column for
	# j→i. -1L means no reverse dyad in the riskset (asymmetric risksets).
	if (directed && any(eff_names == "activeReciprocalTie")) {
		nr <- nrow(riskset_mat_ignore)
		rev_lookup <- integer(D_ignore)
		for (i in seq_len(nr)) {
			for (j in seq_len(nr)) {
				if (i == j) next
				d_fwd <- riskset_mat_ignore[i, j]
				d_rev <- riskset_mat_ignore[j, i]
				if (d_fwd >= 0L && d_rev >= 0L)
					rev_lookup[d_fwd + 1L] <- d_rev + 1L   # 1-based
				else if (d_fwd >= 0L)
					rev_lookup[d_fwd + 1L] <- -1L
			}
		}
	}

	# ── Per-effect computation ────────────────────────────────────────────────
	for (p_idx in seq_along(expanded)) {
		e     <- expanded[[p_idx]]
		eff   <- e$effect
		stype <- stat_map[[eff]]
		
		if (display_progress)
			message("Calculating active-state statistic: ", eff,
							if (!is.na(e$type_label)) paste0(" [type = ", e$type_label, "]") else "")
		
		# ── Dispatch by consider_type / sentinel ────────────────────────────────
		if (e$consider_type == "interact" && !is.null(e$event_type)) {
			# C×C interact: compute per event_type (cached), place in dyad_type block
			cache_key <- paste0(eff, "::", e$event_type)
			if (is.null(interact_cache[[cache_key]])) {
				et_idx  <- match(e$event_type, type_levels)
				ed_tc   <- ed_mat_per_type[[et_idx]]
				n_tc    <- length(unique(ed_tc[, "time"]))
				raw_stype <- if (stype >= 96L) .sentinel_base_stypes(stype, directed) else stype
				if (is.list(raw_stype)) {
					# Sentinel needing two calls — cache both components
					mat_a <- .expand_to_M(
						.call_cpp(raw_stype[[1L]], ed_tc, riskset_mat_sep, 0L, n_tc - 1L, TRUE),
						ed_tc)
					mat_b <- .expand_to_M(
						.call_cpp(raw_stype[[2L]], ed_tc, riskset_mat_sep, 0L, n_tc - 1L, TRUE),
						ed_tc)
					interact_cache[[cache_key]] <- list(a = mat_a, b = mat_b,
					                                   sentinel = stype)
				} else {
					mat_tc <- .call_cpp(raw_stype, ed_tc, riskset_mat_sep, 0L, n_tc - 1L, TRUE)
					interact_cache[[cache_key]] <- .expand_to_M(mat_tc, ed_tc)
				}
			}
			cached <- interact_cache[[cache_key]]
			et_mat <- if (stype >= 96L)
				.apply_sentinel(cached, stype, directed, riskset_mat_sep, rev_lookup = NULL)
			else
				cached
			dt_idx <- match(e$dyad_type, type_levels)
			mat    <- matrix(0, nrow = M, ncol = D_interact)
			col_start <- (dt_idx - 1L) * D_sep + 1L
			col_end   <- dt_idx * D_sep
			mat[, col_start:col_end] <- et_mat

		} else if (is.na(e$type_label)) {
			# "ignore": full edgelist, possibly typed riskset
			if (stype >= 96L) {
				mat <- .compute_sentinel(stype, directed,
				                         ed_mat_full, riskset_mat_ignore,
				                         as.integer(start), as.integer(stop),
				                         M, rev_lookup,
				                         full_timeline = FALSE)
			} else {
				mat <- .call_cpp(stype, ed_mat_full, riskset_mat_ignore,
				                 as.integer(start), as.integer(stop))
			}

		} else {
			# "separate": type-filtered edgelist, untyped riskset
			tc_idx <- match(e$type_label, type_levels)
			ed_tc  <- ed_mat_per_type[[tc_idx]]
			n_tc   <- length(unique(ed_tc[, "time"]))
			if (stype >= 96L) {
				mat_sep <- .compute_sentinel(stype, directed,
				                             ed_tc, riskset_mat_sep,
				                             0L, as.integer(n_tc - 1L),
				                             n_tc, rev_lookup,
				                             full_timeline = TRUE)
				mat_sep <- .expand_to_M(mat_sep, ed_tc)
			} else {
				mat_tc  <- .call_cpp(stype, ed_tc, riskset_mat_sep, 0L, n_tc - 1L, TRUE)
				mat_sep <- .expand_to_M(mat_tc, ed_tc)
			}
			if (type_riskset) {
				mat <- do.call(cbind, rep(list(mat_sep), C_reh))
			} else {
				mat <- mat_sep
			}
		}   # mat is [M × D_out]
		
		# ── Scaling ───────────────────────────────────────────────────────────
		if (identical(e$scaling, "std")) {
			for (m in seq_len(M)) {
				vals <- mat[m, ]
				mu   <- mean(vals)
				s    <- sd(vals)
				mat[m, ] <- if (s > 0) (vals - mu) / s else vals - mu
			}
		}
		
		out[, , p_idx] <- mat
	}
	
	out
}


# ── Public entry point ────────────────────────────────────────────────────────

#' Compute active-state statistics for a \code{remify_durem} object
#'
#' Computes statistics that capture the current state of the active event
#' network at each time point.  These complement the history-weighted statistics
#' returned by \code{\link{remstats}} and cannot be derived from weighted
#' event history alone.
#'
#' See \code{\link{active_effects}} for the full list of available effects
#' and their descriptions.
#'
#' @param reh A \code{remify_durem} object.
#' @param start_effects Formula of active-state effects for the start model,
#'   e.g. \code{~ activeTie() + activeOutdegreeSender()}.
#' @param end_effects Formula of active-state effects for the end model.
#' @param start Integer. Index of first time point to compute (default 2).
#' @param stop  Integer. Index of last  time point to compute (default Inf).
#' @param display_progress Logical. Show progress messages.
#' @return A list with \code{$start_stats} and \code{$end_stats}: 3-D arrays
#'   \[M x D x P\] with effect names suffixed \code{.start} / \code{.end},
#'   and \code{attr(., "reh")} set to \code{reh}.  The same shape as a
#'   \code{remstats_durem} object so the two can be combined at estimation time.
#' @keywords internal
duremstats <- function(reh,
											 start_effects    = NULL,
											 end_effects      = NULL,
											 start            = 2L,
											 stop             = Inf,
											 display_progress = FALSE) {
	
	if (!inherits(reh, "remify_durem"))
		stop("`reh` must be a `remify_durem` object.")
	
	# Unique time points in the dual edgelist
	ed    <- reh$edgelist_dual
	utimes <- sort(unique(ed$time))
	M_total <- length(utimes)
	
	start <- max(1L, as.integer(start)) - 1L           # convert to 0-based
	stop  <- if (is.infinite(stop)) M_total - 1L       # Inf → last time point
	else min(M_total - 1L, as.integer(stop) - 1L)
	
	directed_start <- isTRUE(reh$meta$directed)
	directed_end   <- isTRUE(reh$durem$dur_directed_end)
	
	eff_start <- .parse_active_effects(start_effects)
	eff_end   <- .parse_active_effects(end_effects)
	
	ss <- .compute_active_stats(eff_start, reh, directed_start,
															start, stop, ".start", display_progress)
	es <- .compute_active_stats(eff_end,   reh, directed_end,
															start, stop, ".end",   display_progress)
	
	# ── Prepend baseline (intercept) when formula has no -1 ───────────────────
	ss <- .maybe_prepend_baseline(ss, start_effects, ".start")
	es <- .maybe_prepend_baseline(es, end_effects,   ".end")
	
	out <- list(start_stats = ss, end_stats = es)
	attr(out, "reh") <- reh
	attr(out, "model") <- reh$meta$model
	attr(out, "subset") <- c(start+1,stop+1)
	
	class(out) <- c("remstats_durem", "remstats")   # same class → compatible with estimation
	out
}


# ── Effect constructor functions ──────────────────────────────────────────────
# Each function mirrors the remstats pattern: calling it returns a named list
# that the internal duremstats() function reads to configure the C++ computation
# and post-processing.
# They are designed for use inside formulas:
#   remstats(reh, start_effects = ~ activeTie() + activeOutdegreeSender("std"))
# but can also be called directly, e.g. to inspect the default configuration.

#' Active-state statistics for Duration Relational Event Models
#'
#' Constructor functions for active-state effects used inside formulas passed
#' to \code{\link{remstats}} (when \code{reh} is a \code{remify_durem} object).
#' They capture properties of the currently
#' \emph{active} event network — events that have started but not yet ended —
#' at each time point in a duration relational event sequence.
#'
#' Each function returns a configuration list consumed internally.
#' The functions are passed inside a formula:
#' \preformatted{
#'   remstats(reh,
#'     start_effects = ~ activeTie() + activeOutdegreeSender(scaling = "std"),
#'     end_effects   = ~ activeOutdegreeSender())
#' }
#'
#' \strong{Directed-network effects:}
#' \describe{
#'   \item{\code{activeTie()}}{
#'     Whether there is currently an active event from actor \eqn{i} to actor
#'     \eqn{j} (binary, 0/1).
#'     \emph{Note:} in \code{end_effects} this is always 1 by definition
#'     (only currently active dyads are at risk of ending) and should not
#'     be included as a predictor.}
#'   \item{\code{activeReciprocalTie()}}{
#'     Whether there is currently an active event from actor \eqn{j} to actor
#'     \eqn{i} (binary, 0/1). Captures reciprocity in the active network.}
#'   \item{\code{activeOutdegreeSender()}}{
#'     Number of currently active events in which actor \eqn{i} (sender) is
#'     involved as sender (out-degree in the active-event network).}
#'   \item{\code{activeIndegreeReceiver()}}{
#'     Number of currently active events in which actor \eqn{j} (receiver) is
#'     involved as receiver.}
#'   \item{\code{activeTotaldegreeSender()}}{
#'     Total active degree of actor \eqn{i}: active events in which \eqn{i}
#'     appears as either sender or receiver.}
#'   \item{\code{activeTotaldegreeReceiver()}}{
#'     Total active degree of actor \eqn{j}.}
#'   \item{\code{activeTotaldegreeDyad()}}{
#'     Sum of the total active degrees of actors \eqn{i} and \eqn{j}:
#'     \eqn{\deg(i) + \deg(j)}.}
#'   \item{\code{activeSharedPartners_otp()}}{
#'     \emph{(Advanced)} Number of actors \eqn{h} for whom \eqn{i \to h} and
#'     \eqn{h \to j} are both currently active (outgoing two-path).
#'     Rarely informative when the active network is sparse.}
#'   \item{\code{activeSharedPartners_itp()}}{
#'     \emph{(Advanced)} Incoming two-path: actors \eqn{h} with \eqn{h \to i}
#'     and \eqn{j \to h} both active.}
#'   \item{\code{activeSharedPartners_osp()}}{
#'     \emph{(Advanced)} Outgoing shared partners: actors \eqn{h} with
#'     \eqn{i \to h} and \eqn{j \to h} both active.}
#'   \item{\code{activeSharedPartners_isp()}}{
#'     \emph{(Advanced)} Incoming shared partners: actors \eqn{h} with
#'     \eqn{h \to i} and \eqn{h \to j} both active.}
#' }
#'
#' \strong{Undirected-network effects:}
#' \describe{
#'   \item{\code{activeTie()}}{
#'     Whether there is currently an active event between actors \eqn{i} and
#'     \eqn{j}.
#'     \emph{Note:} in \code{end_effects} this is always 1 by definition
#'     and should not be included as a predictor.}
#'   \item{\code{activeDegreeMin()}}{
#'     Minimum of the active degrees of \eqn{i} and \eqn{j}:
#'     \eqn{\min(\deg(i), \deg(j))}.}
#'   \item{\code{activeDegreeMax()}}{
#'     Maximum of the active degrees of \eqn{i} and \eqn{j}:
#'     \eqn{\max(\deg(i), \deg(j))}.}
#'   \item{\code{activeDegreeDyad()}}{
#'     Sum of the active degrees of \eqn{i} and \eqn{j}:
#'     \eqn{\deg(i) + \deg(j)}.}
#'   \item{\code{activeSharedPartners()}}{
#'     \emph{(Advanced)} Number of actors \eqn{h} for whom both \eqn{(i,h)}
#'     and \eqn{(j,h)} are currently active. Rarely informative when the
#'     active network is sparse.}
#' }
#'
#' @param scaling Scaling applied to the raw statistic before returning:
#'   \describe{
#'     \item{\code{"none"}}{Raw counts (default).}
#'     \item{\code{"std"}}{Per-time-point standardisation:
#'       \eqn{(x - \bar{x}) / \mathrm{sd}(x)}, computed over the \eqn{D}
#'       dyads in the fixed risk set.}
#'   }
#' @param consider_type Character (or logical). How event types are handled:
#'   \describe{
#'     \item{\code{"ignore"} or \code{FALSE}}{Aggregate over all event types
#'       (default). Counts all currently active events regardless of type.}
#'     \item{\code{"separate"} or \code{TRUE}}{Compute one statistic per event
#'       type. Only active events of that type contribute. Output effect names
#'       are suffixed with the type label, e.g.
#'       \code{activeOutdegreeSender.X.start}.}
#'     \item{\code{"interact"}}{Compute one statistic per (past-event type x
#'       dyad type) combination (\eqn{C^2} slices). Requires
#'       \code{extend_riskset_by_type = TRUE} in the \code{remify} call;
#'       otherwise silently coerced to \code{"separate"}.}
#'   }
#'
#' @return A named list with elements \code{effect}, \code{scaling}, and
#'   \code{consider_type}, consumed by \code{\link{duremstats}}.
#'
#' @seealso \code{\link{remstats}} for computing the statistics, and
#'   \code{\link{tie_effects}} / \code{\link{actor_effects}} for the standard
#'   (non-duration) effect overviews. Active-state effects are only available
#'   for the tie-oriented model of events with a duration.
#' @name active_effects
NULL

# ── Internal: build an active-effect config list ───────────────────
# Every exported active-effect stub (activeTie(), activeOutdegreeSender(), ...)
# delegates here. Returns the config consumed by .parse_active_effects ->
# .compute_active_stats: a named list with $effect, $scaling, $consider_type.
.active_effect_cfg <- function(effect, scaling, consider_type) {
	scaling <- match.arg(scaling, c("none", "std"))
	list(
		effect        = effect,
		scaling       = scaling,
		consider_type = .validate_consider_type_durem(consider_type)
	)
}

#' @rdname active_effects
#' @export
activeTie <- function(scaling = c("none", "std"),
											consider_type = "ignore")
	.active_effect_cfg("activeTie", scaling, consider_type)

#' @rdname active_effects
#' @export
activeReciprocalTie <- function(scaling = c("none", "std"),
                                consider_type = "ignore")
	.active_effect_cfg("activeReciprocalTie", scaling, consider_type)

#' @rdname active_effects
#' @export
activeOutdegreeSender <- function(scaling = c("none", "std"),
																	consider_type = "ignore")
	.active_effect_cfg("activeOutdegreeSender", scaling, consider_type)

#' @rdname active_effects
#' @export
activeIndegreeReceiver <- function(scaling = c("none", "std"),
																	 consider_type = "ignore")
	.active_effect_cfg("activeIndegreeReceiver", scaling, consider_type)

#' @rdname active_effects
#' @export
activeTotaldegreeSender <- function(scaling = c("none", "std"),
																		consider_type = "ignore")
	.active_effect_cfg("activeTotaldegreeSender", scaling, consider_type)

#' @rdname active_effects
#' @export
activeTotaldegreeReceiver <- function(scaling = c("none", "std"),
																			consider_type = "ignore")
	.active_effect_cfg("activeTotaldegreeReceiver", scaling, consider_type)

#' @rdname active_effects
#' @export
activeTotaldegreeDyad <- function(scaling = c("none", "std"),
                                  consider_type = "ignore")
	.active_effect_cfg("activeTotaldegreeDyad", scaling, consider_type)

#' @rdname active_effects
#' @export
activeSharedPartners_otp <- function(scaling = c("none", "std"),
																		 consider_type = "ignore")
	.active_effect_cfg("activeSharedPartners_otp", scaling, consider_type)

#' @rdname active_effects
#' @export
activeSharedPartners_itp <- function(scaling = c("none", "std"),
																		 consider_type = "ignore")
	.active_effect_cfg("activeSharedPartners_itp", scaling, consider_type)

#' @rdname active_effects
#' @export
activeSharedPartners_osp <- function(scaling = c("none", "std"),
																		 consider_type = "ignore")
	.active_effect_cfg("activeSharedPartners_osp", scaling, consider_type)

#' @rdname active_effects
#' @export
activeSharedPartners_isp <- function(scaling = c("none", "std"),
																		 consider_type = "ignore")
	.active_effect_cfg("activeSharedPartners_isp", scaling, consider_type)

#' @rdname active_effects
#' @export
activeDegreeMin <- function(scaling = c("none", "std"),
                            consider_type = "ignore")
	.active_effect_cfg("activeDegreeMin", scaling, consider_type)

#' @rdname active_effects
#' @export
activeDegreeMax <- function(scaling = c("none", "std"),
                            consider_type = "ignore")
	.active_effect_cfg("activeDegreeMax", scaling, consider_type)

#' @rdname active_effects
#' @export
activeDegreeDyad <- function(scaling = c("none", "std"),
                             consider_type = "ignore")
	.active_effect_cfg("activeDegreeDyad", scaling, consider_type)

#' @rdname active_effects
#' @export
activeSharedPartners <- function(scaling = c("none", "std"),
																 consider_type = "ignore")
	.active_effect_cfg("activeSharedPartners", scaling, consider_type)

