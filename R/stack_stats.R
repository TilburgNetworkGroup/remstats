#' Stack remstats for model fitting
#'
#' Stack a \code{tomstats} object into long format suitable for fitting with
#' standard R model functions. Each row corresponds to one dyad in the risk
#' set at one event time point.
#'
#' For interval timing (\code{ordinal = FALSE}), the stacked data can be
#' fitted with a Poisson GLM using \code{log_interevent} as an offset:
#' \code{glm(obs ~ -1 + . - time - dyad - log_interevent,
#'           offset = log_interevent, family = poisson, data = stacked$remstats_stack)}
#'
#' For ordinal timing (\code{ordinal = TRUE}), use conditional logistic
#' regression stratified by time point:
#' \code{survival::clogit(obs ~ -1 + . - time - dyad +
#'                        strata(time), data = stacked$remstats_stack)}
#'
#' @param stats A \code{tomstats} object (output of \code{remstats()} or
#'   \code{tomstats()}).
#' @param reh A \code{remify} object (output of \code{remify::remify2()}).
#' @param add_actors Logical (default \code{TRUE}). When \code{TRUE}, two
#'   extra columns \code{actor1} (sender label) and \code{actor2} (receiver
#'   label) are appended by looking up \code{reh$index$dyad_map_active} (or
#'   \code{reh$riskset_info$included} as a fallback).  Set to \code{FALSE} to
#'   suppress this lookup, e.g. when the riskset has not yet been resolved or
#'   for performance reasons.
#'
#' @return A list with elements:
#' \describe{
#'   \item{remstats_stack}{Data frame in long format with columns: \code{time}
#'     (time-point index matching \code{attr(stats, "subset")} sequence),
#'     all statistic columns, \code{log_interevent} (log
#'     inter-event time; interval timing only), \code{obs} (1 = observed event,
#'     0 = non-event), \code{dyad} (active dyad index 1..D), and — when
#'     \code{add_actors = TRUE} and the riskset is available — \code{actor1}
#'     (sender label) and \code{actor2} (receiver label).}
#'   \item{subset}{Integer vector of length 2: first and last event index.}
#'   \item{D}{Number of dyads in the risk set.}
#'   \item{E}{Number of events (time points).}
#'   \item{ordinal}{Logical: whether the ordinal likelihood applies.}
#' }
#'
#' @export
stack_stats <- function(stats, reh, add_actors = TRUE) {
  UseMethod("stack_stats")
}

# NULL-coalescing operator (defined locally if not already available)
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}

# ---------------------------------------------------------------------------
# Internal helper: return a two-column character data frame (actor1, actor2)
# with exactly D rows — one per active dyad, in active-dyad order.
#
# Primary source : reh$index$dyad_map_active  (columns: dyadIDactive, actor1,
#                                               actor2, type)
# Fallback        : reh$riskset_info$included  (columns: actor1, actor2, …)
# Returns NULL silently if neither is found.
# ---------------------------------------------------------------------------
.get_riskset_actors <- function(reh, D) {

  rs <- NULL

  # ── Primary: reh$index$dyad_map_active ──────────────────────────────────────
  if (!is.null(reh$index$dyad_map_active)) {
    dm <- reh$index$dyad_map_active
    if (NROW(dm) >= D && all(c("actor1", "actor2") %in% names(dm))) {
      rs <- dm[seq_len(D), c("actor1", "actor2"), drop = FALSE]
    }
  }

  # ── Fallback: reh$riskset_info$included ─────────────────────────────────────
  if (is.null(rs) && !is.null(reh$riskset_info$included)) {
    inc <- reh$riskset_info$included
    if (NROW(inc) >= D && all(c("actor1", "actor2") %in% names(inc))) {
      rs <- inc[seq_len(D), c("actor1", "actor2"), drop = FALSE]
    }
  }

  if (is.null(rs)) return(NULL)

  rownames(rs) <- NULL
  rs
}

#' @export
#' @method stack_stats tomstats
stack_stats.tomstats <- function(stats, reh, add_actors = TRUE) {

  if (!inherits(reh, "remify")) stop("'reh' must be a remify object.")

  # ── Dimensions ──────────────────────────────────────────────────────────────
  D  <- dim(stats)[2]   # number of dyads
  E  <- dim(stats)[1]   # number of time points in stats
  subset_idx <- as.integer(unlist(attr(stats, "subset")))  # [start, stop]

  # ── Ordinal flag ─────────────────────────────────────────────────────────────
  ordinal <- if (!is.null(reh$meta)) isTRUE(reh$meta$ordinal) else
             isTRUE(attr(reh, "ordinal"))

  # ── Riskset type ─────────────────────────────────────────────────────────────
  riskset <- if (!is.null(reh$meta))
    (reh$meta$riskset_source %||% reh$meta$riskset)
  else
    attr(reh, "riskset")

  # ── Stack statistics: [E*D x P] ──────────────────────────────────────────────
  stat_glm <- as.data.frame(
    do.call(rbind, lapply(seq_len(E), function(e) cbind(subset_idx[1] + e - 1L, stats[e, , ])))
  )
  colnames(stat_glm)[1] <- "time_index"

  # ── Offset: log inter-event time (interval timing only) ──────────────────────
  if (!ordinal) {
    iet <- reh$intereventTime[subset_idx[1]:subset_idx[2]]
    stat_glm$log_interevent <- rep(log(iet), each = D)
  }

  # ── Response: observed events per dyad per time point ────────────────────────
  # dyad_active / dyad contain 1-based column indices into the stats array
  if (!is.null(reh$ids)) {
    # New remify2 format
    if (riskset %in% c("active", "manual")) {
      # dyad_active is [N x 1] matrix — flatten to vector, then subset
      dyad_vec <- as.vector(reh$ids$dyad_active)
    } else {
      dyad_vec <- as.vector(reh$ids$dyad)
    }
  } else {
    # Old remify format
    if (riskset %in% c("active", "manual")) {
      dyad_vec <- as.vector(attr(reh, "dyadIDactive") %||% attr(reh, "dyadID"))
    } else {
      dyad_vec <- as.vector(attr(reh, "dyadID"))
    }
  }

  stat_glm$obs <- unlist(lapply(seq_len(E), function(e) {
    ev_idx <- subset_idx[1] + e - 1L
    obs_dyad <- dyad_vec[[ev_idx]]   # [[ ]] handles both scalar (thin=1) and vector (thin>1)
    tabulate(obs_dyad, nbins = D)
  }))

  # ── Dyad index ───────────────────────────────────────────────────────────────
  stat_glm$dyad <- rep(seq_len(D), E)

  # ── Actor labels: actor1 (sender) and actor2 (receiver) per row ──────────────
  if (add_actors) {
    rs_actors <- .get_riskset_actors(reh, D)
    if (!is.null(rs_actors)) {
      stat_glm$actor1 <- rs_actors$actor1[ stat_glm$dyad ]
      stat_glm$actor2 <- rs_actors$actor2[ stat_glm$dyad ]
    }
  }

  reserved <- c("time_index", "obs", "log_interevent", "dyad", "actor1", "actor2")
  structure(
    list(
      remstats_stack = stat_glm,
      subset     = subset_idx,
      D          = D,
      E          = E,
      ordinal    = ordinal,
      model      = "tie",
      sampled    = FALSE,
      stat_names = setdiff(names(stat_glm), reserved)
    ),
    class = "remstats_stacked"
  )
}

#' @export
#' @method stack_stats tomstats_sampled
stack_stats.tomstats_sampled <- function(stats, reh, add_actors = TRUE) {

  if (!inherits(reh, "remify")) stop("'reh' must be a remify object.")

  # ── Dimensions ──────────────────────────────────────────────────────────────
  E  <- dim(stats)[1]   # number of events
  S  <- dim(stats)[2]   # sampled set size per event
  subset_idx <- as.integer(unlist(attr(stats, "subset")))

  # ── Sampling attributes ──────────────────────────────────────────────────────
  sample_map <- attr(stats, "sample_map")  # [E x S] 1-based dyad indices
  samp_prob  <- attr(stats, "samp_prob")   # [E x S] inclusion probabilities
  case_pos   <- attr(stats, "case_pos")    # list of length E, 1-based

  if (is.null(sample_map) || is.null(samp_prob) || is.null(case_pos)) {
    stop("'stats' is missing required sampling attributes.")
  }

  # ── Ordinal flag ─────────────────────────────────────────────────────────────
  ordinal <- if (!is.null(reh$meta)) isTRUE(reh$meta$ordinal) else
             isTRUE(attr(reh, "ordinal"))

  # ── Stack statistics: [E*S x P] ──────────────────────────────────────────────
  stat_glm <- as.data.frame(
    do.call(rbind, lapply(seq_len(E), function(e) cbind(subset_idx[1] + e - 1L, stats[e, , ])))
  )
  colnames(stat_glm)[1] <- "time_index"

  # ── Offset: log inter-event time (interval only) ─────────────────────────────
  if (!ordinal) {
    iet <- reh$intereventTime[subset_idx[1]:subset_idx[2]]
    stat_glm$log_interevent <- rep(log(iet), each = S)
  }

  # ── Response: 1 for case, 0 for control ──────────────────────────────────────
  stat_glm$obs <- unlist(lapply(seq_len(E), function(e) {
    is_case <- integer(S)
    cp <- case_pos[[e]]  # 1-based case positions
    if (length(cp) > 0) is_case[cp] <- 1L
    is_case
  }))

  # ── Importance weights: 1 for cases, 1/pi_s for controls ─────────────────────
  stat_glm$weight <- unlist(lapply(seq_len(E), function(e) {
    w <- 1.0 / samp_prob[e, ]       # start with 1/pi_s for all
    cp <- case_pos[[e]]              # 1-based case positions
    if (length(cp) > 0) w[cp] <- 1.0  # cases get weight 1
    w
  }))

  # ── Sampled dyad index (1-based within active riskset) ───────────────────────
  stat_glm$dyad <- as.vector(t(sample_map))  # [E*S]: dyad ID for each row

  # ── Actor labels: actor1 (sender) and actor2 (receiver) per row ──────────────
  # sample_map holds 1-based indices into the full active riskset, so use the
  # maximum observed dyad index as the riskset size.
  if (add_actors) {
    D_full <- max(sample_map, na.rm = TRUE)
    rs_actors <- .get_riskset_actors(reh, D_full)
    if (!is.null(rs_actors)) {
      stat_glm$actor1 <- rs_actors$actor1[ stat_glm$dyad ]
      stat_glm$actor2 <- rs_actors$actor2[ stat_glm$dyad ]
    }
  }

  reserved <- c("time_index", "obs", "log_interevent", "dyad", "actor1", "actor2", "weight")
  structure(
    list(
      remstats_stack = stat_glm,
      subset     = subset_idx,
      S          = S,
      E          = E,
      ordinal    = ordinal,
      model      = "tie",
      sampled    = TRUE,
      stat_names = setdiff(names(stat_glm), reserved)
    ),
    class = "remstats_stacked"
  )
}


#' @export
#' @method stack_stats aomstats
stack_stats.aomstats <- function(stats, reh, add_actors = TRUE) {

  if (!inherits(reh, "remify")) stop("'reh' must be a remify object.")

  # ── Dimensions ───────────────────────────────────────────────────────────────
  subset_idx <- as.integer(unlist(attr(stats, "subset")))  # [start, stop]
  E  <- subset_idx[2] - subset_idx[1] + 1L
  N  <- reh$N

  # ── Ordinal flag ─────────────────────────────────────────────────────────────
  ordinal <- isTRUE(reh$meta$ordinal)

  # ── Observed sender/receiver per event ───────────────────────────────────────
  # ids$actor1 and ids$actor2 are 1-based integer IDs
  obs_sender   <- reh$ids$actor1[ subset_idx[1]:subset_idx[2] ]
  obs_receiver <- reh$ids$actor2[ subset_idx[1]:subset_idx[2] ]

  # ── Inter-event times (interval only) ────────────────────────────────────────
  if (!ordinal) {
    iet <- reh$intereventTime[ subset_idx[1]:subset_idx[2] ]
    log_iet <- log(iet)
  }

  # ── Receiver riskset (list indexed by 1-based sender ID) ─────────────────────
  rec_riskset <- reh$receiver_riskset  # list of length N, each element = valid receiver IDs

  # ── Actor label lookup (optional) ────────────────────────────────────────────
  # For aomstats the "actor" column already holds 1-based integer IDs; if the
  # caller wants labels we resolve them from the dictionary.
  actor_labels <- NULL
  if (add_actors && !is.null(reh$meta$dictionary$actors)) {
    dict <- reh$meta$dictionary$actors   # data frame: actorName, actorID
    actor_labels <- setNames(dict$actorName, dict$actorID)
  }

  # ── SENDER STACK ─────────────────────────────────────────────────────────────
  sender_stack <- if (!is.null(stats$sender_stats)) {

    ss <- stats$sender_stats  # [E x N x Ks]
    Ks <- dim(ss)[3]
    stat_names <- dimnames(ss)[[3]]

    df <- as.data.frame(
      do.call(rbind, lapply(seq_len(E), function(e) cbind(subset_idx[1] + e - 1L, ss[e, , ])))
    )
    colnames(df) <- c("time_index", stat_names)

    if (!ordinal) {
      df$log_interevent <- rep(log_iet, each = N)
    }

    df$obs   <- unlist(lapply(seq_len(E), function(e) {
      tabulate(obs_sender[[e]], nbins = N)   # [[ ]] handles simultaneous senders (thin>1)
    }))
    df$actor <- rep(seq_len(N), E)

    if (add_actors && !is.null(actor_labels)) {
      df$actor_label <- actor_labels[ as.character(df$actor) ]
    }

    df

  } else NULL

  # ── RECEIVER STACK ───────────────────────────────────────────────────────────
  receiver_stack <- if (!is.null(stats$receiver_stats)) {

    rs <- stats$receiver_stats  # [E x N x Kr]
    Kr <- dim(rs)[3]
    stat_names <- dimnames(rs)[[3]]

    do.call(rbind, lapply(seq_len(E), function(e) {
      # [[ ]] gives integer vector for both thin=1 (length 1) and thin>1 (length >1)
      senders   <- as.integer(obs_sender[[e]])
      receivers <- as.integer(obs_receiver[[e]])

      # Inner loop over simultaneous events at this time point
      do.call(rbind, lapply(seq_along(senders), function(j) {
        s_id  <- senders[j]
        obs_r <- receivers[j]
        r_ids <- rec_riskset[[ s_id ]]

        mat  <- matrix(rs[e, r_ids, ], nrow = length(r_ids), ncol = Kr)
        df_e <- as.data.frame(mat)
        colnames(df_e) <- stat_names

        df_e$obs   <- as.integer(r_ids == obs_r)
        df_e$actor <- r_ids
        df_e$time_index <- subset_idx[1] + e - 1L

        if (add_actors && !is.null(actor_labels)) {
          df_e$actor_label <- actor_labels[ as.character(r_ids) ]
          df_e[, c("time_index", stat_names, "obs", "actor", "actor_label")]
        } else {
          df_e[, c("time_index", stat_names, "obs", "actor")]
        }
      }))
    }))

  } else NULL

  reserved_s <- c("time_index", "obs", "log_interevent", "actor", "actor_label")
  reserved_r <- c("time_index", "obs", "actor", "actor_label")
  structure(
    list(
      sender_stack        = sender_stack,
      receiver_stack      = receiver_stack,
      subset              = subset_idx,
      N                   = N,
      E                   = E,
      ordinal             = ordinal,
      model               = "actor",
      sender_stat_names   = if (!is.null(sender_stack))
                              setdiff(names(sender_stack), reserved_s) else character(0),
      receiver_stat_names = if (!is.null(receiver_stack))
                              setdiff(names(receiver_stack), reserved_r) else character(0)
    ),
    class = "remstats_stacked"
  )
}

# ── Print / Summary ──────────────────────────────────────────────────────────

#' @export
print.remstats_stacked <- function(x, ...) {
  cat("Stacked Relational Event Network Statistics\n")

  if (identical(x$model, "durem")) {
    cat("> Model: tie-oriented (duration)\n")
    cat("> Timing:", if (isTRUE(x$ordinal)) "ordinal" else "interval", "\n")
    cat("> Dyads (start):", x$D_start, "\n")
    cat("> Dyads (end)  :", x$D_end,   "\n")
    cat("> Time points:", x$E, "\n")
    cat("> Stacked dimensions:",
        nrow(x$remstats_stack), "rows x",
        ncol(x$remstats_stack), "columns\n")
    cat("> Start statistics:\n")
    for (i in seq_along(x$stat_names_start))
      cat("\t >>", i, ":", x$stat_names_start[i], "\n")
    cat("> End statistics:\n")
    for (i in seq_along(x$stat_names_end))
      cat("\t >>", i, ":", x$stat_names_end[i], "\n")
    return(invisible(x))
  }

  if (x$model == "tie") {
    cat("> Model: tie-oriented\n")
    cat("> Timing:", if (x$ordinal) "ordinal" else "interval", "\n")
    if (isTRUE(x$sampled)) {
      cat("> Riskset: case-control sampled (S =", x$S, "per event)\n")
    } else {
      cat("> Dyads:", x$D, "\n")
    }
    cat("> Time points:", x$E, "\n")
    cat("> Stacked dimensions:",
        nrow(x$remstats_stack), "rows x",
        ncol(x$remstats_stack), "columns\n")
    cat("> Statistics:\n")
    for (i in seq_along(x$stat_names))
      cat("\t >>", i, ":", x$stat_names[i], "\n")

  } else {
    cat("> Model: actor-oriented\n")
    cat("> Timing:", if (x$ordinal) "ordinal" else "interval", "\n")
    cat("> Actors:", x$N, "\n")
    cat("> Time points:", x$E, "\n")
    if (!is.null(x$sender_stack)) {
      cat("> Sender model:\n")
      cat("\t >> Stacked dimensions:",
          nrow(x$sender_stack), "rows x",
          ncol(x$sender_stack), "columns\n")
      cat("\t >> Statistics:\n")
      for (i in seq_along(x$sender_stat_names))
        cat("\t \t >>>", i, ":", x$sender_stat_names[i], "\n")
    }
    if (!is.null(x$receiver_stack)) {
      cat("> Receiver model:\n")
      cat("\t >> Stacked dimensions:",
          nrow(x$receiver_stack), "rows x",
          ncol(x$receiver_stack), "columns\n")
      cat("\t >> Statistics:\n")
      for (i in seq_along(x$receiver_stat_names))
        cat("\t \t >>>", i, ":", x$receiver_stat_names[i], "\n")
    }
  }
  invisible(x)
}

# ── DuREM method ─────────────────────────────────────────────────────────────

#' @export
#' @method stack_stats remstats_durem
stack_stats.remstats_durem <- function(stats, reh, add_actors = TRUE) {
	
	if (missing(reh) || is.null(reh)) reh <- attr(stats, "reh")
	if (!inherits(reh, "remify_durem"))
		stop("'reh' must be a remify_durem object.")
	
	ordinal <- isTRUE(reh$meta$ordinal)
	
	ss <- stats$start_stats   # [M × D_s × P_s] or NULL
	es <- stats$end_stats     # [M × D_e × P_e] or NULL
	
	if (is.null(ss) && is.null(es))
		stop("Both start_stats and end_stats are NULL.")
	
	M   <- dim(ss %||% es)[1L]
	D_s <- if (!is.null(ss)) dim(ss)[2L] else 0L
	D_e <- if (!is.null(es)) dim(es)[2L] else 0L
	P_s <- if (!is.null(ss)) dim(ss)[3L] else 0L
	P_e <- if (!is.null(es)) dim(es)[3L] else 0L
	
	names_s <- if (!is.null(ss)) dimnames(ss)[[3L]] else character(0L)
	names_e <- if (!is.null(es)) dimnames(es)[[3L]] else character(0L)
	
	edgelist <- reh$edgelist      # time / actor1 / actor2 / end / type(?)
	ed       <- reh$edgelist_dual
	
	# ── Type riskset detection ─────────────────────────────────────────────────
	ext_by_type <- isTRUE(reh$meta$with_type_riskset)
	has_types   <- "type" %in% names(edgelist)
	
	# ── Riskset lookup from reh$riskset_info$included ────────────────────────
	# Row order in 'included' = column order in the stats arrays.
	# Works for saturated, active, and manual risksets.
	incl <- reh$riskset_info$included
	D_incl <- nrow(incl)

	if (D_s > 0L && D_s != D_incl)
		warning("D_s (", D_s, ") != nrow(included) (", D_incl,
						"); column mapping may be wrong")
	# if (D_e > 0L && D_e != D_incl)
	# 	warning("D_e (", D_e, ") != nrow(included) (", D_incl,
	# 					"); column mapping may be wrong")

	# Hash: (actor1, actor2 [, type]) -> 1-based column index
	if (ext_by_type && "type" %in% names(incl)) {
		incl_key <- paste(incl$actor1, incl$actor2, incl$type, sep = "\t")
	} else {
		incl_key <- paste(incl$actor1, incl$actor2, sep = "\t")
	}
	incl_lookup <- setNames(seq_len(D_incl), incl_key)

	# Base-dyad grouping for blocking: "a1\ta2" -> all column indices
	base_key_vec <- paste(incl$actor1, incl$actor2, sep = "\t")
	base_to_cols <- split(seq_len(D_incl), base_key_vec)

	# Column lookup
	.dcol <- function(a1, a2, tp = NULL) {
		if (ext_by_type && !is.null(tp))
			key <- paste(a1, a2, tp, sep = "\t")
		else
			key <- paste(a1, a2, sep = "\t")
		unname(incl_lookup[key])
	}

	.bkey <- function(a1, a2) paste(a1, a2, sep = "\t")
	
	# Pre-compute column indices for every event in the original edgelist
	ne <- nrow(edgelist)
	
	# Full column index (typed when ext=TRUE)
	if (ext_by_type && has_types) {
		s_col <- vapply(seq_len(ne),
										function(k) .dcol(edgelist$actor1[k],
																			edgelist$actor2[k],
																			edgelist$type[k]),
										integer(1L))
	} else {
		s_col <- vapply(seq_len(ne),
										function(k) .dcol(edgelist$actor1[k],
																			edgelist$actor2[k]),
										integer(1L))
	}
	
	# Base keys per event (for blocking: "a1\ta2")
	bkey_vec <- vapply(seq_len(ne),
										 function(k) .bkey(edgelist$actor1[k],
										 									 edgelist$actor2[k]),
										 character(1L))
	
	# End columns: same riskset, same ordering
	if (D_e > 0L) {
		rs_end  <- attr(es, "riskset")
		dir_end <- isTRUE(reh$durem$directed_end)
		.ek <- function(a1, a2)
			if (dir_end) paste(a1, a2, sep = "\t")
		else paste(pmin(a1, a2), pmax(a1, a2), sep = "\t")
		end_lookup <- setNames(seq_len(nrow(rs_end)),
													 .ek(rs_end$actor1, rs_end$actor2))
		e_col <- unname(end_lookup[.ek(edgelist$actor1, edgelist$actor2)])
	} else {
		e_col <- rep(0L, ne)
	}
	
	# Unique time points covered by the stats arrays
	utimes_all <- sort(unique(ed$time))
	subset_s <- as.integer(unlist(attr(stats, "subset")))
	utimes     <- utimes_all[subset_s[1L]:subset_s[2L]]
	
	# Log inter-event times
	if (!ordinal) {
		origin_t  <- if (subset_s[1L] > 1L) utimes_all[subset_s[1L] - 1L] else 0
		log_iet   <- log(utimes - c(origin_t, utimes[-M]))
	} else {
		log_iet   <- rep(0, M)  # placeholder, won't be included in output
	}
	
	all_s_cols  <- seq_len(D_incl)
	n_stat_cols <- P_s + P_e
	
	# type_exclusive: when TRUE (default), an active event of any type blocks
	# starting events of ALL types for the same actor pair (types are mutually
	# exclusive). When FALSE, types are independent — only the exact typed
	# dyad column is blocked. Has no effect without ext_by_type.
	type_excl <- !ext_by_type || !isFALSE(reh$durem$type_exclusive)
	
	# ── Main stacking loop ───────────────────────────────────────────────────────
	block_list <- vector("list", M)
	
	for (m in seq_len(M)) {
		t    <- utimes[m]
		liet <- log_iet[m]
		
		# Active events: started before t, end strictly after t.
		# An event with end == t ends AT t and is no longer active.
		active_mask <- edgelist$time < t &
			(is.na(edgelist$end) | edgelist$end > t)
		
		# Blocking: exclude active dyads from the start riskset
		if (type_excl) {
			blocked_bkeys <- unique(bkey_vec[active_mask])
			blocked_scols <- unique(unlist(base_to_cols[blocked_bkeys]))
		} else {
			blocked_scols <- unique(s_col[active_mask])
		}
		
		# State 1 – observed end (ended exactly at t, started before t)
		end_mask <- !is.na(edgelist$end) & edgelist$end == t & edgelist$time < t
		end_obs_cols <- if (D_e > 0L) unique(e_col[end_mask]) else integer(0L)
		
		# State 2 – ongoing at risk to end (started strictly before t, runs past t)
		ong_mask <- edgelist$time < t & (is.na(edgelist$end) | edgelist$end > t)
		ong_cols <- if (D_e > 0L) unique(e_col[ong_mask]) else integer(0L)
		
		# State 3 – observed start (started at t)
		sta_obs_cols <- unique(s_col[edgelist$time == t])
		
		# State 4 – inactive start dyads (not blocked, not already counted as observed)
		inactive_cols <- setdiff(all_s_cols, c(blocked_scols, sta_obs_cols))
		
		n_rows <- length(end_obs_cols) + length(ong_cols) +
			length(sta_obs_cols)  + length(inactive_cols)
		if (n_rows == 0L) next
		
		mat <- matrix(0, nrow = n_rows, ncol = n_stat_cols + 4L)
		r   <- 0L
		
		start_col_range <- if (P_s > 0L) 3L:(2L + P_s)              else integer(0L)
		end_col_range   <- if (P_e > 0L) (3L + P_s):(2L + P_s + P_e) else integer(0L)
		
		# State 1 – observed end
		if (P_e > 0L) {
			for (d in end_obs_cols) {
				r <- r + 1L
				mat[r, 1L]             <- 1L
				mat[r, 2L]             <- liet
				mat[r, end_col_range]  <- c(unname(es[m, d, ]))
				mat[r, n_stat_cols + 3L] <- subset_s[1] + m - 1L
				mat[r, n_stat_cols + 4L] <- d
			}
		}
		
		# State 2 – ongoing
		if (P_e > 0L) {
			for (d in ong_cols) {
				r <- r + 1L
				mat[r, 2L]             <- liet
				mat[r, end_col_range]  <- c(unname(es[m, d, ]))
				mat[r, n_stat_cols + 3L] <- subset_s[1] + m - 1L
				mat[r, n_stat_cols + 4L] <- d
			}
		}
		
		# State 3 – observed start
		if (P_s > 0L) {
			for (d in sta_obs_cols) {
				r <- r + 1L
				mat[r, 1L]               <- 1L
				mat[r, 2L]               <- liet
				mat[r, start_col_range]  <- c(unname(ss[m, d, ]))
				mat[r, n_stat_cols + 3L] <- subset_s[1] + m - 1L
				mat[r, n_stat_cols + 4L] <- d
			}
		}
		
		# State 4 – inactive
		if (P_s > 0L) {
			for (d in inactive_cols) {
				r <- r + 1L
				mat[r, 2L]               <- liet
				mat[r, start_col_range]  <- c(unname(ss[m, d, ]))
				mat[r, n_stat_cols + 3L] <- subset_s[1] + m - 1L
				mat[r, n_stat_cols + 4L] <- d
			}
		}
		
		block_list[[m]] <- mat[seq_len(r), , drop = FALSE]
	}
	
	df <- as.data.frame(do.call(rbind, block_list))
	colnames(df) <- c("obs", "log_interevent",
										names_s, names_e,
										"time_index", "dyad")
	if (ordinal) df$log_interevent <- NULL
	
	# ── Optional actor name columns ───────────────────────────────────────────
	if (ext_by_type && "type" %in% names(incl)) {
		df$type <- incl$type[df$dyad]
	} else if (has_types) {
		df$type <- NA_character_
	}
	
	if (add_actors) {
		df$actor1 <- incl$actor1[df$dyad]
		df$actor2 <- incl$actor2[df$dyad]
	}
	
	stat_names_all <- c(names_s, names_e)
	
	structure(
		list(
			remstats_stack   = df,
			subset           = subset_s,
			D_start          = D_s,
			D_end            = D_e,
			E                = M,
			ordinal          = ordinal,
			model            = "durem",
			sampled          = FALSE,
			stat_names       = stat_names_all,
			stat_names_start = names_s,
			stat_names_end   = names_e
		),
		class = c("remstats_stacked_durem", "remstats_stacked")
	)
}


#' @export
summary.remstats_stacked <- function(object, ...) {
  print(object)
  cat("\n")
  if (identical(object$model, "durem")) {
    cat("── Stacked data ────────────────────────────────────────────────────────\n")
    print(summary(object$remstats_stack))
  } else if (object$model == "tie") {
    cat("── Stacked data ────────────────────────────────────────────────────────\n")
    print(summary(object$remstats_stack))
  } else {
    if (!is.null(object$sender_stack)) {
      cat("── Sender stack ────────────────────────────────────────────────────────\n")
      print(summary(object$sender_stack))
    }
    if (!is.null(object$receiver_stack)) {
      cat("── Receiver stack ──────────────────────────────────────────────────────\n")
      print(summary(object$receiver_stack))
    }
  }
  invisible(object)
}
