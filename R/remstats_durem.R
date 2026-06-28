# ─────────────────────────────────────────────────────────────────────────────
# remstats_durem.R
# Duration Relational Event Model — remstats dispatch
#
# Entry point: remstats(reh, start_effects, end_effects, psi_start, psi_end, ...)
#   when reh is a remify_durem object. Dispatched internally via
#   .remstats_durem_dispatch().
#
# Returns a remstats_durem object: a list with $start_stats and $end_stats
# (3-D arrays [M × D × P] with effect names suffixed .start / .end) plus
# metadata needed by durem_statstack() at estimation time.
# ─────────────────────────────────────────────────────────────────────────────


# ── Internal constructor ──────────────────────────────────────────────────────

#' Internal remstats dispatch for \code{remify_durem} objects
#'
#' Called by \code{\link{remstats}} when \code{reh} inherits from
#' \code{"remify_durem"}.  Builds a dual-event edgelist (one \code{"start"} row
#' and one \code{"end"} row per event), applies psi weighting, and calls
#' \code{tomstats} twice — once for the start model, once for the end model.
#'
#' @param reh A \code{remify_durem} object.
#' @param start_effects Formula for start-model statistics (remstats syntax).
#' @param end_effects Formula for end-model statistics.
#' @param psi_start Numeric. Duration exponent for start-model history
#'   weighting. Event weight is \code{w * (end - time + 1)^psi_start}.
#'   Default \code{1}.
#' @param psi_end Numeric. Duration exponent for end-model history weighting.
#'   Default \code{1}.
#' @inheritParams remstats
#' @return A \code{remstats_durem} object.
#' @keywords internal
.remstats_durem <- function(
    reh,
    start_effects    = NULL,
    end_effects      = NULL,
    psi_start        = 1,
    psi_end          = 1,
    attr_actors      = NULL,
    attr_dyads       = NULL,
    memory           = c("full", "window", "decay", "interval"),
    memory_value     = NA,
    start            = 2,
    stop             = Inf,
    display_progress = FALSE
) {
    memory <- match.arg(memory)

    # $edgelist_dual was built by .remify_durem_init() — columns:
    #   time, actor1, actor2, status ("start"/"end"), duration, weight, [type]
    ed     <- reh$edgelist_dual
    actors <- reh$meta$dictionary$actors$actorName
    riskset_type <- reh$meta$riskset
    ordi <- reh$meta$ordinal
    
    # directional flags
    directed_start <- isTRUE(reh$meta$directed)
    directed_end   <- isTRUE(reh$durem$dur_directed_end)

    # Preserve the original extend_riskset_by_type setting
    ext_by_type <- isTRUE(reh$meta$with_type_riskset)

    # ── Helper: build a remify-ready edgelist with psi-scaled weights ─────────
    # Applies (base_weight * duration^psi) when either user weights exist or
    # psi != 0.  When both are absent the weight column is omitted entirely and
    # remify/tomstats treat every event as having weight 1.
    has_weight_col <- "weight" %in% names(ed)
    .make_el <- function(psi) {
    	out <- data.frame(
    		time   = ed$time,
    		actor1 = ed$actor1,
    		actor2 = ed$actor2,
    		stringsAsFactors = FALSE
    	)
    	base_w <- if (has_weight_col) ed$weight else rep(1, nrow(ed))
    	w <- base_w * (ed$duration + 1) ^ psi
    	# Only completed (end) events contribute to history; start events
    	# are evaluation points with zero weight. This ensures ongoing
    	# events don't inflate inertia before their duration is realized.
    	w[ed$status == "start"] <- 0
    	out$weight <- w
    	if ("type" %in% names(ed)) out$type <- ed$type
    	out
    }

    # ── Start model ───────────────────────────────────────────────────────────
    if (!is.null(start_effects)) {
        suppressWarnings(
            reh_start <- remify(
            		edgelist = .make_el(psi_start),
                directed = directed_start,
            		ordinal  = ordi,
                model    = "tie",
                actors   = actors,
            		riskset  = riskset_type,
                extend_riskset_by_type = ext_by_type
            )
        )

        start_stats <- tomstats(
            tie_effects      = start_effects,
            reh              = reh_start,
            attr_actors      = attr_actors,
            attr_dyads       = attr_dyads,
            memory           = memory,
            memory_value     = memory_value,
            first            = start,
            last             = stop,
            display_progress = display_progress
        )
        dimnames(start_stats)[[3]] <- paste0(dimnames(start_stats)[[3]], ".start")
    } else {
        start_stats <- NULL
    }

    # ── End model ─────────────────────────────────────────────────────────────
    if (!is.null(end_effects)) {
        suppressWarnings(
            reh_end <- remify(
            		edgelist = .make_el(psi_end),
                directed = directed_end,
            		ordinal  = ordi,
                model    = "tie",
                actors   = actors,
            		riskset  = riskset_type,
                extend_riskset_by_type = ext_by_type
            )
        )

        end_stats <- tomstats(
            tie_effects      = end_effects,
            reh              = reh_end,
            attr_actors      = attr_actors,
            attr_dyads       = attr_dyads,
            memory           = memory,
            memory_value     = memory_value,
            first            = start,
            last             = stop,
            display_progress = display_progress
        )
        dimnames(end_stats)[[3]] <- paste0(dimnames(end_stats)[[3]], ".end")
        
        rs <- attr(end_stats, "riskset")
        names(rs)[names(rs) == "sender"]   <- "actor1"
        names(rs)[names(rs) == "receiver"] <- "actor2"
        attr(end_stats, "riskset") <- rs
        
    } else {
        end_stats <- NULL
    }
    
    if (!is.null(start_stats)) {
    	last_timepoint <- unlist(attr(start_stats,"subset"))[2]
    }else if (!is.null(end_stats)) {
    	last_timepoint <- unlist(attr(end_stats,"subset"))[2]
    }else{
    	last_timepoint <- NA
    }

    # ── Assemble remstats_durem object ────────────────────────────────────────
    out <- list(
        start_stats = start_stats,
        end_stats   = end_stats,
        psi_start   = psi_start,
        psi_end     = psi_end
    )
    attr(out, "reh") <- reh
    attr(out, "model") <- reh$meta$model
    attr(out, "subset") <- c(first=start,last=unname(last_timepoint))
    class(out) <- c("remstats_durem", "remstats")

    out
}


# ── remstats.remify_durem dispatch ───────────────────────────────────────────

#' Internal dispatch for \code{remify_durem} objects
#'
#' Called by \code{\link{remstats}} when \code{reh} inherits from
#' \code{"remify_durem"}.  Each formula is inspected term-by-term:
#'
#' \itemize{
#'   \item \strong{Pure active-state} formulas (only \code{activeTie()},
#'     \code{activeOutdegreeSender()}, etc.) are forwarded to
#'     \code{duremstats}.
#'   \item \strong{Pure history-weighted} formulas (only \code{inertia()},
#'     \code{reciprocity()}, etc.) are forwarded to \code{.remstats_durem},
#'     which calls \code{tomstats} with optional psi-weighting.
#'   \item \strong{Mixed} formulas are split automatically: active-state terms
#'     go to \code{duremstats} and history-weighted terms go to
#'     \code{.remstats_durem}; the two resulting arrays are combined along the
#'     statistics dimension before being returned.
#' }
#'
#' @param reh            A \code{remify_durem} object.
#' @param start_effects  Formula for start-model statistics.
#' @param end_effects    Formula for end-model statistics.
#' @param psi_start      Duration exponent for start-model history weighting
#'   (forwarded to \code{.remstats_durem}; ignored for active-state effects).
#' @param psi_end        Duration exponent for end-model history weighting.
#' @param attr_actors    Actor-level attribute data frame (forwarded to
#'   \code{tomstats}; ignored for active-state effects).
#' @param attr_dyads     Dyad-level attribute data frame (forwarded to
#'   \code{tomstats}; ignored for active-state effects).
#' @param memory         Memory type forwarded to \code{tomstats}.
#' @param memory_value   Memory value forwarded to \code{tomstats}.
#' @param start          First time-point index.
#' @param stop           Last  time-point index.
#' @param display_progress Logical.
#' @return A \code{remstats_durem} object.
#' @keywords internal

# ── Finalize a durem stats object ──────────────────────────────────────────────
# Build the fit-ready stacked design once, at construction time, attach it as
# `$stacked`, and drop the full `start_stats` / `end_stats` arrays to keep the
# object small. The arrays are fully recomputable from (reh, formula); the
# stacked design is the minimal sufficient representation for fitting, so the
# arrays are pure overhead once it exists.
#
# Actor columns (actor1/actor2) are carried on the stacked design so that
# downstream analyses needing dyad identity have it. This does not perturb
# the GLM design: remstimate selects predictors by name via stat_names_*,
# and .select_stats_durem subsets remstats_stack by column name (not
# position), so the two trailing character columns are inert for fitting.
.finalize_durem <- function(out, reh) {
    design <- .stack_durem(out, reh, add_actors = TRUE)
    out$stacked     <- design
    out$start_stats <- NULL
    out$end_stats   <- NULL
    out
}

.remstats_durem_dispatch <- function(reh,
                                   start_effects    = NULL,
                                   end_effects      = NULL,
                                   psi_start        = 1,
                                   psi_end          = 1,
                                   attr_actors      = NULL,
                                   attr_dyads       = NULL,
                                   memory           = c("full", "window",
                                                        "decay", "interval"),
                                   memory_value     = NA,
                                   start            = 2,
                                   stop             = Inf,
                                   display_progress = FALSE) {

    memory <- match.arg(memory)

    all_durem_effects <- c(names(.durem_stat_type_directed),
                           names(.durem_stat_type_undirected))

    # ── Helpers ───────────────────────────────────────────────────────────────

    # Classify a formula: "durem", "history", "mixed", or "empty"
    .classify <- function(formula) {
        if (is.null(formula)) return("empty")
        labels       <- attr(terms(formula), "term.labels")
        effect_names <- sub("\\(.*$", "", labels)
        is_durem     <- effect_names %in% all_durem_effects
        if (all(is_durem))  return("durem")
        if (!any(is_durem)) return("history")
        return("mixed")
    }

    # Split a formula into its durem and history sub-formulas
    .split_formula <- function(formula) {
        if (is.null(formula)) return(list(durem = NULL, history = NULL))
        labels       <- attr(terms(formula), "term.labels")
        effect_names <- sub("\\(.*$", "", labels)
        is_durem     <- effect_names %in% all_durem_effects
        make_f <- function(terms) {
            if (length(terms) == 0L) return(NULL)
            as.formula(paste("~", paste(terms, collapse = " + ")))
        }
        list(durem   = make_f(labels[ is_durem]),
             history = make_f(labels[!is_durem]))
    }

    # ── Classify both formulas ────────────────────────────────────────────────
    cls_start <- .classify(start_effects)
    cls_end   <- .classify(end_effects)

    use_durem   <- cls_start %in% c("durem",   "mixed") ||
                   cls_end   %in% c("durem",   "mixed")
    use_history <- cls_start %in% c("history", "mixed") ||
                   cls_end   %in% c("history", "mixed")

    # ── Regular tomstats ─────────────────────────────────────────────────────
    if (!use_durem) {
        return(.finalize_durem(.remstats_durem(
            reh              = reh,
            start_effects    = start_effects,
            end_effects      = end_effects,
            psi_start        = psi_start,
            psi_end          = psi_end,
            attr_actors      = attr_actors,
            attr_dyads       = attr_dyads,
            memory           = memory,
            memory_value     = memory_value,
            start            = start,
            stop             = stop,
            display_progress = display_progress
        ), reh))
    }

    # ── Pure active-state ─────────────────────────────────────────────────────
    if (!use_history) {
        return(.finalize_durem(duremstats(
            reh              = reh,
            start_effects    = start_effects,
            end_effects      = end_effects,
            start            = start,
            stop             = stop,
            display_progress = display_progress
        ), reh))
    }

    # ── Mixed: split, compute each half, combine ──────────────────────────────
    sp_start <- .split_formula(start_effects)
    sp_end   <- .split_formula(end_effects)

    dstats <- duremstats(
        reh              = reh,
        start_effects    = sp_start$durem,
        end_effects      = sp_end$durem,
        start            = start,
        stop             = stop,
        display_progress = display_progress
    )

    hstats <- .remstats_durem(
        reh              = reh,
        start_effects    = sp_start$history,
        end_effects      = sp_end$history,
        psi_start        = psi_start,
        psi_end          = psi_end,
        attr_actors      = attr_actors,
        attr_dyads       = attr_dyads,
        memory           = memory,
        memory_value     = memory_value,
        start            = start,
        stop             = stop,
        display_progress = display_progress
    )

    out_stats <- bind_remstats(dstats, hstats)
    
    # ref <- (dstats %||% hstats)
    # ref_arr <- ref$start_stats %||% ref$end_stats
    # attr(out_stats, "subset") <- attr(ref_arr, "subset") 
    # attr(out_stats, "method") <- attr(ref_arr, "method")
    # attr(out_stats, "dyad_keys") <- attr(ref_arr, "dyad_keys")
    
    .finalize_durem(out_stats, reh)
    
}


# ── S3 methods ────────────────────────────────────────────────────────────────

#' Test whether an object is a \code{remstats_durem}
#'
#' @param x Any R object.
#' @return \code{TRUE} if \code{x} inherits from \code{"remstats_durem"}.
#' @export
is.remstats_durem <- function(x) inherits(x, "remstats_durem")


#' Print method for \code{remstats_durem}
#'
#' @param x A \code{remstats_durem} object.
#' @param ... Ignored.
#' @export
print.remstats_durem <- function(x, ...) {
    st  <- x$stacked
    reh <- attr(x, "reh")

    cat("Relational Event Network Statistics\n")
    cat("> Model: tie-oriented (duration)\n")
    cat("> Computation method: per time point\n")

    # Prefer the attached stacked design (post-shrink); fall back to the raw
    # arrays when an object still carries them (pre-shrink / legacy objects).
    if (!is.null(st)) {
        nms_s <- st$stat_names_start
        nms_e <- st$stat_names_end
        dims_s <- if (length(nms_s)) c(st$E, st$D_start, length(nms_s)) else NULL
        dims_e <- if (length(nms_e)) c(st$E, st$D_end,   length(nms_e)) else NULL
    } else {
        ss <- x$start_stats; es <- x$end_stats
        nms_s  <- if (!is.null(ss)) dimnames(ss)[[3]] else character(0)
        nms_e  <- if (!is.null(es)) dimnames(es)[[3]] else character(0)
        dims_s <- if (!is.null(ss)) dim(ss) else NULL
        dims_e <- if (!is.null(es)) dim(es) else NULL
    }

    # ── Start model ────────────────────────────────────────────────────────────
    if (!is.null(dims_s)) {
        cat(sprintf("> Start dimensions: %d time points x %d dyads x %d statistics\n",
                    dims_s[1], dims_s[2], dims_s[3]))
        cat("> Start statistics:\n")
        for (i in seq_along(nms_s))
            cat(sprintf("\t >> %d: %s\n", i, nms_s[i]))
    } else {
        cat("> Start statistics: (none)\n")
    }

    # ── End model ──────────────────────────────────────────────────────────────
    if (!is.null(dims_e)) {
        cat(sprintf("> End dimensions: %d time points x %d dyads x %d statistics\n",
                    dims_e[1], dims_e[2], dims_e[3]))
        cat("> End statistics:\n")
        for (i in seq_along(nms_e))
            cat(sprintf("\t >> %d: %s\n", i, nms_e[i]))
    } else {
        cat("> End statistics: (none)\n")
    }

    cat(sprintf("> psi: start = %g   end = %g\n", x$psi_start, x$psi_end))

    invisible(x)
}


#' Summary method for \code{remstats_durem}
#'
#' @param object A \code{remstats_durem} object.
#' @param ... Ignored.
#' @export
summary.remstats_durem <- function(object, ...) {
    st  <- object$stacked
    out <- list()

    if (!is.null(st)) {
        # Summaries over the at-risk rows actually entering the likelihood,
        # split by process. This is the riskset-relevant view: start statistics
        # over start-process rows, end statistics over end-process rows.
        df <- st$remstats_stack
        if (length(st$stat_names_start)) {
            sd <- df[df$process == "start", st$stat_names_start, drop = FALSE]
            out$start <- sapply(sd, summary)
        }
        if (length(st$stat_names_end)) {
            ed <- df[df$process == "end", st$stat_names_end, drop = FALSE]
            out$end <- sapply(ed, summary)
        }
        return(out)
    }

    # Fallback for objects that still carry the raw arrays.
    if (!is.null(object$start_stats))
        out$start <- apply(object$start_stats, 3, function(y) summary(as.vector(y)))
    if (!is.null(object$end_stats))
        out$end   <- apply(object$end_stats,   3, function(y) summary(as.vector(y)))
    out
}
