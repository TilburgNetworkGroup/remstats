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
# Entry point: duremstats(reh, start_effects, end_effects, ...)
# Returns:     a list with $start_stats and $end_stats (same shape as
#              remstats_durem, so they can be combined at estimation time)
# ─────────────────────────────────────────────────────────────────────────────


# ── Effect name → stat_type integer ──────────────────────────────────────────

.durem_stat_type_directed <- c(
    activeTie                   = 1L,
    activeOutdegreeSender       = 2L,
    activeIndegreeReceiver      = 3L,
    activeTotaldegreeSender     = 4L,
    activeTotaldegreeReceiver   = 5L,
    activeSharedPartners_otp    = 6L,
    activeSharedPartners_itp    = 7L,
    activeSharedPartners_osp    = 8L,
    activeSharedPartners_isp    = 9L
)

.durem_stat_type_undirected <- c(
    activeTie                   = 1L,
    activeDegreeActor1          = 2L,
    activeDegreeActor2          = 3L,
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


# ── Argument helpers ─────────────────────────────────────────────────────────

#' Validate consider_type for active-state effects
#' @keywords internal
.validate_consider_type_durem <- function(consider_type) {
    valid <- c("ignore", "separate", "interact")
    if (!consider_type %in% valid)
        stop(
            "`consider_type = \"", consider_type, "\"` is not supported for ",
            "active-state statistics.\n",
            "Valid values: \"ignore\", \"separate\", \"interact\".",
            call. = FALSE
        )
    invisible(NULL)
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
        # Evaluate the call in the current search path so that activeTie() etc.
        # resolve to the exported stub functions and return their config list.
        tryCatch(
            eval(parse(text = lbl)),
            error = function(e) {
                if (grepl("could not find function", conditionMessage(e),
                          fixed = TRUE))
                    stop("Unknown active-state effect '",
                         sub("\\(.*$", "", lbl), "'. ",
                         "See ?active_effects for available effects.",
                         call. = FALSE)
                stop("Could not evaluate active-state effect term '", lbl,
                     "': ", conditionMessage(e), call. = FALSE)
            }
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

    # Number of output time points
    M <- as.integer(stop - start + 1L)

    # ── Expand effect configs ──────────────────────────────────────────────────
    # Each entry carries a $type_label field:
    #   NA           → "ignore" (or "interact") — one entry per original effect
    #   "<type>"     → "separate"               — one entry per type
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
        } else {
            # "ignore" and "interact" (with types): single output stat
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
    out_D <- vapply(expanded, function(e) {
        if (e$consider_type == "interact") D_interact
        else if (!is.na(e$type_label))     D_sep
        else                               D_ignore
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

    # ── Per-effect computation ────────────────────────────────────────────────
    for (p_idx in seq_along(expanded)) {
        e     <- expanded[[p_idx]]
        eff   <- e$effect
        stype <- stat_map[[eff]]

        if (display_progress)
            message("Calculating active-state statistic: ", eff,
                    if (!is.na(e$type_label)) paste0(" [type = ", e$type_label, "]") else "")

        # Choose the right edgelist and riskset, then call C++
        if (e$consider_type == "interact") {
            # Call C++ once per type with the type-filtered edgelist and untyped
            # riskset, then cbind the results into a [M × D*C] matrix.
            # Column order mirrors type_levels (alphabetically sorted) so that
            # type c occupies columns [(c-1)*D_sep + 1 .. c*D_sep].
            mats <- lapply(seq_along(type_levels), function(tc_idx) {
                calculate_active_stats(
                    edgelist         = ed_mat_per_type[[tc_idx]],
                    risksetMatrix    = riskset_mat_sep,
                    stat_type        = stype,
                    directed         = directed,
                    start            = as.integer(start),
                    stop             = as.integer(stop),
                    display_progress = FALSE   # avoid duplicate messages
                )
            })
            mat <- do.call(cbind, mats)   # [M × D*C]
        } else if (is.na(e$type_label)) {
            # "ignore": use full edgelist and (possibly typed) ignore riskset
            mat <- calculate_active_stats(
                edgelist         = ed_mat_full,
                risksetMatrix    = riskset_mat_ignore,
                stat_type        = stype,
                directed         = directed,
                start            = as.integer(start),
                stop             = as.integer(stop),
                display_progress = display_progress
            )
        } else {
            # "separate": use type-filtered edgelist and untyped riskset
            tc_idx <- match(e$type_label, type_levels)
            mat <- calculate_active_stats(
                edgelist         = ed_mat_per_type[[tc_idx]],
                risksetMatrix    = riskset_mat_sep,
                stat_type        = stype,
                directed         = directed,
                start            = as.integer(start),
                stop             = as.integer(stop),
                display_progress = display_progress
            )
        }   # mat is [M × D] (or [M × D*C] for "interact")

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
#' Available effects for directed networks:
#' \describe{
#'   \item{\code{activeTie()}}{Whether there is a currently active event from
#'     actor i to actor j.}
#'   \item{\code{activeOutdegreeSender()}}{Number of active events in which
#'     actor i (sender) is currently involved as sender.}
#'   \item{\code{activeIndegreeReceiver()}}{Number of active events in which
#'     actor j (receiver) is currently involved as receiver.}
#'   \item{\code{activeTotaldegreeSender()}}{Total active degree (in + out) of
#'     actor i.}
#'   \item{\code{activeTotaldegreeReceiver()}}{Total active degree of actor j.}
#'   \item{\code{activeSharedPartners_otp()}}{Number of actors h for whom
#'     i→h and h→j are both currently active (outgoing two-path).}
#'   \item{\code{activeSharedPartners_itp()}}{Incoming two-path version.}
#'   \item{\code{activeSharedPartners_osp()}}{Outgoing shared partner version.}
#'   \item{\code{activeSharedPartners_isp()}}{Incoming shared partner version.}
#' }
#'
#' Available effects for undirected networks:
#' \describe{
#'   \item{\code{activeTie()}}{Whether there is a currently active event
#'     between i and j.}
#'   \item{\code{activeDegreeActor1()}}{Active degree of actor i.}
#'   \item{\code{activeDegreeActor2()}}{Active degree of actor j.}
#'   \item{\code{activeSharedPartners()}}{Number of actors h for whom both
#'     (i,h) and (j,h) are currently active.}
#' }
#'
#' @param reh A \code{remify_durem} object.
#' @param start_effects Formula of active-state effects for the start model,
#'   e.g. \code{~ activeTie() + activeOutdegreeSender()}.
#' @param end_effects Formula of active-state effects for the end model.
#' @param start Integer. Index of first time point to compute (default 2).
#' @param stop  Integer. Index of last  time point to compute (default Inf).
#' @param display_progress Logical. Show progress messages.
#' @return A list with \code{$start_stats} and \code{$end_stats}: 3-D arrays
#'   \[M × D × P\] with effect names suffixed \code{.start} / \code{.end},
#'   and \code{attr(., "reh")} set to \code{reh}.  The same shape as a
#'   \code{remstats_durem} object so the two can be combined at estimation time.
#' @export
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
    directed_end   <- isTRUE(reh$durem$directed_end)

    eff_start <- .parse_active_effects(start_effects)
    eff_end   <- .parse_active_effects(end_effects)

    ss <- .compute_active_stats(eff_start, reh, directed_start,
                                start, stop, ".start", display_progress)
    es <- .compute_active_stats(eff_end,   reh, directed_end,
                                start, stop, ".end",   display_progress)

    out <- list(start_stats = ss, end_stats = es)
    attr(out, "reh") <- reh
    class(out) <- "remstats_durem"   # same class → compatible with estimation
    out
}


# ── Effect constructor functions ──────────────────────────────────────────────
# Each function mirrors the remstats pattern: calling it returns a named list
# that duremstats() reads to configure the C++ computation and post-processing.
# They are designed for use inside formulas:
#   duremstats(reh, start_effects = ~ activeTie() + activeOutdegreeSender("std"))
# but can also be called directly, e.g. to inspect the default configuration.

#' Active-state statistics for Duration Relational Event Models
#'
#' Constructor functions for active-state effects used inside formulas passed
#' to \code{\link{duremstats}}.  They capture properties of the currently
#' \emph{active} event network — events that have started but not yet ended —
#' at each time point in a duration relational event sequence.
#'
#' Each function returns a configuration list consumed by \code{duremstats()}.
#' The functions can be called directly to inspect defaults or passed inside
#' a formula:
#' \preformatted{
#'   duremstats(reh,
#'     start_effects = ~ activeTie() + activeOutdegreeSender(scaling = "std"),
#'     end_effects   = ~ activeTie())
#' }
#'
#' \strong{Directed-network effects:}
#' \describe{
#'   \item{\code{activeTie()}}{
#'     Whether there is currently an active event from actor \eqn{i} to actor
#'     \eqn{j} (binary, 0/1).}
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
#'   \item{\code{activeSharedPartners_otp()}}{
#'     Number of actors \eqn{h} for whom \eqn{i \to h} and \eqn{h \to j} are
#'     both currently active (outgoing two-path shared partners).}
#'   \item{\code{activeSharedPartners_itp()}}{
#'     Incoming two-path: actors \eqn{h} with \eqn{h \to i} and \eqn{j \to h}
#'     both active.}
#'   \item{\code{activeSharedPartners_osp()}}{
#'     Outgoing shared partners: actors \eqn{h} with \eqn{i \to h} and
#'     \eqn{j \to h} both active.}
#'   \item{\code{activeSharedPartners_isp()}}{
#'     Incoming shared partners: actors \eqn{h} with \eqn{h \to i} and
#'     \eqn{h \to j} both active.}
#' }
#'
#' \strong{Undirected-network effects:}
#' \describe{
#'   \item{\code{activeTie()}}{
#'     Whether there is currently an active event between actors \eqn{i} and
#'     \eqn{j}.}
#'   \item{\code{activeDegreeActor1()}}{
#'     Active degree of actor \eqn{i}.}
#'   \item{\code{activeDegreeActor2()}}{
#'     Active degree of actor \eqn{j}.}
#'   \item{\code{activeSharedPartners()}}{
#'     Number of actors \eqn{h} for whom both \eqn{(i,h)} and \eqn{(j,h)} are
#'     currently active.}
#' }
#'
#' @param scaling Scaling applied to the raw C++ counts before returning:
#'   \describe{
#'     \item{\code{"none"}}{Raw counts (default).}
#'     \item{\code{"std"}}{Per-time-point standardisation:
#'       \eqn{(x - \bar{x}) / \mathrm{sd}(x)}, computed over the \eqn{D}
#'       dyads in the fixed risk set.  \eqn{D} is determined once at
#'       \code{\link[remify]{remify}} time via the \code{riskset} argument
#'       (\code{"full"}, \code{"active"}, \code{"active_saturated"}, or
#'       \code{"manual"}) and does not change over time.  This mirrors the
#'       behaviour of \code{remstats}, where standardisation is always over
#'       the fixed risk-set dyads.}
#'   }
#'   \code{"prop"} is not defined for active-state statistics because the
#'   natural denominator (maximum possible active degree) depends on
#'   modelling assumptions and is not uniquely determined.
#' @param consider_type Character. How event types are handled:
#'   \describe{
#'     \item{\code{"ignore"}}{Aggregate over all event types (default).  The
#'       statistic counts all currently active events regardless of type.  When
#'       \code{extend_riskset_by_type = TRUE} in the \code{remify} call, the
#'       output D matches \code{reh$D} (typed dyads), but the value is the
#'       same for all type slots of a given actor pair.}
#'     \item{\code{"separate"}}{Compute one statistic per event type.  For
#'       each type \eqn{c}, only currently active events of that type
#'       contribute.  Implemented in R by filtering \code{edgelist_dual} to
#'       type-\eqn{c} events and calling the C++ function once per type;
#'       no C++ changes required.  The output effect names are suffixed with
#'       the type label, e.g. \code{activeTie.X.start}.}
#'     \item{\code{"interact"}}{Compute one statistic with \eqn{D\times C} columns:
#'     for each (actor-pair, type) combination, only active events of that type
#'     contribute.  Implemented R-side by filtering \code{edgelist_dual} to
#'     each type and calling the C++ function once per type; results are
#'     \code{cbind}-ed in alphabetical type order.  The output has
#'     \eqn{D_{\text{base}} \times C} columns and is consistent with a typed
#'     riskset (\code{extend_riskset_by_type = TRUE}).}
#'   }
#'
#' @return A named list with elements \code{effect}, \code{scaling}, and
#'   \code{consider_type}, consumed by \code{\link{duremstats}}.
#'
#' @seealso \code{\link{duremstats}}
#' @name active_effects
NULL

.active_effect_cfg <- function(name, scaling, consider_type) {
    scaling <- match.arg(scaling, c("none", "std"))
    .validate_consider_type_durem(consider_type)
    list(effect = name, scaling = scaling, consider_type = consider_type)
}

#' @rdname active_effects
#' @export
activeTie <- function(scaling = c("none", "std"),
                      consider_type = "ignore")
    .active_effect_cfg("activeTie", scaling, consider_type)

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
activeDegreeActor1 <- function(scaling = c("none", "std"),
                                consider_type = "ignore")
    .active_effect_cfg("activeDegreeActor1", scaling, consider_type)

#' @rdname active_effects
#' @export
activeDegreeActor2 <- function(scaling = c("none", "std"),
                                consider_type = "ignore")
    .active_effect_cfg("activeDegreeActor2", scaling, consider_type)

#' @rdname active_effects
#' @export
activeSharedPartners <- function(scaling = c("none", "std"),
                                 consider_type = "ignore")
    .active_effect_cfg("activeSharedPartners", scaling, consider_type)
