# ─────────────────────────────────────────────────────────────────────────────
# remstats_durem.R
# Duration Relational Event Model — remstats dispatch
#
# Entry point: remstats(reh, start_effects, end_effects, psi_start, psi_end, ...)
#   when reh is a remify_durem object.
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

    # directional flags
    directed_start <- isTRUE(reh$meta$directed)
    directed_end   <- isTRUE(reh$durem$directed_end)

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
        if (has_weight_col || psi != 0) {
            base_w    <- if (has_weight_col) ed$weight else rep(1, nrow(ed))
            out$weight <- base_w * ed$duration ^ psi
        }
        if ("type" %in% names(ed)) out$type <- ed$type
        out
    }

    # ── Start model ───────────────────────────────────────────────────────────
    suppressWarnings(
        reh_start <- remify(
            edgelist = .make_el(psi_start),
            directed = directed_start,
            model    = "tie",
            actors   = actors
        )
    )

    start_stats <- tomstats(
        effects          = start_effects,
        reh              = reh_start,
        attr_actors      = attr_actors,
        attr_dyads       = attr_dyads,
        memory           = memory,
        memory_value     = memory_value,
        start            = start,
        stop             = stop,
        display_progress = display_progress
    )
    dimnames(start_stats)[[3]] <- paste0(dimnames(start_stats)[[3]], ".start")

    # ── End model ─────────────────────────────────────────────────────────────
    suppressWarnings(
        reh_end <- remify(
            edgelist = .make_el(psi_end),
            directed = directed_end,
            model    = "tie",
            actors   = actors
        )
    )

    end_stats <- tomstats(
        effects          = end_effects,
        reh              = reh_end,
        attr_actors      = attr_actors,
        attr_dyads       = attr_dyads,
        memory           = memory,
        memory_value     = memory_value,
        start            = start,
        stop             = stop,
        display_progress = display_progress
    )
    dimnames(end_stats)[[3]] <- paste0(dimnames(end_stats)[[3]], ".end")

    # ── Assemble remstats_durem object ────────────────────────────────────────
    out <- list(
        start_stats = start_stats,
        end_stats   = end_stats,
        psi_start   = psi_start,
        psi_end     = psi_end
    )
    attr(out, "reh") <- reh
    class(out) <- "remstats_durem"

    out
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
    ss  <- x$start_stats
    es  <- x$end_stats
    reh <- attr(x, "reh")

    cat("Relational Event Network Statistics\n")
    cat("> Model: tie-oriented (duration)\n")
    cat("> Computation method: per time point\n")

    # ── Start model ────────────────────────────────────────────────────────────
    if (!is.null(ss)) {
        d <- dim(ss)
        cat(sprintf("> Start dimensions: %d time points x %d dyads x %d statistics\n",
                    d[1], d[2], d[3]))
        cat("> Start statistics:\n")
        nms <- dimnames(ss)[[3]]
        for (i in seq_along(nms))
            cat(sprintf("\t >> %d: %s\n", i, nms[i]))
    } else {
        cat("> Start statistics: (none)\n")
    }

    # ── End model ──────────────────────────────────────────────────────────────
    if (!is.null(es)) {
        d <- dim(es)
        cat(sprintf("> End dimensions: %d time points x %d dyads x %d statistics\n",
                    d[1], d[2], d[3]))
        cat("> End statistics:\n")
        nms <- dimnames(es)[[3]]
        for (i in seq_along(nms))
            cat(sprintf("\t >> %d: %s\n", i, nms[i]))
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
    out <- list()
    if (!is.null(object$start_stats))
        out$start <- apply(object$start_stats, 3, function(y) summary(as.vector(y)))
    if (!is.null(object$end_stats))
        out$end   <- apply(object$end_stats,   3, function(y) summary(as.vector(y)))
    out
}
