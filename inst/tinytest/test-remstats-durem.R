## Tests for remstats_durem — the remstats() duration dispatch
##
## Covers:
##   - remstats(reh_durem, ...) dispatches to .remstats_durem()
##   - Returns remstats_durem class with $start_stats and $end_stats arrays
##   - Effect name suffixes (.start / .end)
##   - psi_start / psi_end stored and affect statistics
##   - Different effects for start and end models
##   - Directed vs undirected start/end
##   - Right-censored events handled
##   - is.remstats_durem(), print() run without error

library(tinytest)

# ── shared test data ───────────────────────────────────────────────────────────

# 5 events, 3 actors, non-overlapping for simplicity
el <- data.frame(
    time   = c(1,  4,  7, 11, 15),
    actor1 = c("A", "B", "A", "C", "B"),
    actor2 = c("B", "C", "C", "A", "A"),
    end    = c(3,  6,  9, 13, 18)
)

el_cens <- el
el_cens$end[3] <- NA   # one right-censored event

el_ud <- el            # undirected version uses same data

suppressWarnings({
    reh      <- remify(el,      model = "tie", duration = TRUE)
    reh_cens <- remify(el_cens, model = "tie", duration = TRUE)
    reh_ud   <- remify(el,      model = "tie", duration = TRUE, directed = FALSE)
})

# ── 1. Dispatch: remstats on remify_durem returns remstats_durem ───────────────

suppressWarnings({
    stats <- remstats(reh,
                      start_effects = ~ inertia(),
                      end_effects   = ~ inertia())
})

expect_inherits(stats, "remstats_durem",
    info = "remstats on remify_durem returns remstats_durem")
expect_true(is.remstats_durem(stats),
    info = "is.remstats_durem() TRUE")

# ── 2. $start_stats and $end_stats are 3-D arrays ─────────────────────────────

expect_true(is.array(stats$start_stats),
    info = "$start_stats is an array")
expect_equal(length(dim(stats$start_stats)), 3L,
    info = "$start_stats is 3-dimensional")

expect_true(is.array(stats$end_stats),
    info = "$end_stats is an array")
expect_equal(length(dim(stats$end_stats)), 3L,
    info = "$end_stats is 3-dimensional")

# ── 3. Effect name suffixes ───────────────────────────────────────────────────

expect_true(
    all(endsWith(dimnames(stats$start_stats)[[3]], ".start")),
    info = "start effect names end with .start"
)
expect_true(
    all(endsWith(dimnames(stats$end_stats)[[3]], ".end")),
    info = "end effect names end with .end"
)

# ── 4. psi stored correctly ───────────────────────────────────────────────────

suppressWarnings({
    stats_psi <- remstats(reh,
                          start_effects = ~ remstats::inertia(),
                          end_effects   = ~ remstats::inertia(),
                          psi_start     = 2,
                          psi_end       = 0.5)
})

expect_equal(stats_psi$psi_start, 2,   info = "psi_start stored")
expect_equal(stats_psi$psi_end,   0.5, info = "psi_end stored")

# ── 5. psi affects statistics values ─────────────────────────────────────────

suppressWarnings({
    stats_p0 <- remstats(reh,
                         start_effects = ~ remstats::inertia(),
                         end_effects   = ~ remstats::inertia(),
                         psi_start = 0, psi_end = 0)
    stats_p1 <- remstats(reh,
                         start_effects = ~ remstats::inertia(),
                         end_effects   = ~ remstats::inertia(),
                         psi_start = 1, psi_end = 1)
})

expect_false(
    identical(stats_p0$start_stats, stats_p1$start_stats),
    info = "psi_start = 0 vs 1 produces different start statistics"
)

# ── 6. Different number of effects for start and end ─────────────────────────

suppressWarnings({
    stats2 <- remstats(reh,
                       start_effects = ~ remstats::inertia() + remstats::reciprocity(),
                       end_effects   = ~ remstats::inertia())
})

expect_equal(dim(stats2$start_stats)[3], 3L,
    info = "start_stats has 1+2 effects (inertia + reciprocity)")
expect_equal(dim(stats2$end_stats)[3], 2L,
    info = "end_stats has 1+1 effect (inertia)")

# ── 7. attr(stats, "reh") is the remify_durem object ─────────────────────────

expect_true(is.remify_durem(attr(stats, "reh")),
    info = "attr reh is the remify_durem object")

# ── 8. Right-censored events ─────────────────────────────────────────────────
# Censored events have no end row in the dual-event edgelist, so the
# end model sees fewer time points.  The result should still be a valid
# remstats_durem with correct class and structure.

suppressWarnings({
    stats_cens <- remstats(reh_cens,
                           start_effects = ~ remstats::inertia(),
                           end_effects   = ~ remstats::inertia())
})

expect_inherits(stats_cens, "remstats_durem",
    info = "censored data: remstats_durem class returned")
expect_true(is.array(stats_cens$end_stats),
    info = "censored data: $end_stats is still an array")

# end model has fewer time points than start model (one end missing)
expect_true(
    dim(stats_cens$end_stats)[1] < dim(stats$end_stats)[1],
    info = "censored data: end_stats has fewer time points than uncensored"
)

# ── 9. Undirected start ───────────────────────────────────────────────────────

suppressWarnings({
    stats_ud <- remstats(reh_ud,
                         start_effects = ~ remstats::inertia(),
                         end_effects   = ~ remstats::inertia())
})

expect_inherits(stats_ud, "remstats_durem",
    info = "undirected start: remstats_durem returned")

# undirected riskset is smaller than directed
expect_true(
    dim(stats_ud$start_stats)[2] < dim(stats$start_stats)[2],
    info = "undirected start: fewer dyads in start_stats than directed"
)

# ── 10. is.remstats_durem FALSE for regular remstats ─────────────────────────

reh_plain <- suppressWarnings(
    remify(el[, c("time", "actor1", "actor2")], model = "tie")
)
stats_plain <- remstats(reh_plain, tie_effects = ~ remstats::inertia())

expect_false(is.remstats_durem(stats_plain),
    info = "is.remstats_durem FALSE for plain remstats object")

# ── 11. Typed + weighted edgelist ────────────────────────────────────────────
# User's own type column must survive the dual-event expansion unchanged.

el_tw <- el
el_tw$type   <- c("A", "B", "A", "B", "A")
el_tw$weight <- 1:5

suppressWarnings({
    reh_tw    <- remify(el_tw, model = "tie", duration = TRUE)
    stats_tw  <- remstats(reh_tw,
                          start_effects = ~ remstats::inertia(),
                          end_effects   = ~ remstats::inertia())
})

expect_inherits(stats_tw, "remstats_durem",
    info = "typed + weighted: remstats_durem returned")
expect_true(is.array(stats_tw$start_stats),
    info = "typed + weighted: start_stats is array")

# type column must still be present in reh$edgelist (not overwritten)
expect_true("type" %in% names(reh_tw$edgelist),
    info = "typed edgelist: type column preserved in $edgelist")
expect_equal(reh_tw$edgelist$type, as.character(el_tw$type),
    info = "typed edgelist: type values unchanged")

# ── 12. Combined event weights and psi ───────────────────────────────────────
# When the edgelist has user-supplied weights AND psi > 0, the effective history
# weight is event_weight * duration^psi.  Three scenarios must all differ:
#   (a) user weights, psi = 0  → only event weights matter
#   (b) no weights,   psi = 1  → only duration scaling matters
#   (c) user weights, psi = 1  → multiplicative combination

el_w <- el
el_w$weight <- c(1, 2, 3, 4, 5)

suppressWarnings({
    reh_w <- remify(el_w, model = "tie", duration = TRUE)

    # (a) user weights, psi = 0 — duration scaling switched off
    stats_w_p0 <- remstats(reh_w,
                           start_effects = ~ remstats::inertia(),
                           end_effects   = ~ remstats::inertia(),
                           psi_start = 0, psi_end = 0)

    # (b) no user weights, psi = 1 — pure duration scaling
    stats_nw_p1 <- remstats(reh,
                            start_effects = ~ remstats::inertia(),
                            end_effects   = ~ remstats::inertia(),
                            psi_start = 1, psi_end = 1)

    # (c) user weights + psi = 1 — multiplicative combination
    stats_w_p1 <- remstats(reh_w,
                           start_effects = ~ remstats::inertia(),
                           end_effects   = ~ remstats::inertia(),
                           psi_start = 1, psi_end = 1)
})

# (a) vs (c): same weights, different psi — must differ
expect_false(
    identical(stats_w_p0$start_stats, stats_w_p1$start_stats),
    info = "user weights: psi=0 vs psi=1 produces different start statistics"
)

# (b) vs (c): same psi, different weights — must differ
expect_false(
    identical(stats_nw_p1$start_stats, stats_w_p1$start_stats),
    info = "psi=1: with vs without user weights produces different start statistics"
)

# (a) vs (b): different weights AND different psi — must differ
expect_false(
    identical(stats_w_p0$start_stats, stats_nw_p1$start_stats),
    info = "event weights only vs duration scaling only produces different start statistics"
)

# ── 13. sampling = TRUE issues a warning and is ignored ──────────────────────

expect_warning(
    remstats(reh,
                              start_effects = ~ remstats::inertia(),
                              end_effects   = ~ remstats::inertia(),
                              sampling      = TRUE),
    pattern = "sampling",
    info = "sampling=TRUE with remify_durem issues an unsupported warning"
)

# ── 15. print() runs without error ───────────────────────────────────────────

expect_silent(capture.output(print(stats)),
    info = "print.remstats_durem runs without error")

expect_silent(capture.output(print(stats2)),
    info = "print.remstats_durem: different effect counts")

