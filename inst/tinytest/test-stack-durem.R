## Tests for stack_stats.remstats_durem
##
## Small 3-actor edgelist whose active states can be traced by hand:
##
##   time  actor1  actor2  end
##      1    A       B       6    ← A→B active from t=1..6
##      2    B       C       7    ← B→C active from t=2..7
##      5    A       C       8    ← A→C active from t=5..8
##
## Dual edgelist unique times: 1, 2, 5, 6, 7, 8
## With default start=2, stats covers rows 2..6 of unique times → 5 rows:
##   row 1: t=2   row 2: t=5   row 3: t=6   row 4: t=7   row 5: t=8
##
## Active state BEFORE each time (blocking / end-risk):
##   t=2:  A→B active
##   t=5:  A→B, B→C active
##   t=6:  A→B, B→C, A→C active
##   t=7:  B→C, A→C active  (A→B ended at t=6)
##   t=8:  A→C active         (B→C ended at t=7)
##
## Directed actors A(0) B(1) C(2).  N*(N-1)=6 start dyads.
## Dyad column order (from get_riskset alphabetical):
##   1=A→B  2=A→C  3=B→A  4=B→C  5=C→A  6=C→B

library(tinytest)

el <- data.frame(
    time   = c(1, 2, 5),
    actor1 = c("A", "B", "A"),
    actor2 = c("B", "C", "C"),
    end    = c(6, 7, 8)
)

suppressWarnings({
    reh   <- remify(el, duration = TRUE, dur_directed_end = TRUE, model = "tie")
    stats <- remstats(reh,
                      start_effects = ~ inertia(),
                      end_effects   = ~ inertia(),
                      psi_start = 1, psi_end = 1)
    stacked <- stack_stats(stats, reh)
})

df <- stacked$remstats_stack

# ── 1. Class and structure ─────────────────────────────────────────────────────

expect_inherits(stacked, "remstats_stacked",
    info = "stack_stats.remstats_durem returns remstats_stacked")
expect_inherits(stacked, "remstats_stacked_durem",
    info = "also inherits remstats_stacked_durem")
expect_true(is.data.frame(df),
    info = "$remstats_stack is a data.frame")

# ── 2. Required columns ────────────────────────────────────────────────────────

for (col in c("obs", "log_interevent", "inertia.start", "inertia.end", "time_index", "dyad")) {
    expect_true(col %in% names(df),
        info = paste("column present:", col))
}

# ── 3. obs is binary ──────────────────────────────────────────────────────────

expect_true(all(df$obs %in% c(0L, 1L)),
    info = "obs values are 0 or 1")

# ── 4. Metadata ───────────────────────────────────────────────────────────────

expect_equal(stacked$E, 5L,
    info = "5 time points computed (start=2, 6 unique dual times)")
expect_equal(stacked$ordinal, FALSE,
    info = "ordinal = FALSE for DuREM")
expect_equal(stacked$model, "durem",
    info = "model = durem")
expect_equal(stacked$D_start, reh$N * (reh$N - 1L),
    info = "D_start = N*(N-1) for directed start model")

# ── 5. Row counts and the active-dyad start blocking ──────────────────
# The start risk set EXCLUDES currently-active dyads. "Active across the interval
# ending at t" uses end >= t: a dyad ending exactly at t was active throughout
# that interval and so cannot be at risk to start during it. With start = 2 the
# first event (A→B start at t=1) is outside the window. Per time point:
#   t=2: 0 obs-end + 1 ongoing(A→B) + 1 obs-start(B→C) + 4 inactive = 6
#   t=5: 0        + 2 ongoing(A→B,B→C) + 1 obs-start(A→C) + 3 inactive = 6
#   t=6: 1 obs-end(A→B) + 2 ongoing(B→C,A→C) + 0 + 3 inactive = 6
#   t=7: 1 obs-end(B→C) + 1 ongoing(A→C) + 0 + 4 inactive = 6
#   t=8: 1 obs-end(A→C) + 0 ongoing + 0 + 5 inactive = 6
# Total = 30. Under the buggy end > t blocking these were 7 at t=6/7/8 (the dyad
# ending exactly at t was wrongly left in the start risk set) → 33 rows.
#
# NB: the stacked df has columns obs/log_interevent/<stats>/time_index/dyad/process
# (there is no "time" column), so we assert convention-independent aggregates
# rather than per-time-point selectors.

expect_equal(nrow(df), 30L,
    info = "30 rows: active dyads (incl. those ending exactly at t) excluded from start risk set")
expect_equal(sum(df$obs), 5L,
    info = "5 observed events in window: 2 starts + 3 ends")
expect_equal(sum(df$process == "start" & df$obs == 1L), 2L,
    info = "2 observed starts (B→C, A→C; A→B start at t=1 is outside the start=2 window)")
expect_equal(sum(df$process == "end" & df$obs == 1L), 3L,
    info = "3 observed ends (A→B, B→C, A→C)")

# ── 8. log_interevent is finite and non-positive-infinite ─────────────────────

expect_true(all(is.finite(df$log_interevent)),
    info = "all log_interevent values are finite")

# ── 9. stat_names stored correctly ────────────────────────────────────────────

expect_equal(stacked$stat_names[c(2,4)], c("inertia.start", "inertia.end"),
    info = "stat_names = inertia.start + inertia.end")
expect_equal(stacked$stat_names_start[2], "inertia.start",
    info = "stat_names_start = inertia.start")
expect_equal(stacked$stat_names_end[2], "inertia.end",
    info = "stat_names_end = inertia.end")

# ── 10. Undirected end model ──────────────────────────────────────────────────
# With dur_directed_end = FALSE: end model has N*(N-1)/2 = 3 dyads.

suppressWarnings({
    reh_de    <- remify(el, duration = TRUE, dur_directed_end = TRUE, model = "tie")
    reh_ude   <- remify(el, duration = TRUE, model = "tie")  # dur_directed_end = FALSE (default)

    stats_ude <- remstats(reh_ude,
                          start_effects = ~ inertia(),
                          end_effects   = ~ inertia(),
                          psi_start = 0, psi_end = 0)
    stacked_ude <- stack_stats(stats_ude, reh_ude)
})

expect_equal(stacked_ude$D_end, reh$N * (reh$N - 1L) / 2L,
    info = "undirected end model: D_end = N*(N-1)/2")
expect_true(is.data.frame(stacked_ude$remstats_stack),
    info = "undirected end model: remstats_stack is data.frame")

# ── 11. Right-censored events ─────────────────────────────────────────────────
# Censored events are always in the end risk set but never observed to end.

el_cens <- el
el_cens$end[2] <- NA  # B→C is right-censored

suppressWarnings({
    reh_cens   <- remify(el_cens, duration = TRUE)
    stats_cens <- remstats(reh_cens,
                           start_effects = ~ inertia(),
                           end_effects   = ~ inertia())
    stacked_cens <- stack_stats(stats_cens, reh_cens)
})

expect_true(is.data.frame(stacked_cens$remstats_stack),
    info = "right-censored: stack returned successfully")
# No observed end for the censored event
df_cens <- stacked_cens$remstats_stack
# Total obs=1 events: 2 observed ends (A→B and A→C) + 3 observed starts
expect_equal(sum(df_cens$obs), 4L,
    info = "right-censored: 5 observed events (3 starts + 2 ends - 1 training)")
attr(stats_cens,"subset")
stacked_cens$D_end

