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
    reh   <- remify(el, duration = TRUE, directed_end = TRUE)
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

for (col in c("obs", "log_interevent", "inertia.start", "inertia.end", "event", "dyad")) {
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

# ── 5. Row counts at t=2 ──────────────────────────────────────────────────────
# Active before t=2: A→B only.
# State 1 (obs end):    none
# State 2 (ongoing):    A→B               → 1 end-model row
# State 3 (obs start):  B→C               → 1 start-model row (obs=1)
# State 4 (inactive):   A→C, B→A, C→A, C→B → 4 start-model rows (obs=0)
# Total: 6 rows at event=1

rows_t2 <- df[df$event == 1L, ]
expect_equal(nrow(rows_t2), 6L,
    info = "6 rows at t=2 (1 ongoing + 1 obs-start + 4 inactive)")

# Exactly one obs=1 start row and zero obs=1 end rows
start_obs_t2 <- rows_t2[rows_t2$obs == 1L & rows_t2$inertia.end == 0, ]
end_obs_t2   <- rows_t2[rows_t2$obs == 1L & rows_t2$inertia.start == 0, ]
expect_equal(nrow(start_obs_t2), 1L,
    info = "exactly 1 observed start at t=2")

# ── 6. Row counts at t=6 ──────────────────────────────────────────────────────
# Active before t=6: A→B, B→C, A→C (all three started, none ended yet)
# State 1 (obs end):   A→B (ended at 6)   → 1 end-model row (obs=1)
# State 2 (ongoing):   B→C, A→C           → 2 end-model rows (obs=0)
# State 3 (obs start): none (no start at t=6)
# State 4 (inactive):  B→A, C→A, C→B     → 3 start-model rows (obs=0)
# Total: 6 rows at event=3

rows_t6 <- df[df$event == 3L, ]
expect_equal(nrow(rows_t6), 6L,
    info = "6 rows at t=6 (1 obs-end + 2 ongoing + 3 inactive)")

end_obs_t6   <- rows_t6[rows_t6$obs == 1L & rows_t6$inertia.start == 0, ]
start_obs_t6 <- rows_t6[rows_t6$obs == 1L & rows_t6$inertia.end   == 0, ]
expect_equal(nrow(end_obs_t6),   1L, info = "exactly 1 observed end at t=6")

# ── 7. Row counts at t=8 ──────────────────────────────────────────────────────
# Active before t=8: A→C only (B→C ended at 7, A→B ended at 6)
# State 1 (obs end):   A→C (ended at 8)  → 1 end-model row (obs=1)
# State 2 (ongoing):   none
# State 3 (obs start): none
# State 4 (inactive):  all 6 start dyads  → but A→C is in end model, not start blocking
#   Actually A→C started at t=5 ≤ t=8 and ends at t=8 (end > t is FALSE for t=8)
#   So A→C is NOT blocking: blocking uses end > t strictly. A→C ends AT t=8, not after.
#   Therefore all 6 directed dyads are in state 4 (inactive start risk).
# Total: 1 + 0 + 0 + 6 = 7 rows at event=5

rows_t8 <- df[df$event == 5L, ]
end_obs_t8 <- rows_t8[rows_t8$obs == 1L, ]
expect_equal(nrow(end_obs_t8), 1L,
    info = "exactly 1 observed end at t=8")

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
# With directed_end = FALSE: end model has N*(N-1)/2 = 3 dyads.

suppressWarnings({
    reh_de    <- remify(el, duration = TRUE, directed_end = TRUE)
    reh_ude   <- remify(el, duration = TRUE)  # directed_end = FALSE (default)

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

