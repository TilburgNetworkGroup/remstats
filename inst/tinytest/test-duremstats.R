## Tests for duremstats() — active-state statistics for DuREM
##
## Strategy: use a small edgelist whose active state can be traced by hand,
## then assert specific cell values (not just "non-zero" or "differs").
##
## Test edgelist (directed, 3 actors A/B/C):
##
##   time  actor1  actor2  end
##      1    A       B      10
##      2    A       C      10
##      3    B       C      10
##      6    C       A      10
##
## Dual edgelist unique times: 1, 2, 3, 6, 10  (5 time points, rows 1–5
## when start=1 is passed to duremstats).
##
## Active state BEFORE each event (stats computed from history strictly
## before t, consistent with tomstats convention):
##
##   Row 1 (t=1):  nothing active
##   Row 2 (t=2):  A→B
##   Row 3 (t=3):  A→B, A→C
##   Row 4 (t=6):  A→B, A→C, B→C
##   Row 5 (t=10): A→B, A→C, B→C, C→A
##
## Derived quantities used below (all actors 0-based for C++, A=0 B=1 C=2):
##
##   Row 4:  active_out=[A:2,B:1,C:0]  active_in=[A:0,B:1,C:2]
##   Row 5:  active_out=[A:2,B:1,C:1]  active_in=[A:1,B:1,C:2]
##
## Dyad ordering (directed, from get_riskset with actorIDs 0,1,2):
##   col 1 = A→B   col 2 = A→C   col 3 = B→A
##   col 4 = B→C   col 5 = C→A   col 6 = C→B

library(tinytest)

# ── shared setup ──────────────────────────────────────────────────────────────

el <- data.frame(
    time   = c(1, 2, 3, 6),
    actor1 = c("A", "A", "B", "C"),
    actor2 = c("B", "C", "C", "A"),
    end    = c(10, 10, 10, 10)
)

suppressWarnings(reh <- remify(el, duration = TRUE))

# Build the riskset matrix the same way duremstats does, so we can look up
# dyad column indices by actor-pair name.
N          <- reh$N
actor_dict <- reh$meta$dictionary$actors
actor_ids  <- setNames(actor_dict$actorID - 1L, actor_dict$actorName)  # 0-based

riskset_mat <- convert_to_risksetMatrix(
    get_riskset(as.integer(seq_len(N) - 1L), 0L, directed = TRUE),
    N = N, C = 1L
)

# Helper: R column index for dyad from→to (1-based, for use in stats[row, col, slice])
dyad_col <- function(from, to) {
    r <- actor_ids[from] + 1L   # 0-based ID + 1 = R row index
    c <- actor_ids[to]   + 1L
    as.integer(riskset_mat[r, c]) + 1L   # dyad_id is 0-based; +1 for R
}

AB <- dyad_col("A", "B"); AC <- dyad_col("A", "C")
BA <- dyad_col("B", "A"); BC <- dyad_col("B", "C")
CA <- dyad_col("C", "A"); CB <- dyad_col("C", "B")

# Compute all directed active-state statistics (start=1 → all 5 rows)
suppressWarnings({
    s_tie  <- duremstats(reh, start_effects = ~ activeTie(),
                         start = 1L, stop = Inf)$start_stats
    s_ods  <- duremstats(reh, start_effects = ~ activeOutdegreeSender(),
                         start = 1L, stop = Inf)$start_stats
    s_idr  <- duremstats(reh, start_effects = ~ activeIndegreeReceiver(),
                         start = 1L, stop = Inf)$start_stats
    s_tds  <- duremstats(reh, start_effects = ~ activeTotaldegreeSender(),
                         start = 1L, stop = Inf)$start_stats
    s_tdr  <- duremstats(reh, start_effects = ~ activeTotaldegreeReceiver(),
                         start = 1L, stop = Inf)$start_stats
    s_otp  <- duremstats(reh, start_effects = ~ activeSharedPartners_otp(),
                         start = 1L, stop = Inf)$start_stats
    s_itp  <- duremstats(reh, start_effects = ~ activeSharedPartners_itp(),
                         start = 1L, stop = Inf)$start_stats
    s_osp  <- duremstats(reh, start_effects = ~ activeSharedPartners_osp(),
                         start = 1L, stop = Inf)$start_stats
    s_isp  <- duremstats(reh, start_effects = ~ activeSharedPartners_isp(),
                         start = 1L, stop = Inf)$start_stats
})

# ── 1. Output structure ───────────────────────────────────────────────────────

expect_true(is.array(s_tie),
    info = "start_stats is an array")
expect_equal(length(dim(s_tie)), 3L,
    info = "start_stats is 3-dimensional [M x D x P]")
expect_equal(dim(s_tie)[1], 5L,
    info = "5 time points (start=1, 4 start + 4 end events = 5 unique times)")
expect_equal(dim(s_tie)[2], N * (N - 1L),
    info = "D = N*(N-1) dyads for directed network")
expect_equal(dim(s_tie)[3], 1L,
    info = "1 effect requested")
expect_true(endsWith(dimnames(s_tie)[[3]], ".start"),
    info = "effect name suffixed .start")

# ── 2. activeTie — row 4 (t=6): A→B, A→C, B→C active ────────────────────────
# Expected: A→B=1, A→C=1, B→A=0, B→C=1, C→A=0, C→B=0

expect_equal(unname(s_tie[4, AB, 1]), 1, info = "activeTie row4 A→B = 1 (active)")
expect_equal(unname(s_tie[4, AC, 1]), 1, info = "activeTie row4 A→C = 1 (active)")
expect_equal(unname(s_tie[4, BA, 1]), 0, info = "activeTie row4 B→A = 0 (not active)")
expect_equal(unname(s_tie[4, BC, 1]), 1, info = "activeTie row4 B→C = 1 (active)")
expect_equal(unname(s_tie[4, CA, 1]), 0, info = "activeTie row4 C→A = 0 (not yet active)")
expect_equal(unname(s_tie[4, CB, 1]), 0, info = "activeTie row4 C→B = 0 (not active)")

# row 5 (t=10): all four active
expect_equal(unname(s_tie[5, AB, 1]), 1, info = "activeTie row5 A→B = 1")
expect_equal(unname(s_tie[5, CA, 1]), 1, info = "activeTie row5 C→A = 1 (now active)")
expect_equal(unname(s_tie[5, CB, 1]), 0, info = "activeTie row5 C→B = 0 (never started)")

# row 1 (t=1): nothing active yet
expect_equal(unname(s_tie[1, AB, 1]), 0, info = "activeTie row1 A→B = 0 (not yet started)")

# ── 3. activeOutdegreeSender — row 4 ─────────────────────────────────────────
# active_out = [A:2, B:1, C:0]
# A→B: sender=A → 2;  B→A: sender=B → 1;  C→A: sender=C → 0

expect_equal(unname(s_ods[4, AB, 1]), 2, info = "activeOutdegreeSender row4 A→B = 2 (A sends 2)")
expect_equal(unname(s_ods[4, AC, 1]), 2, info = "activeOutdegreeSender row4 A→C = 2 (same sender)")
expect_equal(unname(s_ods[4, BA, 1]), 1, info = "activeOutdegreeSender row4 B→A = 1 (B sends 1)")
expect_equal(unname(s_ods[4, BC, 1]), 1, info = "activeOutdegreeSender row4 B→C = 1 (same sender)")
expect_equal(unname(s_ods[4, CA, 1]), 0, info = "activeOutdegreeSender row4 C→A = 0 (C sends 0)")
expect_equal(unname(s_ods[4, CB, 1]), 0, info = "activeOutdegreeSender row4 C→B = 0 (same sender)")

# ── 4. activeIndegreeReceiver — row 4 ────────────────────────────────────────
# active_in = [A:0, B:1, C:2]
# A→B: receiver=B → 1;  A→C: receiver=C → 2;  B→A: receiver=A → 0

expect_equal(unname(s_idr[4, AB, 1]), 1, info = "activeIndegreeReceiver row4 A→B = 1 (B receives 1)")
expect_equal(unname(s_idr[4, AC, 1]), 2, info = "activeIndegreeReceiver row4 A→C = 2 (C receives 2)")
expect_equal(unname(s_idr[4, BA, 1]), 0, info = "activeIndegreeReceiver row4 B→A = 0 (A receives 0)")
expect_equal(unname(s_idr[4, BC, 1]), 2, info = "activeIndegreeReceiver row4 B→C = 2 (C receives 2)")
expect_equal(unname(s_idr[4, CA, 1]), 0, info = "activeIndegreeReceiver row4 C→A = 0 (A receives 0)")
expect_equal(unname(s_idr[4, CB, 1]), 1, info = "activeIndegreeReceiver row4 C→B = 1 (B receives 1)")

# ── 5. activeTotaldegreeSender — row 5 ───────────────────────────────────────
# active_out=[A:2,B:1,C:1]  active_in=[A:1,B:1,C:2]
# totaldeg: A=3, B=2, C=3

expect_equal(unname(s_tds[5, AB, 1]), 3, info = "activeTotaldegreeSender row5 A→B = 3 (A total=3)")
expect_equal(unname(s_tds[5, AC, 1]), 3, info = "activeTotaldegreeSender row5 A→C = 3")
expect_equal(unname(s_tds[5, BA, 1]), 2, info = "activeTotaldegreeSender row5 B→A = 2 (B total=2)")
expect_equal(unname(s_tds[5, BC, 1]), 2, info = "activeTotaldegreeSender row5 B→C = 2")
expect_equal(unname(s_tds[5, CA, 1]), 3, info = "activeTotaldegreeSender row5 C→A = 3 (C total=3)")
expect_equal(unname(s_tds[5, CB, 1]), 3, info = "activeTotaldegreeSender row5 C→B = 3")

# ── 6. activeTotaldegreeReceiver — row 5 ─────────────────────────────────────
# receiver total degrees: A=3, B=2, C=3

expect_equal(unname(s_tdr[5, AB, 1]), 2, info = "activeTotaldegreeReceiver row5 A→B = 2 (B total=2)")
expect_equal(unname(s_tdr[5, AC, 1]), 3, info = "activeTotaldegreeReceiver row5 A→C = 3 (C total=3)")
expect_equal(unname(s_tdr[5, BA, 1]), 3, info = "activeTotaldegreeReceiver row5 B→A = 3 (A total=3)")
expect_equal(unname(s_tdr[5, BC, 1]), 3, info = "activeTotaldegreeReceiver row5 B→C = 3 (C total=3)")
expect_equal(unname(s_tdr[5, CA, 1]), 3, info = "activeTotaldegreeReceiver row5 C→A = 3 (A total=3)")
expect_equal(unname(s_tdr[5, CB, 1]), 2, info = "activeTotaldegreeReceiver row5 C→B = 2 (B total=2)")

# ── 7. activeSharedPartners_otp — row 5 ──────────────────────────────────────
# Active: A→B, A→C, B→C, C→A
# otp(i→j) = #h: i→h AND h→j active
#   A→B: h=C: A→C yes, C→B no  → 0
#   A→C: h=B: A→B yes, B→C yes → 1
#   B→A: h=C: B→C yes, C→A yes → 1
#   B→C: h=A: B→A no           → 0
#   C→A: h=B: C→B no           → 0
#   C→B: h=A: C→A yes, A→B yes → 1

expect_equal(unname(s_otp[5, AB, 1]), 0, info = "otp row5 A→B = 0")
expect_equal(unname(s_otp[5, AC, 1]), 1, info = "otp row5 A→C = 1 (path via B)")
expect_equal(unname(s_otp[5, BA, 1]), 1, info = "otp row5 B→A = 1 (path via C)")
expect_equal(unname(s_otp[5, BC, 1]), 0, info = "otp row5 B→C = 0")
expect_equal(unname(s_otp[5, CA, 1]), 0, info = "otp row5 C→A = 0")
expect_equal(unname(s_otp[5, CB, 1]), 1, info = "otp row5 C→B = 1 (path via A)")

# ── 8. activeSharedPartners_itp — row 5 ──────────────────────────────────────
# itp(i→j) = #h: j→h AND h→i active
#   A→B: h=C: B→C yes, C→A yes → 1
#   A→C: h=B: C→B no           → 0
#   B→A: h=C: A→C yes, C→B no  → 0
#   B→C: h=A: C→A yes, A→B yes → 1
#   C→A: h=B: A→B yes, B→C yes → 1 (wait: j=A, j→h = A→h; h→i = h→C)
#     actually itp(C→A): j=A, i=C → #h: A→h AND h→C active
#     h=B: A→B yes, B→C yes → 1
#   C→B: h=A: B→A no           → 0

expect_equal(unname(s_itp[5, AB, 1]), 1, info = "itp row5 A→B = 1 (path B→C→A)")
expect_equal(unname(s_itp[5, AC, 1]), 0, info = "itp row5 A→C = 0")
expect_equal(unname(s_itp[5, BA, 1]), 0, info = "itp row5 B→A = 0")
expect_equal(unname(s_itp[5, BC, 1]), 1, info = "itp row5 B→C = 1 (path C→A→B)")
expect_equal(unname(s_itp[5, CA, 1]), 1, info = "itp row5 C→A = 1 (path A→B→C)")
expect_equal(unname(s_itp[5, CB, 1]), 0, info = "itp row5 C→B = 0")

# ── 9. activeSharedPartners_osp — row 5 ──────────────────────────────────────
# osp(i→j) = #h: i→h AND j→h active
#   A→B: h=C: A→C yes, B→C yes → 1
#   A→C: h=B: A→B yes, C→B no  → 0
#   B→A: h=C: B→C yes, A→C yes → 1
#   B→C: h=A: B→A no           → 0
#   C→A: h=B: C→B no           → 0
#   C→B: h=A: C→A yes, B→A no  → 0

expect_equal(unname(s_osp[5, AB, 1]), 1, info = "osp row5 A→B = 1 (shared C as target)")
expect_equal(unname(s_osp[5, AC, 1]), 0, info = "osp row5 A→C = 0")
expect_equal(unname(s_osp[5, BA, 1]), 1, info = "osp row5 B→A = 1 (shared C as target)")
expect_equal(unname(s_osp[5, BC, 1]), 0, info = "osp row5 B→C = 0")
expect_equal(unname(s_osp[5, CA, 1]), 0, info = "osp row5 C→A = 0")
expect_equal(unname(s_osp[5, CB, 1]), 0, info = "osp row5 C→B = 0")

# ── 10. activeSharedPartners_isp — row 5 ─────────────────────────────────────
# isp(i→j) = #h: h→i AND h→j active
#   A→B: h=C: C→A yes, C→B no  → 0
#   A→C: h=B: B→A no           → 0
#   B→A: h=C: C→B no           → 0
#   B→C: h=A: A→B yes, A→C yes → 1
#   C→A: h=B: B→C yes, B→A no  → 0
#   C→B: h=A: A→C yes, A→B yes → 1

expect_equal(unname(s_isp[5, AB, 1]), 0, info = "isp row5 A→B = 0")
expect_equal(unname(s_isp[5, AC, 1]), 0, info = "isp row5 A→C = 0")
expect_equal(unname(s_isp[5, BA, 1]), 0, info = "isp row5 B→A = 0")
expect_equal(unname(s_isp[5, BC, 1]), 1, info = "isp row5 B→C = 1 (A sends to both B and C)")
expect_equal(unname(s_isp[5, CA, 1]), 0, info = "isp row5 C→A = 0")
expect_equal(unname(s_isp[5, CB, 1]), 1, info = "isp row5 C→B = 1 (A sends to both C and B)")

# ── 11. Nothing active at row 1 ───────────────────────────────────────────────

for (stat in list(s_tie, s_ods, s_idr, s_tds, s_tdr,
                  s_otp, s_itp, s_osp, s_isp)) {
    expect_true(all(stat[1, , 1] == 0),
        info = "all active-state stats zero at row 1 (nothing active yet)")
}

# ── 12. Undirected active-state statistics ────────────────────────────────────
# Edgelist (undirected, actors A/B/C):
#   time  actor1  actor2  end
#      1    A       B       8
#      2    A       C       8
#      3    B       C       8
#
# Unique dual times: 1, 2, 3, 8  (4 rows with start=1)
# Active state BEFORE each event:
#   Row 1 (t=1):  nothing
#   Row 2 (t=2):  A-B
#   Row 3 (t=3):  A-B, A-C
#   Row 4 (t=8):  A-B, A-C, B-C
#
# active_degree at row 3: [A:2, B:1, C:1]
# active_degree at row 4: [A:2, B:2, C:2]
#
# Undirected canonical dyads (i<j, IDs 0,1,2): A-B=0, A-C=1, B-C=2
# In R columns: A-B=1, A-C=2, B-C=3

el_ud <- data.frame(
    time   = c(1, 2, 3),
    actor1 = c("A", "A", "B"),
    actor2 = c("B", "C", "C"),
    end    = c(8, 8, 8)
)

suppressWarnings(reh_ud <- remify(el_ud, duration = TRUE, directed = FALSE))

riskset_mat_ud <- convert_to_risksetMatrix(
    get_riskset(as.integer(seq_len(N) - 1L), 0L, directed = FALSE),
    N = N, C = 1L
)

dyad_col_ud <- function(a, b) {
    lo <- min(actor_ids[a], actor_ids[b]) + 1L  # canonical: smaller index
    hi <- max(actor_ids[a], actor_ids[b]) + 1L
    as.integer(riskset_mat_ud[lo, hi]) + 1L
}

d_AB <- dyad_col_ud("A", "B")
d_AC <- dyad_col_ud("A", "C")
d_BC <- dyad_col_ud("B", "C")

suppressWarnings({
    u_tie  <- duremstats(reh_ud, start_effects = ~ activeTie(),
                         start = 1L)$start_stats
    u_deg1 <- duremstats(reh_ud, start_effects = ~ activeDegreeActor1(),
                         start = 1L)$start_stats
    u_deg2 <- duremstats(reh_ud, start_effects = ~ activeDegreeActor2(),
                         start = 1L)$start_stats
    u_sp   <- duremstats(reh_ud, start_effects = ~ activeSharedPartners(),
                         start = 1L)$start_stats
})

# activeTie row 3 (t=3): A-B active, A-C active, B-C not yet
expect_equal(unname(u_tie[3, d_AB, 1]), 1, info = "ud activeTie row3 A-B = 1")
expect_equal(unname(u_tie[3, d_AC, 1]), 1, info = "ud activeTie row3 A-C = 1")
expect_equal(unname(u_tie[3, d_BC, 1]), 0, info = "ud activeTie row3 B-C = 0")

# activeTie row 4 (t=8): all three active
expect_equal(unname(u_tie[4, d_AB, 1]), 1, info = "ud activeTie row4 A-B = 1")
expect_equal(unname(u_tie[4, d_AC, 1]), 1, info = "ud activeTie row4 A-C = 1")
expect_equal(unname(u_tie[4, d_BC, 1]), 1, info = "ud activeTie row4 B-C = 1")

# activeDegreeActor1 row 3: active_degree=[A:2,B:1,C:1]
# A-B: actor1=A (lower ID) → degree[A]=2
# A-C: actor1=A           → degree[A]=2
# B-C: actor1=B           → degree[B]=1
expect_equal(unname(u_deg1[3, d_AB, 1]), 2, info = "ud activeDegreeActor1 row3 A-B = 2")
expect_equal(unname(u_deg1[3, d_AC, 1]), 2, info = "ud activeDegreeActor1 row3 A-C = 2")
expect_equal(unname(u_deg1[3, d_BC, 1]), 1, info = "ud activeDegreeActor1 row3 B-C = 1")

# activeDegreeActor2 row 3:
# A-B: actor2=B → degree[B]=1
# A-C: actor2=C → degree[C]=1
# B-C: actor2=C → degree[C]=1
expect_equal(unname(u_deg2[3, d_AB, 1]), 1, info = "ud activeDegreeActor2 row3 A-B = 1")
expect_equal(unname(u_deg2[3, d_AC, 1]), 1, info = "ud activeDegreeActor2 row3 A-C = 1")
expect_equal(unname(u_deg2[3, d_BC, 1]), 1, info = "ud activeDegreeActor2 row3 B-C = 1")

# activeSharedPartners row 4 (all 3 active):
# sp(A-B): #h: (A,h) AND (B,h) active → h=C: A-C yes, B-C yes → 1
# sp(A-C): #h: (A,h) AND (C,h) active → h=B: A-B yes, B-C yes → 1
# sp(B-C): #h: (B,h) AND (C,h) active → h=A: A-B yes, A-C yes → 1
expect_equal(unname(u_sp[4, d_AB, 1]), 1, info = "ud activeSharedPartners row4 A-B = 1")
expect_equal(unname(u_sp[4, d_AC, 1]), 1, info = "ud activeSharedPartners row4 A-C = 1")
expect_equal(unname(u_sp[4, d_BC, 1]), 1, info = "ud activeSharedPartners row4 B-C = 1")

# nothing active at row 1
expect_true(all(u_tie[1, , 1] == 0),
    info = "ud: all zero at row 1 (nothing active yet)")
expect_true(all(u_sp[1, , 1] == 0),
    info = "ud: shared partners all zero at row 1")

# ── 13. end_effects produce $end_stats with .end suffix ──────────────────────
# reh has directed_end = FALSE (default), so end model is undirected.
# activeDegreeActor1 is the second undirected effect (stat_type 2).

suppressWarnings({
    both <- duremstats(reh,
                       start_effects = ~ activeTie(),
                       end_effects   = ~ activeTie() + activeDegreeActor1(),
                       start = 1L)
})

expect_true(!is.null(both$start_stats), info = "start_stats present")
expect_true(!is.null(both$end_stats),   info = "end_stats present")
expect_equal(dim(both$start_stats)[3], 1L, info = "start_stats has 1 effect")
expect_equal(dim(both$end_stats)[3],   2L, info = "end_stats has 2 effects")
expect_true(all(endsWith(dimnames(both$start_stats)[[3]], ".start")),
    info = "start effect names end in .start")
expect_true(all(endsWith(dimnames(both$end_stats)[[3]], ".end")),
    info = "end effect names end in .end")

# end stats tie values should equal start stats tie values (same reh, same history).
# AB = 1 by both directed and undirected dyad ordering (A-B is dyad 0 in both),
# so it is a valid column index into the undirected end_stats (D = 3).
expect_equal(unname(both$end_stats[5, AB, 1]), unname(s_tie[5, AB, 1]),
    info = "end_stats activeTie matches start_stats (same event history)")

# ── 14. unknown effect name triggers error ────────────────────────────────────

expect_error(
    duremstats(reh, start_effects = ~ notAnEffect()),
    pattern = "Unknown active-state effect",
    info = "unknown effect name raises error"
)

# ── 15. output is a remstats_durem object ─────────────────────────────────────

expect_inherits(both, "remstats_durem",
    info = "duremstats returns remstats_durem class")
expect_true(is.remstats_durem(both),
    info = "is.remstats_durem TRUE for duremstats output")

# ── 16. consider_type = "separate" with typed events ─────────────────────────
# Two types X and Y.  At row 4 (t=6): A→B(X), A→C(X), B→C(Y) active.
# activeTie.X: A→B=1, A→C=1, B→C=0
# activeTie.Y: A→B=0, A→C=0, B→C=1

el_typed <- data.frame(
    time   = c(1, 2, 3, 6),
    actor1 = c("A", "A", "B", "C"),
    actor2 = c("B", "C", "C", "A"),
    end    = c(10, 10, 10, 10),
    type   = c("X", "X", "Y", "Y")
)

suppressWarnings(reh_typed <- remify(el_typed, duration = TRUE))

suppressWarnings(
    s_sep <- remstats(reh_typed,
                        start_effects = ~ activeTie(consider_type = "separate"),
                        start = 1L, stop = Inf)$start_stats
)

# Should produce 2 statistics: activeTie.X.start and activeTie.Y.start
expect_equal(dim(s_sep)[3], 2L,
    info = "consider_type='separate' produces C=2 statistics")
expect_true(any(grepl("\\.X\\.start$", dimnames(s_sep)[[3]])),
    info = "activeTie.X.start present")
expect_true(any(grepl("\\.Y\\.start$", dimnames(s_sep)[[3]])),
    info = "activeTie.Y.start present")

# Extract type-specific slices
idx_X <- grep("\\.X\\.start$", dimnames(s_sep)[[3]])
idx_Y <- grep("\\.Y\\.start$", dimnames(s_sep)[[3]])

# Row 4 (t=6): A→B(X) and A→C(X) active; B→C(Y) active; C→A not yet active
expect_equal(s_sep[4, AB, idx_X], 1,
    info = "activeTie.X row4 A→B = 1 (X event active)")
expect_equal(s_sep[4, AC, idx_X], 1,
    info = "activeTie.X row4 A→C = 1 (X event active)")
expect_equal(s_sep[4, BC, idx_X], 0,
    info = "activeTie.X row4 B→C = 0 (B→C is type Y, not X)")
expect_equal(s_sep[4, BC, idx_Y], 1,
    info = "activeTie.Y row4 B→C = 1 (Y event active)")
expect_equal(s_sep[4, AB, idx_Y], 0,
    info = "activeTie.Y row4 A→B = 0 (A→B is type X, not Y)")

# ── 17. consider_type = "interact" with typed events ─────────────────────────
# Two types X and Y.  "interact" produces a SINGLE statistic with D*C columns.
# Column layout: type X in cols 1..D, type Y in cols (D+1)..(2*D)
# (types are sorted alphabetically: X before Y).
#
# At row 4 (t=6): A→B(X), A→C(X), B→C(Y) active; C→A not yet active.
# Type-X block: A→B=1, A→C=1, B→C=0, others=0
# Type-Y block: A→B=0, A→C=0, B→C=1, others=0

suppressWarnings(
    s_int <- duremstats(reh_typed,
                        start_effects = ~ activeTie(consider_type = "interact"),
                        start = 1L, stop = Inf)$start_stats
)

D_base <- N * (N - 1L)   # directed, 3 actors → 6 dyads

# Single output statistic with D*C = 12 columns
expect_equal(dim(s_int)[3], 1L,
    info = "consider_type='interact' produces 1 output statistic")
expect_equal(dim(s_int)[2], D_base * 2L,
    info = "consider_type='interact' produces D*C = 12 columns")
expect_true(endsWith(dimnames(s_int)[[3]][1], ".start"),
    info = "interact stat name ends in .start (no type suffix)")

# Type-X block (columns 1..D_base): A→B=1, A→C=1, B→C=0 at row 4
expect_equal(s_int[4, AB, 1], 1,
    info = "interact row4 A→B (X-block) = 1")
expect_equal(s_int[4, AC, 1], 1,
    info = "interact row4 A→C (X-block) = 1")
expect_equal(s_int[4, BC, 1], 0,
    info = "interact row4 B→C (X-block) = 0 (B→C is type Y)")

# Type-Y block (columns D_base+1 .. 2*D_base)
AB_Y <- AB + D_base
AC_Y <- AC + D_base
BC_Y <- BC + D_base

expect_equal(s_int[4, BC_Y, 1], 1,
    info = "interact row4 B→C (Y-block) = 1")
expect_equal(s_int[4, AB_Y, 1], 0,
    info = "interact row4 A→B (Y-block) = 0 (A→B is type X)")

# Row 1: nothing active in either block
expect_true(all(s_int[1, , 1] == 0),
    info = "interact: all zero at row 1 (nothing active)")

# "interact" on an untyped reh degrades silently to "ignore"
suppressWarnings(
    s_int_ud <- duremstats(reh,
                           start_effects = ~ activeTie(consider_type = "interact"),
                           start = 1L, stop = Inf)$start_stats
)
expect_equal(dim(s_int_ud)[2], D_base,
    info = "interact on untyped reh degrades to ignore (D = D_base)")

# ── 17b. invalid consider_type raises error ───────────────────────────────────

expect_error(
    activeTie(consider_type = "nonsense"),
    pattern = "not supported",
    info = "invalid consider_type raises error"
)

# ── 18. extend_riskset_by_type = TRUE increases D ────────────────────────────
# With C=2 types and extend_riskset_by_type=TRUE, D should be C * N*(N-1) = 12

suppressWarnings(
    reh_typed_ext <- remify(el_typed, duration = TRUE,
                            extend_riskset_by_type = TRUE)
)

suppressWarnings(
    s_ext <- duremstats(reh_typed_ext,
                        start_effects = ~ activeTie(),
                        start = 1L, stop = Inf)$start_stats
)

C_typed <- reh_typed_ext$C
D_typed <- C_typed * N * (N - 1L)

expect_equal(dim(s_ext)[2], D_typed,
    info = "extend_riskset_by_type=TRUE: D = C * N*(N-1)")
expect_equal(dim(s_ext)[2], reh_typed_ext$D,
    info = "D in active-state stats matches reh$D when extend_riskset_by_type=TRUE")
