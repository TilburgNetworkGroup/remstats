# test-aomstats-typed.R
#
# Verifies aomstats() with typed events against the tomstats2() tie model
# (extend_riskset_by_type = FALSE) as the ground truth, for multiple stats
# and memory types.
#
# Design:
#   For every valid (event m, receiver j) cell:
#     aomstats$receiver_stats[m, j, stat] ==
#       tomstats2[m, dyad_col(sender_m, j), stat]
#   For sender stats analogously (any reference receiver j != i).
#
# The dyad_col formula (1-based, derived from remify2 internals):
#   d(s, j) = (s-1)*(N-1) + j - as.integer(j > s)
#
# Sections:
#   1  Dimensions and slice names
#   2  Invariants: separate sums to ignore; LOCF; zero before first type event
#   3  Full memory: receiver ignore  vs tie
#   4  Full memory: receiver separate vs tie (consider_type = TRUE)
#   5  Decay memory: receiver ignore  vs tie
#   6  Decay memory: receiver separate vs tie
#   7  Sender stats (outdegreeSender, indegreeSender): full and decay

library(tinytest)
library(remify)
library(remstats)

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------
data(randomREH, package = "remify")
el <- randomREH$edgelist[1:100, ]

reh_actor <- remify2(
  edgelist = el,
  actors   = randomREH$actors,
  directed = TRUE,
  origin   = randomREH$origin,
  model    = "actor"
)

# Tie model with untyped riskset (extend = FALSE) is the reference
reh_tie <- remify2(
  edgelist               = el,
  actors                 = randomREH$actors,
  directed               = TRUE,
  origin                 = randomREH$origin,
  model                  = "tie",
  extend_riskset_by_type = FALSE
)

N          <- length(randomREH$actors)   # number of actors
M          <- nrow(el) - 1L              # aomstats rows  (events 2:100)
tol        <- 1e-10
types      <- sort(unique(el$type))      # competition, conflict, cooperation
actor1_ids <- reh_actor$ids$actor1       # 1-based sender ID for every event

# Directed dyad column (1-based) in the full untyped tie-model riskset.
# Equivalent to remify2's 0-based formula a1*(N-1)+a2-int(a2>a1)+1
# translated to 1-based actor IDs.
dyad_col <- function(s, j, N) (s - 1L) * (N - 1L) + j - as.integer(j > s)

# ---------------------------------------------------------------------------
# Helper: check aomstats receiver_stats[,,stat] == tomstats2[,,stat]
#   for a random sample of events, all receivers per event.
#
# aom_arr : [M x N x P] from aomstats()$receiver_stats
# tie_arr : [M x D x P] from tomstats2()
# stat    : slice name (must exist in both arrays)
# ---------------------------------------------------------------------------
check_aom_tie_receiver <- function(aom_arr, tie_arr, stat,
                                   n_events = 15L, seed = 1L) {
  set.seed(seed)
  M_loc  <- dim(aom_arr)[1]
  events <- sample(seq_len(M_loc), min(n_events, M_loc))
  for (m in events) {
    s <- actor1_ids[m + 1L]   # sender at aomstats output row m (1-based)
    for (j in seq_len(N)) {
      if (j == s) next
      d <- dyad_col(s, j, N)
      expect_equal(
        aom_arr[m, j, stat],
        tie_arr[m, d, stat],
        tolerance = tol,
        info = paste0("stat=", stat, " m=", m, " s=", s, " j=", j)
      )
    }
  }
}

# Helper: check aomstats sender_stats[,,stat] == tomstats2[,,stat]
#   Sender stats are receiver-independent: for any reference receiver j != i,
#   tie_arr[m, dyad_col(i, j), stat] carries the same value.
#   We use j_ref = (i %% N) + 1  (cycles, never self-loops).
check_aom_tie_sender <- function(aom_sender, tie_arr, stat,
                                 n_events = 15L, seed = 2L) {
  set.seed(seed)
  M_loc  <- dim(aom_sender)[1]
  events <- sample(seq_len(M_loc), min(n_events, M_loc))
  for (m in events) {
    for (i in seq_len(N)) {
      j_ref <- (i %% N) + 1L
      d     <- dyad_col(i, j_ref, N)
      expect_equal(
        aom_sender[m, i, stat],
        tie_arr[m, d, stat],
        tolerance = tol,
        info = paste0("sender stat=", stat, " m=", m, " i=", i)
      )
    }
  }
}

# ===========================================================================
# SECTION 1: Dimensions and slice names
# ===========================================================================

rec_stats_ig <- c("inertia", "indegreeReceiver", "outdegreeReceiver", "reciprocity")

ts_ig <- aomstats(
  reh              = reh_actor,
  receiver_effects = ~ inertia() + indegreeReceiver() +
                       outdegreeReceiver() + reciprocity()
)
ts_sep <- aomstats(
  reh              = reh_actor,
  receiver_effects = ~ inertia(consider_type = "separate") +
                       indegreeReceiver(consider_type = "separate") +
                       outdegreeReceiver(consider_type = "separate") +
                       reciprocity(consider_type = "separate")
)

expect_equal(dim(ts_ig$receiver_stats),  c(M, N, 4L),
             info = "ignore: receiver dims [M x N x 4]")
expect_equal(dim(ts_sep$receiver_stats), c(M, N, 12L),
             info = "separate: receiver dims [M x N x 12]")

expect_equal(
  dimnames(ts_ig$receiver_stats)[[3]],
  rec_stats_ig,
  info = "ignore: correct receiver slice names"
)

expected_sep <- unlist(lapply(rec_stats_ig, function(s) paste0(s, ".", types)))
expect_true(
  all(expected_sep %in% dimnames(ts_sep$receiver_stats)[[3]]),
  info = "separate: all type slices present"
)

# Sender stats
ts_s_ig <- aomstats(
  reh            = reh_actor,
  sender_effects = ~ outdegreeSender() + indegreeSender()
)
ts_s_sep <- aomstats(
  reh            = reh_actor,
  sender_effects = ~ outdegreeSender(consider_type = "separate") +
                     indegreeSender(consider_type = "separate")
)

expect_equal(dim(ts_s_ig$sender_stats),  c(M, N, 2L),
             info = "sender ignore: dims [M x N x 2]")
expect_equal(dim(ts_s_sep$sender_stats), c(M, N, 6L),
             info = "sender separate: dims [M x N x 6]")

expect_equal(
  dimnames(ts_s_ig$sender_stats)[[3]],
  c("outdegreeSender", "indegreeSender"),
  info = "sender ignore: correct slice names"
)

# ===========================================================================
# SECTION 2: Invariants
# ===========================================================================

# 2.1  Separate slices sum to ignore — receiver stats
for (stat in rec_stats_ig) {
  sep_sum <- Reduce("+", lapply(types, function(tp)
    ts_sep$receiver_stats[,, paste0(stat, ".", tp)]
  ))
  expect_equal(sep_sum, ts_ig$receiver_stats[,, stat], tolerance = tol,
               info = paste("receiver separate sums to ignore:", stat))
}

# 2.2  Separate slices sum to ignore — sender stats
for (stat in c("outdegreeSender", "indegreeSender")) {
  sep_sum <- Reduce("+", lapply(types, function(tp)
    ts_s_sep$sender_stats[,, paste0(stat, ".", tp)]
  ))
  expect_equal(sep_sum, ts_s_ig$sender_stats[,, stat], tolerance = tol,
               info = paste("sender separate sums to ignore:", stat))
}

# 2.3  LOCF: between consecutive type-t events, separate stats are constant.
#   Representative check: inertia.competition.
comp_full <- which(el$type == "competition")
comp_rows <- comp_full - 1L
comp_rows <- comp_rows[comp_rows >= 1L & comp_rows <= M]
if (length(comp_rows) >= 2L) {
  from <- comp_rows[1]; to <- comp_rows[2] - 1L
  if (to > from) {
    val_from <- ts_sep$receiver_stats[from, , "inertia.competition"]
    for (k in seq(from + 1L, to)) {
      expect_equal(
        ts_sep$receiver_stats[k, , "inertia.competition"],
        val_from, tolerance = tol,
        info = paste("LOCF inertia.competition: row", k)
      )
    }
  }
}

# 2.4  Zero before first event of each type — check inertia per type
for (tp in types) {
  first_full <- min(which(el$type == tp))
  first_out  <- first_full - 1L
  if (first_out > 1L) {
    zero_vals <- ts_sep$receiver_stats[seq_len(first_out - 1L), ,
                                       paste0("inertia.", tp)]
    expect_equal(sum(abs(zero_vals)), 0, tolerance = tol,
                 info = paste("zero before first event: type =", tp))
  }
}

# ===========================================================================
# SECTION 3: Full memory — receiver ignore vs tomstats2
# ===========================================================================

ts_tie_ig <- tomstats2(
  ~ inertia() + indegreeReceiver() + outdegreeReceiver() + reciprocity(),
  reh = reh_tie, sampling = FALSE
)

for (stat in rec_stats_ig) {
  check_aom_tie_receiver(ts_ig$receiver_stats, ts_tie_ig, stat, n_events = 15L)
}

# ===========================================================================
# SECTION 4: Full memory — receiver separate vs tomstats2 (consider_type=TRUE)
# ===========================================================================

ts_tie_sep <- tomstats2(
  ~ inertia(consider_type = TRUE) +
    indegreeReceiver(consider_type = TRUE) +
    outdegreeReceiver(consider_type = TRUE) +
    reciprocity(consider_type = TRUE),
  reh = reh_tie, sampling = FALSE
)

for (stat in rec_stats_ig) {
  for (tp in types) {
    check_aom_tie_receiver(
      ts_sep$receiver_stats, ts_tie_sep,
      stat  = paste0(stat, ".", tp),
      n_events = 15L, seed = 1L
    )
  }
}

# ===========================================================================
# SECTION 5: Decay memory — receiver ignore vs tomstats2
# ===========================================================================

ts_ig_d <- aomstats(
  reh              = reh_actor,
  receiver_effects = ~ inertia() + indegreeReceiver() +
                       outdegreeReceiver() + reciprocity(),
  memory = "decay", memory_value = 100
)

ts_tie_ig_d <- tomstats2(
  ~ inertia() + indegreeReceiver() + outdegreeReceiver() + reciprocity(),
  reh = reh_tie, sampling = FALSE,
  memory = "decay", memory_value = 100
)

for (stat in rec_stats_ig) {
  check_aom_tie_receiver(ts_ig_d$receiver_stats, ts_tie_ig_d, stat,
                         n_events = 15L, seed = 2L)
}

# ===========================================================================
# SECTION 6: Decay memory — receiver separate vs tomstats2
# ===========================================================================

ts_sep_d <- aomstats(
  reh              = reh_actor,
  receiver_effects = ~ inertia(consider_type = "separate") +
                       indegreeReceiver(consider_type = "separate") +
                       outdegreeReceiver(consider_type = "separate") +
                       reciprocity(consider_type = "separate"),
  memory = "decay", memory_value = 100
)

ts_tie_sep_d <- tomstats2(
  ~ inertia(consider_type = TRUE) +
    indegreeReceiver(consider_type = TRUE) +
    outdegreeReceiver(consider_type = TRUE) +
    reciprocity(consider_type = TRUE),
  reh = reh_tie, sampling = FALSE,
  memory = "decay", memory_value = 100
)

# 6.1  Separate sums to ignore under decay
for (stat in rec_stats_ig) {
  sep_d_sum <- Reduce("+", lapply(types, function(tp)
    ts_sep_d$receiver_stats[,, paste0(stat, ".", tp)]
  ))
  expect_equal(sep_d_sum, ts_ig_d$receiver_stats[,, stat], tolerance = tol,
               info = paste("decay separate sums to ignore:", stat))
}

# 6.2  Value-level match against tie model
for (stat in rec_stats_ig) {
  for (tp in types) {
    check_aom_tie_receiver(
      ts_sep_d$receiver_stats, ts_tie_sep_d,
      stat     = paste0(stat, ".", tp),
      n_events = 15L, seed = 3L
    )
  }
}

# ===========================================================================
# SECTION 7: Sender stats — full and decay memory
# ===========================================================================

# --- 7.1  Full memory -------------------------------------------------------
ts_tie_s_ig <- tomstats2(
  ~ outdegreeSender() + indegreeSender(),
  reh = reh_tie, sampling = FALSE
)

for (stat in c("outdegreeSender", "indegreeSender")) {
  check_aom_tie_sender(ts_s_ig$sender_stats, ts_tie_s_ig, stat, n_events = 15L)
}

# Separate sums to ignore already checked in Section 2.2; spot-check values
for (stat in c("outdegreeSender", "indegreeSender")) {
  ts_tie_s_tp <- tomstats2(
    as.formula(paste0("~ ", stat, "(consider_type = TRUE)")),
    reh = reh_tie, sampling = FALSE
  )
  for (tp in types) {
    sl <- paste0(stat, ".", tp)
    if (sl %in% dimnames(ts_s_sep$sender_stats)[[3]] &&
        sl %in% dimnames(ts_tie_s_tp)[[3]]) {
      check_aom_tie_sender(ts_s_sep$sender_stats, ts_tie_s_tp, sl,
                           n_events = 10L, seed = 4L)
    }
  }
}

# --- 7.2  Decay memory ------------------------------------------------------
ts_s_ig_d <- aomstats(
  reh            = reh_actor,
  sender_effects = ~ outdegreeSender() + indegreeSender(),
  memory = "decay", memory_value = 100
)
ts_tie_s_ig_d <- tomstats2(
  ~ outdegreeSender() + indegreeSender(),
  reh = reh_tie, sampling = FALSE,
  memory = "decay", memory_value = 100
)

for (stat in c("outdegreeSender", "indegreeSender")) {
  check_aom_tie_sender(ts_s_ig_d$sender_stats, ts_tie_s_ig_d, stat,
                       n_events = 15L, seed = 5L)
}
