# test-aomstats-vs-tie.R
#
# Direct comparison of aomstats() and tomstats() at a single event.
#
# Strategy:
#   - Fix on output row m = 99 (i.e. edgelist event 100, since aomstats
#     starts at event 2 so output row m corresponds to edgelist row m+1).
#   - Inspect the sender at that event, then pick 5 specific receivers.
#   - Look up the corresponding dyad column in tomstats via the explicit
#     formula: d(s, j) = (s-1)*(N-1) + j - as.integer(j > s)   [1-based]
#   - Compare values directly — no loops, no sampling, no automation.

library(tinytest)
library(remify)
library(remstats)

data(randomREH, package = "remify")
el <- randomREH$edgelist[1:100, ]
N  <- length(randomREH$actors)
tol <- 1e-10

reh_actor <- remify(
  edgelist = el, actors = randomREH$actors,
  directed = TRUE, origin = randomREH$origin, model = "actor"
)
reh_tie <- remify(
  edgelist = el, actors = randomREH$actors,
  directed = TRUE, origin = randomREH$origin, model = "tie",
  extend_riskset_by_type = FALSE
)

# aomstats output row 99 = edgelist row 100
m <- 99L
sender_id <- reh_actor$ids$actor1[m + 1L]   # 1-based sender at event 100
cat("event 100: sender_id =", sender_id, "\n")

# Pick 5 receivers that are not the sender
receivers <- setdiff(1:8, sender_id)[1:5]
cat("checking receivers:", receivers, "\n")

dyad_col <- function(s, j) (s - 1L) * (N - 1L) + j - as.integer(j > s)

# ---------------------------------------------------------------------------
# BLOCK A: full memory, inertia + indegreeReceiver + outdegreeReceiver + reciprocity
# ---------------------------------------------------------------------------
effects <- ~ inertia() + indegreeReceiver() + outdegreeReceiver() + reciprocity()
effects_sep <- ~ inertia(consider_type = "separate") +
                 indegreeReceiver(consider_type = "separate") +
                 outdegreeReceiver(consider_type = "separate") +
                 reciprocity(consider_type = "separate")

ts_aom  <- aomstats(reh = reh_actor, receiver_effects = effects)
ts_tie  <- tomstats(effects, reh = reh_tie, sampling = FALSE)
ts_sep  <- aomstats(reh = reh_actor, receiver_effects = effects_sep)
ts_tie_sep <- tomstats(
  ~ inertia(consider_type = TRUE) + indegreeReceiver(consider_type = TRUE) +
    outdegreeReceiver(consider_type = TRUE) + reciprocity(consider_type = TRUE),
  reh = reh_tie, sampling = FALSE
)

types <- sort(unique(el$type))

for (j in receivers) {
  d <- dyad_col(sender_id, j)
  cat(sprintf("  j=%d  d=%d\n", j, d))

  # ignore vs tie
  expect_equal(ts_aom$receiver_stats[m, j, "inertia"],
               ts_tie[m, d, "inertia"], tolerance = tol,
               info = sprintf("full/ignore inertia          j=%d", j))
  expect_equal(ts_aom$receiver_stats[m, j, "indegreeReceiver"],
               ts_tie[m, d, "indegreeReceiver"], tolerance = tol,
               info = sprintf("full/ignore indegreeReceiver  j=%d", j))
  expect_equal(ts_aom$receiver_stats[m, j, "outdegreeReceiver"],
               ts_tie[m, d, "outdegreeReceiver"], tolerance = tol,
               info = sprintf("full/ignore outdegreeReceiver j=%d", j))
  expect_equal(ts_aom$receiver_stats[m, j, "reciprocity"],
               ts_tie[m, d, "reciprocity"], tolerance = tol,
               info = sprintf("full/ignore reciprocity       j=%d", j))

  # separate vs tie (per type)
  for (tp in types) {
    sl <- paste0("inertia.", tp)
    expect_equal(ts_sep$receiver_stats[m, j, sl],
                 ts_tie_sep[m, d, sl], tolerance = tol,
                 info = sprintf("full/separate %s j=%d", sl, j))
  }
}

# ---------------------------------------------------------------------------
# BLOCK B: decay memory, same stats
# ---------------------------------------------------------------------------
effects_d     <- ~ inertia() + indegreeReceiver() + outdegreeReceiver() + reciprocity()
effects_sep_d <- ~ inertia(consider_type = "separate") +
                   indegreeReceiver(consider_type = "separate") +
                   outdegreeReceiver(consider_type = "separate") +
                   reciprocity(consider_type = "separate")
effects_tie_sep_d <- ~ inertia(consider_type = TRUE) +
                       indegreeReceiver(consider_type = TRUE) +
                       outdegreeReceiver(consider_type = TRUE) +
                       reciprocity(consider_type = TRUE)

ts_aom_d     <- aomstats(reh = reh_actor, receiver_effects = effects_d,
                          memory = "decay", memory_value = 100)
ts_tie_d     <- tomstats(effects_d, reh = reh_tie, sampling = FALSE,
                           memory = "decay", memory_value = 100)
ts_sep_d     <- aomstats(reh = reh_actor, receiver_effects = effects_sep_d,
                          memory = "decay", memory_value = 100)
ts_tie_sep_d <- tomstats(effects_tie_sep_d, reh = reh_tie, sampling = FALSE,
                           memory = "decay", memory_value = 100)

for (j in receivers) {
  d <- dyad_col(sender_id, j)

  expect_equal(ts_aom_d$receiver_stats[m, j, "inertia"],
               ts_tie_d[m, d, "inertia"], tolerance = tol,
               info = sprintf("decay/ignore inertia          j=%d", j))
  expect_equal(ts_aom_d$receiver_stats[m, j, "indegreeReceiver"],
               ts_tie_d[m, d, "indegreeReceiver"], tolerance = tol,
               info = sprintf("decay/ignore indegreeReceiver  j=%d", j))
  expect_equal(ts_aom_d$receiver_stats[m, j, "outdegreeReceiver"],
               ts_tie_d[m, d, "outdegreeReceiver"], tolerance = tol,
               info = sprintf("decay/ignore outdegreeReceiver j=%d", j))
  expect_equal(ts_aom_d$receiver_stats[m, j, "reciprocity"],
               ts_tie_d[m, d, "reciprocity"], tolerance = tol,
               info = sprintf("decay/ignore reciprocity       j=%d", j))

  for (tp in types) {
    sl <- paste0("inertia.", tp)
    expect_equal(ts_sep_d$receiver_stats[m, j, sl],
                 ts_tie_sep_d[m, d, sl], tolerance = tol,
                 info = sprintf("decay/separate %s j=%d", sl, j))
  }
}

# ---------------------------------------------------------------------------
# BLOCK C: sender stats at event 100 — outdegreeSender + indegreeSender
#   Sender stat is actor-level so it should be equal for all reference
#   receivers. We check actor i=sender_id using two reference receivers.
# ---------------------------------------------------------------------------
ts_s  <- aomstats(reh = reh_actor,
                  sender_effects = ~ outdegreeSender() + indegreeSender())
ts_s_tie <- tomstats(~ outdegreeSender() + indegreeSender(),
                       reh = reh_tie, sampling = FALSE)

i <- sender_id
j_ref1 <- receivers[1]; j_ref2 <- receivers[2]
d1 <- dyad_col(i, j_ref1); d2 <- dyad_col(i, j_ref2)

expect_equal(ts_s$sender_stats[m, i, "outdegreeSender"],
             ts_s_tie[m, d1, "outdegreeSender"], tolerance = tol,
             info = "sender outdegreeSender vs tie (ref j1)")
expect_equal(ts_s$sender_stats[m, i, "outdegreeSender"],
             ts_s_tie[m, d2, "outdegreeSender"], tolerance = tol,
             info = "sender outdegreeSender vs tie (ref j2)")
expect_equal(ts_s$sender_stats[m, i, "indegreeSender"],
             ts_s_tie[m, d1, "indegreeSender"], tolerance = tol,
             info = "sender indegreeSender vs tie (ref j1)")
