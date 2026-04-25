# Spot-check: aomstats consider_type="separate" vs tomstats consider_type=TRUE
#
# Full event history (9915 events) so stats are rich, but start=9900 so only
# ~15 output rows are computed. All receivers checked at all output rows.
#
# Effects covered (all directed, consider_type="separate" vs TRUE):
#   - inertia, reciprocity
#   - degrees: indegreeSender, outdegreeSender, indegreeReceiver, outdegreeReceiver
#   - triads:  otp, itp, osp, isp
#   - pshifts: psABBA, psABBY, psABXA, psABXB, psABXY, psABAY, psABAB
#   - rrank:   rrankSend, rrankReceive
#   - recency: recencySendSender, recencySendReceiver,
#              recencyReceiveSender, recencyReceiveReceiver
#
# Memory types: full, decay, window, interval

library(remify)
library(remstats)
library(tinytest)

data(randomREH, package = "remify")
el <- randomREH$edgelist

reh_tie <- remify(el, actors = randomREH$actors, directed = TRUE,
                   origin = randomREH$origin, model = "tie",
                   extend_riskset_by_type = FALSE)
reh_actor <- remify(el, actors = randomREH$actors, directed = TRUE,
                     origin = randomREH$origin, model = "actor")

el_reh <- reh_tie$edgelist
N      <- length(randomREH$actors)
dict   <- reh_tie$meta$dictionary$actors[, 1]
dyad_col <- function(s, j) (s - 1L) * (N - 1L) + j - as.integer(j > s)

START <- 9900L
tol   <- 1e-7

run_check <- function(tie_formula, aom_formula, memory, memory_value, label) {
  cat("\n---", label, "---\n")

  ts_tie <- tomstats(
    tie_formula, reh = reh_tie, sampling = FALSE,
    memory = memory, memory_value = memory_value,
    start = START
  )

  ts_aom <- aomstats(
    reh = reh_actor, receiver_effects = aom_formula,
    memory = memory, memory_value = memory_value,
    start = START
  )

  slices <- dimnames(ts_tie)[[3]]
  slices <- slices[slices != "baseline"]
  M_out  <- dim(ts_aom$receiver_stats)[1]

  for (m in seq_len(M_out)) {
    event_row   <- START - 1L + m
    sender_name <- as.character(el_reh$actor1[event_row])
    sender_id   <- which(dict == sender_name)
    for (j in seq_len(N)) {
      if (j == sender_id) next
      d <- dyad_col(sender_id, j)
      for (sl in slices) {
        expect_equal(
          ts_aom$receiver_stats[m, j, sl],
          ts_tie[m, d, sl],
          tolerance = tol,
          info = sprintf("%s m=%d j=%d slice=%s", label, m, j, sl)
        )
      }
    }
  }
  cat("  done:", length(slices), "slices x", M_out, "events x", N - 1L,
      "receivers =", length(slices) * M_out * (N - 1L), "checks\n")
}

# ---------------------------------------------------------------------------
# Formula groups
# ---------------------------------------------------------------------------

f_base_tie <- ~ inertia(consider_type = TRUE) +
                 reciprocity(consider_type = TRUE)
f_base_aom <- ~ inertia(consider_type = "separate") +
                 reciprocity(consider_type = "separate")

f_deg_tie  <- ~ indegreeReceiver(consider_type = TRUE) +
                 outdegreeReceiver(consider_type = TRUE) +
                 totaldegreeReceiver(consider_type = TRUE)
f_deg_aom  <- ~ indegreeReceiver(consider_type = "separate") +
                 outdegreeReceiver(consider_type = "separate") +
                 totaldegreeReceiver(consider_type = "separate")

f_triad_tie <- ~ otp(consider_type = TRUE) + itp(consider_type = TRUE) +
                  osp(consider_type = TRUE) + isp(consider_type = TRUE)
f_triad_aom <- ~ otp(consider_type = "separate") + itp(consider_type = "separate") +
                  osp(consider_type = "separate") + isp(consider_type = "separate")

f_ps_tie <- ~ psABBA(consider_type = TRUE) + psABBY(consider_type = TRUE) +
               psABXA(consider_type = TRUE) + psABXB(consider_type = TRUE) +
               psABXY(consider_type = TRUE) + psABAY(consider_type = TRUE) +
               psABAB(consider_type = TRUE)
f_ps_aom <- ~ psABBA(consider_type = "separate") + psABBY(consider_type = "separate") +
               psABXA(consider_type = "separate") + psABXB(consider_type = "separate") +
               psABXY(consider_type = "separate") + psABAY(consider_type = "separate") +
               psABAB(consider_type = "separate")

f_rr_tie <- ~ rrankSend(consider_type = TRUE) + rrankReceive(consider_type = TRUE)
f_rr_aom <- ~ rrankSend(consider_type = "separate") + rrankReceive(consider_type = "separate")

f_rec_tie <- ~ recencySendReceiver(consider_type = TRUE) +
                recencyReceiveReceiver(consider_type = TRUE) +
                recencyContinue(consider_type = TRUE)
f_rec_aom <- ~ recencySendReceiver(consider_type = "separate") +
                recencyReceiveReceiver(consider_type = "separate") +
                recencyContinue(consider_type = "separate")

# ---------------------------------------------------------------------------
# Full memory
# ---------------------------------------------------------------------------
run_check(f_base_tie,  f_base_aom,  "full", NA, "full / base")
run_check(f_deg_tie,   f_deg_aom,   "full", NA, "full / degrees")
run_check(f_triad_tie, f_triad_aom, "full", NA, "full / triads")
run_check(f_ps_tie,    f_ps_aom,    "full", NA, "full / pshifts")
run_check(f_rr_tie,    f_rr_aom,    "full", NA, "full / rrank")
run_check(f_rec_tie,   f_rec_aom,   "full", NA, "full / recency")

# ---------------------------------------------------------------------------
# Decay memory — primary regression test for the bug fix                     
# ---------------------------------------------------------------------------
run_check(f_base_tie,  f_base_aom,  "decay", 100, "decay / base")
run_check(f_deg_tie,   f_deg_aom,   "decay", 100, "decay / degrees")
run_check(f_triad_tie, f_triad_aom, "decay", 100, "decay / triads")
run_check(f_ps_tie,    f_ps_aom,    "decay", 100, "decay / pshifts")
run_check(f_rr_tie,    f_rr_aom,    "decay", 100, "decay / rrank")
run_check(f_rec_tie,   f_rec_aom,   "decay", 100, "decay / recency")

# ---------------------------------------------------------------------------
# Window and interval                                                        
# ---------------------------------------------------------------------------
run_check(f_base_tie, f_base_aom, "window",   1000,         "window / base")
run_check(f_deg_tie,  f_deg_aom,  "window",   1000,         "window / degrees")
run_check(f_base_tie, f_base_aom, "interval", c(500, 2000), "interval / base")
run_check(f_deg_tie,  f_deg_aom,  "interval", c(500, 2000), "interval / degrees")
