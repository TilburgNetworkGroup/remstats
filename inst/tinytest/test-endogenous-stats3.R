# Condition 3: Directed events with types, tie-oriented model with active risk set
# Tests correctness of statistics with consider_type = "ignore", "separate", "interact"

library(tinytest)

# Small edgelist
edgelist <- data.frame(
  time = 1:10,
  actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
  actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
)
event_types <- c(1, 1, 2, 2, 1, 2, 2, 1, 1, 1)
edgelist$type <- event_types
reh <- remify::remify(edgelist, model = "tie", riskset = "active", extend_riskset_by_type = TRUE)

# ── "ignore" (default) ───────────────────────────────────────────────────────
# All effects with default consider_type = "ignore" produce one aggregated stat

effects_ig <- ~
  FEtype() +
  outdegreeSender() + outdegreeReceiver() +
  indegreeSender() + indegreeReceiver() +
  totaldegreeSender() + totaldegreeReceiver() +
  totaldegreeDyad() +
  inertia() + reciprocity() +
  isp() + itp() + osp() + otp() +
  isp(unique = TRUE) + itp(unique = TRUE) +
  osp(unique = TRUE) + otp(unique = TRUE) +
  psABBA() + psABBY() + psABAB() + psABAY() +
  psABXA() + psABXB() + psABXY() +
  recencyContinue() +
  recencySendSender() + recencySendReceiver() +
  recencyReceiveSender() + recencyReceiveReceiver() +
  rrankSend() + rrankReceive()

stats <- remstats(reh, tie_effects = effects_ig, start = 1)
riskset <- attr(stats, "riskset")

# No type suffixes in dimnames
expect_false(any(grepl("\\.1$|\\.2$|TypeAgg", dimnames(stats)[[3]])))

# Baseline
expect_equal(stats[, , "baseline"],
  matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# FEtype
FEtype <- cbind(
  matrix(0, nrow = nrow(edgelist), ncol = sum(riskset$type == 1)),
  matrix(1, nrow = nrow(edgelist), ncol = sum(riskset$type == 2)))
expect_equal(stats[, , "FEtype_2"], FEtype)

# outdegreeSender (ignore = aggregate over types)
outdegreeSender.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 1, 1, 1, 0, 0, 1, 1, 1, 0),
  c(2, 1, 1, 1, 0, 0, 2, 1, 1, 0),
  c(2, 2, 2, 2, 0, 0, 2, 2, 2, 0),
  c(2, 2, 2, 2, 1, 0, 2, 2, 2, 0),
  c(2, 2, 2, 2, 1, 1, 2, 2, 2, 1),
  c(2, 3, 3, 3, 1, 1, 2, 3, 3, 1),
  c(2, 4, 4, 4, 1, 1, 2, 4, 4, 1),
  c(2, 5, 5, 5, 1, 1, 2, 5, 5, 1)
)
expect_equal(stats[, , "outdegreeSender"], outdegreeSender.ig)

# outdegreeReceiver (ignore)
outdegreeReceiver.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 1, 0, 1, 0, 0),
  c(0, 1, 0, 0, 1, 1, 0, 1, 0, 0),
  c(0, 2, 0, 0, 1, 2, 0, 2, 0, 0),
  c(0, 2, 0, 0, 2, 2, 0, 2, 0, 0),
  c(1, 2, 1, 0, 2, 2, 1, 2, 1, 1),
  c(1, 2, 1, 1, 2, 2, 1, 2, 1, 1),
  c(1, 2, 1, 1, 3, 2, 1, 2, 1, 1),
  c(1, 2, 1, 1, 4, 2, 1, 2, 1, 1),
  c(1, 2, 1, 1, 5, 2, 1, 2, 1, 1)
)
expect_equal(stats[, , "outdegreeReceiver"], outdegreeReceiver.ig)

# indegreeSender (ignore)
indegreeSender.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 2, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 3, 0, 1, 0, 0, 0),
  c(1, 1, 1, 1, 3, 0, 1, 1, 1, 0),
  c(1, 1, 1, 1, 4, 0, 1, 1, 1, 0),
  c(2, 1, 1, 1, 4, 0, 2, 1, 1, 0),
  c(2, 1, 1, 1, 5, 0, 2, 1, 1, 0),
  c(2, 1, 1, 1, 5, 1, 2, 1, 1, 1)
)
expect_equal(stats[, , "indegreeSender"], indegreeSender.ig)

# indegreeReceiver (ignore)
indegreeReceiver.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 1, 0, 0, 0, 1, 0, 1, 1),
  c(1, 1, 1, 0, 0, 1, 1, 1, 1, 1),
  c(2, 1, 2, 0, 0, 1, 2, 1, 2, 2),
  c(3, 1, 3, 0, 0, 1, 3, 1, 3, 3),
  c(3, 1, 3, 0, 1, 1, 3, 1, 3, 3),
  c(4, 1, 4, 0, 1, 1, 4, 1, 4, 4),
  c(4, 2, 4, 0, 1, 2, 4, 2, 4, 4),
  c(5, 2, 5, 0, 1, 2, 5, 2, 5, 5),
  c(5, 2, 5, 1, 1, 2, 5, 2, 5, 5)
)
expect_equal(stats[, , "indegreeReceiver"], indegreeReceiver.ig)

# totaldegreeSender/Receiver/Dyad (ignore) = in + out
totaldegreeSender.ig <- indegreeSender.ig + outdegreeSender.ig
expect_equal(stats[, , "totaldegreeSender"], totaldegreeSender.ig)
totaldegreeReceiver.ig <- indegreeReceiver.ig + outdegreeReceiver.ig
expect_equal(stats[, , "totaldegreeReceiver"], totaldegreeReceiver.ig)
totaldegreeDyad.ig <- totaldegreeSender.ig + totaldegreeReceiver.ig
expect_equal(stats[, , "totaldegreeDyad"], totaldegreeDyad.ig)

# inertia (ignore)
inertia.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 1, 1, 0, 0),
  c(2, 1, 0, 0, 0, 0, 2, 1, 0, 0),
  c(2, 1, 1, 0, 0, 0, 2, 1, 1, 0),
  c(2, 1, 1, 0, 1, 0, 2, 1, 1, 0),
  c(2, 1, 1, 0, 1, 0, 2, 1, 1, 1),
  c(2, 2, 1, 0, 1, 0, 2, 2, 1, 1),
  c(2, 2, 2, 0, 1, 0, 2, 2, 2, 1),
  c(2, 2, 2, 1, 1, 0, 2, 2, 2, 1)
)
expect_equal(stats[, , "inertia"], inertia.ig)

# reciprocity (ignore)
reciprocity.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 1, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 1, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 1, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 2, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 2, 0, 0, 0, 1, 0)
)
expect_equal(stats[, , "reciprocity"], reciprocity.ig)

# itp (ignore)
itp.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 1, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 2, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 2, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 3, 0, 1, 1, 0, 1)
)
expect_equal(stats[, , "itp"], itp.ig)

itp.unique.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 1, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 1, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 1, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 2, 0, 1, 1, 0, 1)
)
expect_equal(stats[, , "itp.unique"], itp.unique.ig)

# otp (ignore)
otp.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 2, 0, 0, 0, 0, 0, 2, 0),
  c(0, 0, 2, 0, 0, 0, 0, 0, 2, 0),
  c(0, 0, 3, 0, 0, 0, 0, 0, 3, 0)
)
expect_equal(stats[, , "otp"], otp.ig)

otp.unique.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 2, 0, 0, 0, 0, 0, 2, 0)
)
expect_equal(stats[, , "otp.unique"], otp.unique.ig)

# isp (ignore)
isp.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(2, 0, 0, 0, 0, 0, 2, 0, 0, 0),
  c(2, 0, 0, 0, 0, 1, 2, 0, 0, 1)
)
expect_equal(stats[, , "isp"], isp.ig)

isp.unique.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1)
)
expect_equal(stats[, , "isp.unique"], isp.unique.ig)

# osp (ignore)
osp.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 1, 0, 1, 0, 1, 0, 1, 0, 0),
  c(0, 1, 0, 1, 0, 1, 0, 1, 0, 0),
  c(0, 2, 0, 1, 0, 1, 0, 2, 0, 0),
  c(0, 2, 0, 1, 0, 1, 0, 2, 0, 0)
)
expect_equal(stats[, , "osp"], osp.ig)

osp.unique.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 1, 0, 1, 0, 1, 0, 1, 0, 0),
  c(0, 1, 0, 1, 0, 1, 0, 1, 0, 0),
  c(0, 1, 0, 1, 0, 1, 0, 1, 0, 0),
  c(0, 1, 0, 1, 0, 1, 0, 1, 0, 0)
)
expect_equal(stats[, , "osp.unique"], osp.unique.ig)

# psABBA (ignore)
psABBA.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABBA"], psABBA.ig)

# psABBY (ignore)
psABBY.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 1, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 1)
)
expect_equal(stats[, , "psABBY"], psABBY.ig)

# psABAB (ignore)
psABAB.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
  c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABAB"], psABAB.ig)

# psABAY (ignore)
psABAY.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 1, 0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 1, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 1, 1, 0, 0, 0, 0, 1, 0),
  c(0, 1, 0, 1, 0, 0, 0, 1, 0, 0),
  c(0, 1, 1, 0, 0, 0, 0, 1, 1, 0)
)
expect_equal(stats[, , "psABAY"], psABAY.ig)

# psABXA (ignore)
psABXA.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 1, 0, 1, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 1, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 1),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABXA"], psABXA.ig)

# psABXB (ignore)
psABXB.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 1),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 1),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 1),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(1, 0, 1, 0, 0, 0, 1, 0, 1, 0),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 1),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABXB"], psABXB.ig)

# psABXY (ignore)
psABXY.ig <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0)
)
expect_equal(stats[, , "psABXY"], psABXY.ig)

# recencyContinue (ignore)
recencyContinue.ig <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(1/2, 0.0, 0.0, 0.0, 0.0, 0, 1/2, 0.0, 0.0, 0.0),
  c(1/3, 1/2, 0.0, 0.0, 0.0, 0, 1/3, 1/2, 0.0, 0.0),
  c(1/2, 1/3, 0.0, 0.0, 0.0, 0, 1/2, 1/3, 0.0, 0.0),
  c(1/3, 1/4, 1/2, 0.0, 0.0, 0, 1/3, 1/4, 1/2, 0.0),
  c(1/4, 1/5, 1/3, 0.0, 1/2, 0, 1/4, 1/5, 1/3, 0.0),
  c(1/5, 1/6, 1/4, 0.0, 1/3, 0, 1/5, 1/6, 1/4, 1/2),
  c(1/6, 1/2, 1/5, 0.0, 1/4, 0, 1/6, 1/2, 1/5, 1/3),
  c(1/7, 1/3, 1/2, 0.0, 1/5, 0, 1/7, 1/3, 1/2, 1/4),
  c(1/8, 1/4, 1/3, 1/2, 1/6, 0, 1/8, 1/4, 1/3, 1/5)
)
expect_equal(stats[, , "recencyContinue"], recencyContinue.ig)

# recencySendSender (ignore)
recencySendSender.ig <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(1/2, 0.0, 0.0, 0.0, 0.0, 0.0, 1/2, 0.0, 0.0, 0.0),
  c(1/3, 1/2, 1/2, 1/2, 0.0, 0.0, 1/3, 1/2, 1/2, 0.0),
  c(1/2, 1/3, 1/3, 1/3, 0.0, 0.0, 1/2, 1/3, 1/3, 0.0),
  c(1/3, 1/2, 1/2, 1/2, 0.0, 0.0, 1/3, 1/2, 1/2, 0.0),
  c(1/4, 1/3, 1/3, 1/3, 1/2, 0.0, 1/4, 1/3, 1/3, 0.0),
  c(1/5, 1/4, 1/4, 1/4, 1/3, 1/2, 1/5, 1/4, 1/4, 1/2),
  c(1/6, 1/2, 1/2, 1/2, 1/4, 1/3, 1/6, 1/2, 1/2, 1/3),
  c(1/7, 1/2, 1/2, 1/2, 1/5, 1/4, 1/7, 1/2, 1/2, 1/4),
  c(1/8, 1/2, 1/2, 1/2, 1/6, 1/5, 1/8, 1/2, 1/2, 1/5)
)
expect_equal(stats[, , "recencySendSender"], recencySendSender.ig)

# recencySendReceiver (ignore)
recencySendReceiver.ig <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(0.0, 1/2, 0.0, 0.0, 0.0, 1/2, 0.0, 1/2, 0.0, 0.0),
  c(0.0, 1/3, 0.0, 0.0, 1/2, 1/3, 0.0, 1/3, 0.0, 0.0),
  c(0.0, 1/2, 0.0, 0.0, 1/3, 1/2, 0.0, 1/2, 0.0, 0.0),
  c(0.0, 1/3, 0.0, 0.0, 1/2, 1/3, 0.0, 1/3, 0.0, 0.0),
  c(1/2, 1/4, 1/2, 0.0, 1/3, 1/4, 1/2, 1/4, 1/2, 1/2),
  c(1/3, 1/5, 1/3, 1/2, 1/4, 1/5, 1/3, 1/5, 1/3, 1/3),
  c(1/4, 1/6, 1/4, 1/3, 1/2, 1/6, 1/4, 1/6, 1/4, 1/4),
  c(1/5, 1/7, 1/5, 1/4, 1/2, 1/7, 1/5, 1/7, 1/5, 1/5),
  c(1/6, 1/8, 1/6, 1/5, 1/2, 1/8, 1/6, 1/8, 1/6, 1/6)
)
expect_equal(stats[, , "recencySendReceiver"], recencySendReceiver.ig)

# recencyReceiveSender (ignore)
recencyReceiveSender.ig <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(0.0, 0.0, 0.0, 0.0, 1/2, 0.0, 0.0, 0.0, 0.0, 0.0),
  c(1/2, 0.0, 0.0, 0.0, 1/3, 0.0, 1/2, 0.0, 0.0, 0.0),
  c(1/3, 0.0, 0.0, 0.0, 1/2, 0.0, 1/3, 0.0, 0.0, 0.0),
  c(1/4, 0.0, 0.0, 0.0, 1/2, 0.0, 1/4, 0.0, 0.0, 0.0),
  c(1/5, 1/2, 1/2, 1/2, 1/3, 0.0, 1/5, 1/2, 1/2, 0.0),
  c(1/6, 1/3, 1/3, 1/3, 1/2, 0.0, 1/6, 1/3, 1/3, 0.0),
  c(1/2, 1/4, 1/4, 1/4, 1/3, 0.0, 1/2, 1/4, 1/4, 0.0),
  c(1/3, 1/5, 1/5, 1/5, 1/2, 0.0, 1/3, 1/5, 1/5, 0.0),
  c(1/4, 1/6, 1/6, 1/6, 1/3, 1/2, 1/4, 1/6, 1/6, 1/2)
)
expect_equal(stats[, , "recencyReceiveSender"], recencyReceiveSender.ig)

# recencyReceiveReceiver (ignore)
recencyReceiveReceiver.ig <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(1/2, 0.0, 1/2, 0.0, 0.0, 0.0, 1/2, 0.0, 1/2, 1/2),
  c(1/3, 1/2, 1/3, 0.0, 0.0, 1/2, 1/3, 1/2, 1/3, 1/3),
  c(1/2, 1/3, 1/2, 0.0, 0.0, 1/3, 1/2, 1/3, 1/2, 1/2),
  c(1/2, 1/4, 1/2, 0.0, 0.0, 1/4, 1/2, 1/4, 1/2, 1/2),
  c(1/3, 1/5, 1/3, 0.0, 1/2, 1/5, 1/3, 1/5, 1/3, 1/3),
  c(1/2, 1/6, 1/2, 0.0, 1/3, 1/6, 1/2, 1/6, 1/2, 1/2),
  c(1/3, 1/2, 1/3, 0.0, 1/4, 1/2, 1/3, 1/2, 1/3, 1/3),
  c(1/2, 1/3, 1/2, 0.0, 1/5, 1/3, 1/2, 1/3, 1/2, 1/2),
  c(1/3, 1/4, 1/3, 1/2, 1/6, 1/4, 1/3, 1/4, 1/3, 1/3)
)
expect_equal(stats[, , "recencyReceiveReceiver"], recencyReceiveReceiver.ig)

# rrankSend (ignore)
rrankSend.ig <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(1, 0.0, 0.0, 0.0, 0, 0, 1, 0.0, 0.0, 0),
  c(1, 1.0, 0.0, 0.0, 0, 0, 1, 1.0, 0.0, 0),
  c(1, 1.0, 0.0, 0.0, 0, 0, 1, 1.0, 0.0, 0),
  c(1, 1/2, 1.0, 0.0, 0, 0, 1, 1/2, 1.0, 0),
  c(1, 1/2, 1.0, 0.0, 1, 0, 1, 1/2, 1.0, 0),
  c(1, 1/2, 1.0, 0.0, 1, 0, 1, 1/2, 1.0, 1),
  c(1, 1.0, 1/2, 0.0, 1, 0, 1, 1.0, 1/2, 1),
  c(1, 1/2, 1.0, 0.0, 1, 0, 1, 1/2, 1.0, 1),
  c(1, 1/3, 1/2, 1.0, 1, 0, 1, 1/3, 1/2, 1)
)
expect_equal(stats[, , "rrankSend"], rrankSend.ig)

# rrankReceive (ignore)
rrankReceive.ig <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0.0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0.0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0.0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1.0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 1.0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 1/2, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 1/2, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 1.0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 1.0, 0, 0, 0, 1, 0)
)
expect_equal(stats[, , "rrankReceive"], rrankReceive.ig)

# ── "separate" ───────────────────────────────────────────────────────────────
# C type-specific slices; inertia.1 + inertia.2 = inertia (ignore)

stats_sep <- remstats(reh, tie_effects = ~
  inertia(consider_type = "separate") +
  outdegreeSender(consider_type = "separate") +
  reciprocity(consider_type = "separate") +
  itp(consider_type = "separate"), start = 1
)

# Naming: .1 and .2 suffixes, no TypeAgg
expect_true(all(c("inertia.1","inertia.2",
                  "outdegreeSender.1","outdegreeSender.2",
                  "reciprocity.1","reciprocity.2",
                  "itp.1","itp.2") %in% dimnames(stats_sep)[[3]]))

# inertia.1 + inertia.2 = inertia (ignore)
expect_equal(stats_sep[,,"inertia.1"] + stats_sep[,,"inertia.2"],
  inertia.ig, info = "separate inertia sums to ignore")

# outdegreeSender.1 + outdegreeSender.2 = outdegreeSender (ignore)
expect_equal(stats_sep[,,"outdegreeSender.1"] + stats_sep[,,"outdegreeSender.2"],
  outdegreeSender.ig, info = "separate outdegreeSender sums to ignore")

# inertia.1: type-1 events only, same value for both dyad types of same actor pair
# (col 1 = col 7, col 2 = col 8, etc. since "separate" replicates across dyad types)
inertia.1 <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 1, 1, 0, 0),
  c(1, 1, 0, 0, 0, 0, 1, 1, 0, 0),
  c(1, 1, 0, 0, 0, 0, 1, 1, 0, 0),
  c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
  c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
  c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
  c(1, 1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(1, 1, 1, 1, 1, 0, 1, 1, 1, 0)
)
expect_equal(stats_sep[,,"inertia.1"], inertia.1, info = "inertia.1 correct")

# reciprocity.1: type-1 reciprocal events only, replicated across dyad types
reciprocity.1 <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 1, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 1, 0, 0, 0, 1, 0)
)
expect_equal(stats_sep[,,"reciprocity.1"], reciprocity.1, info = "reciprocity.1 correct")

# ext=FALSE should give same separate values for matching actor pairs
reh_false <- remify::remify(edgelist, model = "tie", riskset = "active",
  extend_riskset_by_type = FALSE)
stats_sep_false <- remstats(reh_false, tie_effects = ~
  inertia(consider_type = "separate"), start = 1)
# For each untyped dyad d, stats_sep[,d,"inertia.1"] should equal
# stats_sep_false[,d,"inertia.1"] (same actor pair)
riskset_false <- attr(stats_sep_false, "riskset")
for (d in seq_len(nrow(riskset_false))) {
  a1 <- riskset_false$sender[d]; a2 <- riskset_false$receiver[d]
  # Find matching col in typed riskset (type=1)
  col_typed <- which(riskset$sender == a1 & riskset$receiver == a2 & riskset$type == 1)
  if (length(col_typed) == 1) {
    expect_equal(stats_sep[,,  "inertia.1"][, col_typed],
                 stats_sep_false[,, "inertia.1"][, d],
                 info = paste("separate ext=TRUE/FALSE match for dyad", a1, "->", a2))
  }
}

# ── "interact" ───────────────────────────────────────────────────────────────
# C^2 slices; only meaningful with ext=TRUE

stats_int <- remstats(reh, tie_effects = ~
  inertia(consider_type = "interact"), start = 1)

# Naming: .1.1, .1.2, .2.1, .2.2
expect_true(all(c("inertia.1.1","inertia.1.2",
                  "inertia.2.1","inertia.2.2") %in% dimnames(stats_int)[[3]]))

# Zeroing: .1.1 = 0 for type-2 dyad cols, .1.2 = 0 for type-1 dyad cols
type1_cols <- which(riskset$type == 1)
type2_cols <- which(riskset$type == 2)
expect_true(all(stats_int[, type2_cols, "inertia.1.1"] == 0),
  info = "interact .1.1 zero for type-2 dyads")
expect_true(all(stats_int[, type1_cols, "inertia.1.2"] == 0),
  info = "interact .1.2 zero for type-1 dyads")
expect_true(all(stats_int[, type2_cols, "inertia.2.2"] == 0 |
                stats_int[, type2_cols, "inertia.2.2"] >= 0),  # non-zero allowed
  info = "interact .2.2 type-2 dyads have type-2 inertia")

# .1.1 + .2.1 = inertia (ignore) for type-1 cols
expect_equal(
  stats_int[, type1_cols, "inertia.1.1"] + stats_int[, type1_cols, "inertia.2.1"],
  inertia.ig[, type1_cols],
  info = "interact .1.1 + .2.1 = ignore for type-1 dyads")

# .1.2 + .2.2 = inertia (ignore) for type-2 cols
expect_equal(
  stats_int[, type2_cols, "inertia.1.2"] + stats_int[, type2_cols, "inertia.2.2"],
  inertia.ig[, type2_cols],
  info = "interact .1.2 + .2.2 = ignore for type-2 dyads")

# ── Standardization ──────────────────────────────────────────────────────────
std_effects <- ~
  outdegreeSender(scaling = "std") + outdegreeReceiver(scaling = "std") +
  indegreeSender(scaling = "std") + indegreeReceiver(scaling = "std") +
  totaldegreeSender(scaling = "std") + totaldegreeReceiver(scaling = "std") +
  totaldegreeDyad(scaling = "std") +
  inertia(scaling = "std") + reciprocity(scaling = "std") +
  isp(scaling = "std") + itp(scaling = "std") +
  osp(scaling = "std") + otp(scaling = "std") +
  isp(scaling = "std", unique = TRUE) + itp(scaling = "std", unique = TRUE) +
  osp(scaling = "std", unique = TRUE) + otp(scaling = "std", unique = TRUE)
std_stats <- remstats(reh, tie_effects = std_effects, start = 1)

sapply(2:dim(std_stats)[3], function(p) {
  stat_name <- dimnames(std_stats)[[3]][p]
  scaled_original <- t(apply(stats[, , stat_name], 1, scale))
  scaled_original[which(apply(stats[, , stat_name], 1, sd) == 0), ] <-
    rep(0, ncol(stats))
  expect_equal(std_stats[, , stat_name], scaled_original)
})

# ── Proportional scaling ──────────────────────────────────────────────────────
prop_effects <- ~
  outdegreeSender(scaling = "prop") + outdegreeReceiver(scaling = "prop") +
  indegreeSender(scaling = "prop") + indegreeReceiver(scaling = "prop") +
  totaldegreeSender(scaling = "prop") + totaldegreeReceiver(scaling = "prop") +
  totaldegreeDyad(scaling = "prop") +
  inertia(scaling = "prop") + reciprocity(scaling = "prop")
prop_stats <- remstats(reh, tie_effects = prop_effects, start = 1)

# in/out-degree sender/receiver: scaled by (m-1)
sapply(2:5, function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[, , stat_name] / (1:nrow(stats) - 1)
  scaled_original[1, ] <- 1 / 4
  expect_equal(prop_stats[, , stat_name], scaled_original)
})

# total degree sender/receiver: scaled by 2*(m-1)
sapply(6:7, function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[, , stat_name] / (2 * (1:nrow(stats) - 1))
  scaled_original[1, ] <- 1 / 4
  expect_equal(prop_stats[, , stat_name], scaled_original)
})

# totaldegreeDyad
prop_totaldegreeDyad <- stats[,,"totaldegreeDyad"] / (2*(1:nrow(stats)-1))
prop_totaldegreeDyad[1,] <- 1/4
expect_equal(prop_stats[,,"totaldegreeDyad"], prop_totaldegreeDyad)

# inertia: scaled by outdegreeSender
prop_inertia <- stats[,,"inertia"] / stats[,,"outdegreeSender"]
prop_inertia[stats[,,"outdegreeSender"] == 0] <- 1/3
expect_equal(prop_stats[,,"inertia"], prop_inertia)

# reciprocity: scaled by indegreeSender
prop_reciprocity <- stats[,,"reciprocity"] / stats[,,"indegreeSender"]
prop_reciprocity[stats[,,"indegreeSender"] == 0] <- 1/3
expect_equal(prop_stats[,,"reciprocity"], prop_reciprocity)

# ── Method tests ──────────────────────────────────────────────────────────────
edgelist2 <- data.frame(
  time = c(1, 2, 3, 4, 5, 5, 5, 6, 7, 8),
  actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
  actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1),
  type = c(1, 1, 2, 2, 1, 2, 2, 1, 1, 1)
)
reh2 <- remify::remify(edgelist2, model = "tie", riskset = "active", extend_riskset_by_type = TRUE)
effects2 <- ~ FEtype() + inertia() + itp()
riskset2 <- attr(remstats(reh2, tie_effects = effects2, start = 1), "riskset")

# pt method
pt_stats <- remstats(reh2, tie_effects = effects2, start = 1)
inertia.pt <- rbind(
  matrix(0, ncol = nrow(riskset2)),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 1, 1, 0, 0),
  c(2, 1, 0, 0, 0, 0, 2, 1, 0, 0),
  c(2, 1, 1, 0, 0, 0, 2, 1, 1, 0),
  c(2, 2, 1, 0, 1, 0, 2, 2, 1, 1),
  c(2, 2, 2, 0, 1, 0, 2, 2, 2, 1),
  c(2, 2, 2, 1, 1, 0, 2, 2, 2, 1)
)
expect_equal(pt_stats[, , "inertia"], inertia.pt)

itp.pt <- rbind(
  matrix(0, ncol = nrow(riskset2)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 1, 2, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 2, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 3, 0, 1, 1, 0, 1)
)
expect_equal(pt_stats[, , "itp"], itp.pt)



