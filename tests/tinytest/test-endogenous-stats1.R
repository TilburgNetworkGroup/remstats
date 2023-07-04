# Condition 1: Directed events, tie-oriented model

# Small edgelist
edgelist <- data.frame(
  time = 1:5,
  actor1 = c(1, 1, 2, 2, 3),
  actor2 = c(2, 3, 1, 3, 2)
)
# Statistics
reh <- remify::remify(edgelist, model = "tie")
effects <- ~
  outdegreeSender() + outdegreeReceiver() +
    indegreeSender() + indegreeReceiver() +
    totaldegreeSender() + totaldegreeReceiver() +
    inertia() + reciprocity() +
    isp() + itp() + osp() + otp() +
    psABBA() + psABBY() + psABAB() + psABBY() +
    psABXA() + psABXB() + psABXY() +
    recencyContinue() +
    recencySendSender() + recencySendReceiver() +
    recencyReceiveSender() + recencyReceiveReceiver() +
    rrankSend() + rrankReceive()
stats <- remstats(reh, tie_effects = effects)
riskset <- attr(stats, "riskset")

# Baseline
expect_equal(stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# outdegreeSender
outdegreeSender <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 1, 0, 0, 0, 0),
  c(2, 2, 0, 0, 0, 0),
  c(2, 2, 1, 1, 0, 0),
  c(2, 2, 2, 2, 0, 0)
)
expect_equal(stats[, , "outdegreeSender"], outdegreeSender)

# outdegreeReceiver
outdegreeReceiver <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 1, 0, 1, 0),
  c(0, 0, 2, 0, 2, 0),
  c(1, 0, 2, 0, 2, 1),
  c(2, 0, 2, 0, 2, 2)
)
expect_equal(stats[, , "outdegreeReceiver"], outdegreeReceiver)

# indegreeSender
indegreeSender <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 1, 1, 0, 0),
  c(0, 0, 1, 1, 1, 1),
  c(1, 1, 1, 1, 1, 1),
  c(1, 1, 1, 1, 2, 2)
)
expect_equal(stats[, , "indegreeSender"], indegreeSender)

# indegreeReceiver
indegreeReceiver <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 1),
  c(1, 1, 0, 1, 0, 1),
  c(1, 1, 1, 1, 1, 1),
  c(1, 2, 1, 2, 1, 1)
)
expect_equal(stats[, , "indegreeReceiver"], indegreeReceiver)

# totaldegreeSender
totaldegreeSender <- indegreeSender + outdegreeSender
expect_equal(stats[, , "totaldegreeSender"], totaldegreeSender)

# totaldegreeReceiver
totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
expect_equal(stats[, , "totaldegreeReceiver"], totaldegreeReceiver)

# inertia
inertia <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0),
  c(1, 1, 1, 0, 0, 0),
  c(1, 1, 1, 1, 0, 0)
)
expect_equal(stats[, , "inertia"], inertia)

# reciprocity
reciprocity <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 1, 0, 0, 0),
  c(0, 0, 1, 0, 1, 0),
  c(1, 0, 1, 0, 1, 0),
  c(1, 0, 1, 0, 1, 1)
)
expect_equal(stats[, , "reciprocity"], reciprocity)

# itp
itp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1),
  c(0, 0, 0, 0, 1, 1)
)
expect_equal(stats[, , "itp"], itp)

# otp
otp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0),
  c(0, 1, 0, 1, 0, 0)
)
expect_equal(stats[, , "otp"], otp)

# isp
isp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 1),
  c(0, 0, 0, 1, 0, 1),
  c(0, 1, 0, 1, 1, 1)
)
expect_equal(stats[, , "isp"], isp)

# osp
osp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0),
  c(1, 0, 1, 0, 0, 0)
)
expect_equal(stats[, , "osp"], osp)

# psABBA
psABBA <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0),
  c(1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1)
)
expect_equal(stats[, , "psABBA"], psABBA)

# psABBY
psABBY <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 1),
  c(0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0)
)
expect_equal(stats[, , "psABBY"], psABBY)

# psABAB
psABAB <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0)
)
expect_equal(stats[, , "psABAB"], psABAB)

# psABXB
psABXB <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 1),
  c(0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 1, 0),
  c(0, 1, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABXB"], psABXB)

# psABXY
psABXY <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABXY"], psABXY)

# recencyContinue
recencyContinue <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1 / 2, 0, 0, 0, 0, 0),
  c(1 / 3, 1 / 2, 0, 0, 0, 0),
  c(1 / 4, 1 / 3, 1 / 2, 0, 0, 0),
  c(1 / 5, 1 / 4, 1 / 3, 1 / 2, 0, 0)
)
expect_equal(stats[, , "recencyContinue"], recencyContinue)

# recencySendSender
recencySendSender <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1 / 2, 1 / 2, 0, 0, 0, 0),
  c(1 / 2, 1 / 2, 0, 0, 0, 0),
  c(1 / 3, 1 / 3, 1 / 2, 1 / 2, 0, 0),
  c(1 / 4, 1 / 4, 1 / 2, 1 / 2, 0, 0)
)
expect_equal(stats[, , "recencySendSender"], recencySendSender)

# recencySendReceiver
recencySendReceiver <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 1 / 2, 0, 1 / 2, 0),
  c(0, 0, 1 / 2, 0, 1 / 2, 0),
  c(1 / 2, 0, 1 / 3, 0, 1 / 3, 1 / 2),
  c(1 / 2, 0, 1 / 4, 0, 1 / 4, 1 / 2)
)
expect_equal(stats[, , "recencySendReceiver"], recencySendReceiver)

# recencyReceiveSender
recencyReceiveSender <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 1 / 2, 1 / 2, 0, 0),
  c(0, 0, 1 / 3, 1 / 3, 1 / 2, 1 / 2),
  c(1 / 2, 1 / 2, 1 / 4, 1 / 4, 1 / 3, 1 / 3),
  c(1 / 3, 1 / 3, 1 / 5, 1 / 5, 1 / 2, 1 / 2)
)
expect_equal(stats[, , "recencyReceiveSender"], recencyReceiveSender)

# recencyReceiveReceiver
recencyReceiveReceiver <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1 / 2, 0, 0, 0, 0, 1 / 2),
  c(1 / 3, 1 / 2, 0, 1 / 2, 0, 1 / 3),
  c(1 / 4, 1 / 3, 1 / 2, 1 / 3, 1 / 2, 1 / 4),
  c(1 / 5, 1 / 2, 1 / 3, 1 / 2, 1 / 3, 1 / 5)
)
expect_equal(stats[, , "recencyReceiveReceiver"], recencyReceiveReceiver)

# rrankSend
rrankSend <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0),
  c(1 / 2, 1, 0, 0, 0, 0),
  c(1 / 2, 1, 1, 0, 0, 0),
  c(1 / 2, 1, 1 / 2, 1, 0, 0)
)
expect_equal(stats[, , "rrankSend"], rrankSend)

# rrankReceive
rrankReceive <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 1, 0, 0, 0),
  c(0, 0, 1, 0, 1, 0),
  c(1, 0, 1, 0, 1, 0),
  c(1, 0, 1, 0, 1 / 2, 1)
)
expect_equal(stats[, , "rrankReceive"], rrankReceive)
