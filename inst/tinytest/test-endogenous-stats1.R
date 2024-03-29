# Condition 1: Directed events, tie-oriented model with active risk set

# Small edgelist
edgelist <- data.frame(
  time = 1:10,
  actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
  actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
)

# Statistics
reh <- remify::remify(edgelist, model = "tie", riskset = "active")
effects <- ~
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
stats <- remstats(reh, tie_effects = effects, method = "pe")
riskset <- attr(stats, "riskset")

# Baseline
expect_equal(stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# outdegreeSender
outdegreeSender <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 1, 1, 1, 0, 0, 0),
  c(2, 1, 1, 1, 0, 0, 0),
  c(2, 2, 2, 2, 0, 0, 0),
  c(2, 2, 2, 2, 1, 0, 0),
  c(2, 2, 2, 2, 1, 1, 1),
  c(2, 3, 3, 3, 1, 1, 1),
  c(2, 4, 4, 4, 1, 1, 1),
  c(2, 5, 5, 5, 1, 1, 1)
)
expect_equal(stats[, , "outdegreeSender"], outdegreeSender)

# outdegreeReceiver
outdegreeReceiver <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 1, 0),
  c(0, 1, 0, 0, 1, 1, 0),
  c(0, 2, 0, 0, 1, 2, 0),
  c(0, 2, 0, 0, 2, 2, 0),
  c(1, 2, 1, 0, 2, 2, 1),
  c(1, 2, 1, 1, 2, 2, 1),
  c(1, 2, 1, 1, 3, 2, 1),
  c(1, 2, 1, 1, 4, 2, 1),
  c(1, 2, 1, 1, 5, 2, 1)
)
expect_equal(stats[, , "outdegreeReceiver"], outdegreeReceiver)

# indegreeSender
indegreeSender <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 1, 0, 0),
  c(1, 0, 0, 0, 1, 0, 0),
  c(1, 0, 0, 0, 2, 0, 0),
  c(1, 0, 0, 0, 3, 0, 0),
  c(1, 1, 1, 1, 3, 0, 0),
  c(1, 1, 1, 1, 4, 0, 0),
  c(2, 1, 1, 1, 4, 0, 0),
  c(2, 1, 1, 1, 5, 0, 0),
  c(2, 1, 1, 1, 5, 1, 1)
)
expect_equal(stats[, , "indegreeSender"], indegreeSender)

# indegreeReceiver
indegreeReceiver <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 1, 0, 0, 0, 1),
  c(1, 1, 1, 0, 0, 1, 1),
  c(2, 1, 2, 0, 0, 1, 2),
  c(3, 1, 3, 0, 0, 1, 3),
  c(3, 1, 3, 0, 1, 1, 3),
  c(4, 1, 4, 0, 1, 1, 4),
  c(4, 2, 4, 0, 1, 2, 4),
  c(5, 2, 5, 0, 1, 2, 5),
  c(5, 2, 5, 1, 1, 2, 5)
)
expect_equal(stats[, , "indegreeReceiver"], indegreeReceiver)

# totaldegreeSender
totaldegreeSender <- indegreeSender + outdegreeSender
expect_equal(stats[, , "totaldegreeSender"], totaldegreeSender)

# totaldegreeReceiver
totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
expect_equal(stats[, , "totaldegreeReceiver"], totaldegreeReceiver)

# totaldegreeDyad
totaldegreeDyad <- totaldegreeSender + totaldegreeReceiver
expect_equal(stats[, , "totaldegreeDyad"], totaldegreeDyad)

# inertia
inertia <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 0),
  c(2, 1, 0, 0, 0, 0, 0),
  c(2, 1, 1, 0, 0, 0, 0),
  c(2, 1, 1, 0, 1, 0, 0),
  c(2, 1, 1, 0, 1, 0, 1),
  c(2, 2, 1, 0, 1, 0, 1),
  c(2, 2, 2, 0, 1, 0, 1),
  c(2, 2, 2, 1, 1, 0, 1)
)
expect_equal(stats[, , "inertia"], inertia)

# reciprocity
reciprocity <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 1, 0, 0),
  c(0, 0, 1, 0, 1, 0, 0),
  c(0, 0, 1, 0, 1, 0, 0),
  c(0, 0, 1, 0, 2, 0, 0),
  c(0, 0, 1, 0, 2, 0, 0)
)
expect_equal(stats[, , "reciprocity"], reciprocity)

# itp
itp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(1, 1, 0, 0, 1, 0, 0),
  c(1, 1, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 2, 0, 0),
  c(1, 1, 0, 1, 2, 0, 0),
  c(1, 1, 0, 1, 3, 0, 1)
)
expect_equal(stats[, , "itp"], itp)

# itp.unique
itp.unique <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0),
	c(1, 1, 0, 0, 1, 0, 0),
	c(1, 1, 0, 1, 1, 0, 0),
	c(1, 1, 0, 1, 1, 0, 0),
	c(1, 1, 0, 1, 1, 0, 0),
	c(1, 1, 0, 1, 2, 0, 1)
)
expect_equal(stats[, , "itp.unique"], itp.unique)

# otp
otp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 2, 0, 0, 0, 0),
  c(0, 0, 2, 0, 0, 0, 0),
  c(0, 0, 3, 0, 0, 0, 0)
)
expect_equal(stats[, , "otp"], otp)

# otp.unique
otp.unique <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 1, 0, 0, 0, 0),
	c(0, 0, 1, 0, 0, 0, 0),
	c(0, 0, 1, 0, 0, 0, 0),
	c(0, 0, 1, 0, 0, 0, 0),
	c(0, 0, 1, 0, 0, 0, 0),
	c(0, 0, 1, 0, 0, 0, 0),
	c(0, 0, 1, 0, 0, 0, 0),
	c(0, 0, 2, 0, 0, 0, 0)
)
expect_equal(stats[, , "otp.unique"], otp.unique)

# isp
isp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(2, 0, 0, 0, 0, 0, 0),
  c(2, 0, 0, 0, 0, 1, 1)
)
expect_equal(stats[, , "isp"], isp)

# isp.unique
isp.unique <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 1, 1)
)
expect_equal(stats[, , "isp.unique"], isp.unique)

# osp
osp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0),
  c(0, 1, 0, 1, 0, 1, 0),
  c(0, 1, 0, 1, 0, 1, 0),
  c(0, 2, 0, 1, 0, 1, 0),
  c(0, 2, 0, 1, 0, 1, 0)
)
expect_equal(stats[, , "osp"], osp)

# osp.unique
osp.unique <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0),
  c(0, 1, 0, 1, 0, 1, 0),
  c(0, 1, 0, 1, 0, 1, 0),
  c(0, 1, 0, 1, 0, 1, 0),
  c(0, 1, 0, 1, 0, 1, 0)
)
expect_equal(stats[, , "osp.unique"], osp.unique)

# psABBA
psABBA <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABBA"], psABBA)

# psABBY
psABBY <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 1, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 1, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1, 1)
)
expect_equal(stats[, , "psABBY"], psABBY)

# psABAB
psABAB <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 1),
  c(0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0, 0)
)
expect_equal(stats[, , "psABAB"], psABAB)

# psABXB
psABXB <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 1, 0, 0, 0, 1),
  c(0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 1),
  c(1, 0, 0, 0, 0, 0, 1),
  c(0, 0, 0, 0, 0, 0, 0),
  c(1, 0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1, 0),
  c(1, 0, 0, 0, 0, 0, 1),
  c(0, 0, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABXB"], psABXB)

# psABXY
psABXY <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 1),
  c(0, 0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 0, 1, 0),
  c(0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 1),
  c(0, 0, 0, 0, 0, 1, 0),
  c(1, 0, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABXY"], psABXY)

# psABXA
psABXA <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(0, 1, 0, 0, 0, 1, 0),
	c(0, 0, 0, 0, 1, 0, 0),
	c(0, 1, 0, 0, 0, 1, 0),
	c(0, 0, 0, 0, 0, 0, 0),
	c(1, 0, 0, 0, 0, 0, 1),
	c(0, 0, 0, 1, 0, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0)
)
expect_equal(stats[, , "psABXA"], psABXA)

# psABAY
psABAY <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 1, 1, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 1, 0, 1, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 1, 0),
	c(0, 0, 1, 1, 0, 0, 0),
	c(0, 1, 0, 1, 0, 0, 0),
	c(0, 1, 1, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABAY"], psABAY)

# recencyContinue
recencyContinue <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1/2, 0.0, 0.0, 0.0, 0.0, 0, 0.0),
  c(1/3, 1/2, 0.0, 0.0, 0.0, 0, 0.0),
  c(1/2, 1/3, 0.0, 0.0, 0.0, 0, 0.0),
  c(1/3, 1/4, 1/2, 0.0, 0.0, 0, 0.0),
  c(1/4, 1/5, 1/3, 0.0, 1/2, 0, 0.0),
  c(1/5, 1/6, 1/4, 0.0, 1/3, 0, 1/2),
  c(1/6, 1/2, 1/5, 0.0, 1/4, 0, 1/3),
  c(1/7, 1/3, 1/2, 0.0, 1/5, 0, 1/4),
  c(1/8, 1/4, 1/3, 1/2, 1/6, 0, 1/5)
)
expect_equal(stats[, , "recencyContinue"], recencyContinue)

# recencySendSender
recencySendSender <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1/2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
  c(1/3, 1/2, 1/2, 1/2, 0.0, 0.0, 0.0),
  c(1/2, 1/3, 1/3, 1/3, 0.0, 0.0, 0.0),
  c(1/3, 1/2, 1/2, 1/2, 0.0, 0.0, 0.0),
  c(1/4, 1/3, 1/3, 1/3, 1/2, 0.0, 0.0),
  c(1/5, 1/4, 1/4, 1/4, 1/3, 1/2, 1/2),
  c(1/6, 1/2, 1/2, 1/2, 1/4, 1/3, 1/3),
  c(1/7, 1/2, 1/2, 1/2, 1/5, 1/4, 1/4),
  c(1/8, 1/2, 1/2, 1/2, 1/6, 1/5, 1/5)
)
expect_equal(stats[, , "recencySendSender"], recencySendSender)

# recencySendReceiver
recencySendReceiver <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0.0, 1/2, 0.0, 0.0, 0.0, 1/2, 0.0),
  c(0.0, 1/3, 0.0, 0.0, 1/2, 1/3, 0.0),
  c(0.0, 1/2, 0.0, 0.0, 1/3, 1/2, 0.0),
  c(0.0, 1/3, 0.0, 0.0, 1/2, 1/3, 0.0),
  c(1/2, 1/4, 1/2, 0.0, 1/3, 1/4, 1/2),
  c(1/3, 1/5, 1/3, 1/2, 1/4, 1/5, 1/3),
  c(1/4, 1/6, 1/4, 1/3, 1/2, 1/6, 1/4),
  c(1/5, 1/7, 1/5, 1/4, 1/2, 1/7, 1/5),
  c(1/6, 1/8, 1/6, 1/5, 1/2, 1/8, 1/6)
)
expect_equal(stats[, , "recencySendReceiver"], recencySendReceiver)

# recencyReceiveSender
recencyReceiveSender <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0.0, 0.0, 0.0, 0.0, 1/2, 0.0, 0.0),
  c(1/2, 0.0, 0.0, 0.0, 1/3, 0.0, 0.0),
  c(1/3, 0.0, 0.0, 0.0, 1/2, 0.0, 0.0),
  c(1/4, 0.0, 0.0, 0.0, 1/2, 0.0, 0.0),
  c(1/5, 1/2, 1/2, 1/2, 1/3, 0.0, 0.0),
  c(1/6, 1/3, 1/3, 1/3, 1/2, 0.0, 0.0),
  c(1/2, 1/4, 1/4, 1/4, 1/3, 0.0, 0.0),
  c(1/3, 1/5, 1/5, 1/5, 1/2, 0.0, 0.0),
  c(1/4, 1/6, 1/6, 1/6, 1/3, 1/2, 1/2)
)
expect_equal(stats[, , "recencyReceiveSender"], recencyReceiveSender)

# recencyReceiveReceiver
recencyReceiveReceiver <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1/2, 0.0, 1/2, 0.0, 0.0, 0.0, 1/2),
  c(1/3, 1/2, 1/3, 0.0, 0.0, 1/2, 1/3),
  c(1/2, 1/3, 1/2, 0.0, 0.0, 1/3, 1/2),
  c(1/2, 1/4, 1/2, 0.0, 0.0, 1/4, 1/2),
  c(1/3, 1/5, 1/3, 0.0, 1/2, 1/5, 1/3),
  c(1/2, 1/6, 1/2, 0.0, 1/3, 1/6, 1/2),
  c(1/3, 1/2, 1/3, 0.0, 1/4, 1/2, 1/3),
  c(1/2, 1/3, 1/2, 0.0, 1/5, 1/3, 1/2),
  c(1/3, 1/4, 1/3, 1/2, 1/6, 1/4, 1/3)
)
expect_equal(stats[, , "recencyReceiveReceiver"], recencyReceiveReceiver)

# rrankSend
rrankSend <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0.0, 0.0, 0.0, 0, 0, 0),
  c(1, 1.0, 0.0, 0.0, 0, 0, 0),
  c(1, 1.0, 0.0, 0.0, 0, 0, 0),
  c(1, 1/2, 1.0, 0.0, 0, 0, 0),
  c(1, 1/2, 1.0, 0.0, 1, 0, 0),
  c(1, 1/2, 1.0, 0.0, 1, 0, 1),
  c(1, 1.0, 1/2, 0.0, 1, 0, 1),
  c(1, 1/2, 1.0, 0.0, 1, 0, 1),
  c(1, 1/3, 1/2, 1.0, 1, 0, 1)
)
expect_equal(stats[, , "rrankSend"], rrankSend)

# rrankReceive
rrankReceive <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0.0, 0, 0),
  c(0, 0, 0, 0, 0.0, 0, 0),
  c(0, 0, 0, 0, 0.0, 0, 0),
  c(0, 0, 0, 0, 1.0, 0, 0),
  c(0, 0, 1, 0, 1.0, 0, 0),
  c(0, 0, 1, 0, 1/2, 0, 0),
  c(0, 0, 1, 0, 1/2, 0, 0),
  c(0, 0, 1, 0, 1.0, 0, 0),
  c(0, 0, 1, 0, 1.0, 0, 0)
)
expect_equal(stats[, , "rrankReceive"], rrankReceive)

# test standardization
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
std_stats <- remstats(reh, tie_effects = std_effects)

sapply(2:dim(std_stats)[3], function(p) {
  stat_name <- dimnames(std_stats)[[3]][p]
  scaled_original <- t(apply(stats[,,stat_name], 1, scale))
  scaled_original[which(apply(stats[,,stat_name], 1, sd) == 0),] <- 
    rep(0, ncol(stats))
  expect_equal(std_stats[,,stat_name], scaled_original)
})

# test proportional scaling
prop_effects <- ~ 
  outdegreeSender(scaling = "prop") + 
    outdegreeReceiver(scaling = "prop") +
    indegreeSender(scaling = "prop") + 
    indegreeReceiver(scaling = "prop") +
    totaldegreeSender(scaling = "prop") + 
    totaldegreeReceiver(scaling = "prop") + 
    totaldegreeDyad(scaling = "prop") +
    inertia(scaling = "prop") + 
    reciprocity(scaling = "prop") 
prop_stats <- remstats(reh, tie_effects = prop_effects)

sapply(2:5, function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[,,stat_name] / (1:nrow(stats)-1)
  scaled_original[1,] <- 1/4
  expect_equal(prop_stats[,,stat_name], scaled_original)
}) # in- and out-degree of the sender and receiver

sapply(6:7, function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[,,stat_name] / (2*(1:nrow(stats)-1))
  scaled_original[1,] <- 1/4
  expect_equal(prop_stats[,,stat_name], scaled_original)
}) # total degree of the sender and receiver

# totaldegreeDyad
prop_totaldegreeDyad <- stats[,,"totaldegreeDyad"] / (2*(1:nrow(stats)-1))
prop_totaldegreeDyad[1,] <- prop_totaldegreeDyad[1,] <- 1/4
expect_equal(prop_stats[,,"totaldegreeDyad"], prop_totaldegreeDyad)

# inertia
prop_inertia <- stats[,,"inertia"] / stats[,,"outdegreeSender"]
prop_inertia[stats[,,"outdegreeSender"]==0] <- 1 / 3
expect_equal(prop_stats[,,"inertia"], prop_inertia)

# reciprocity
prop_reciprocity <- stats[,,"reciprocity"] / stats[,,"indegreeSender"]
prop_reciprocity[stats[,,"indegreeSender"]==0] <- 1 / 3
expect_equal(prop_stats[,,"reciprocity"], prop_reciprocity)


# Test method -------------------------------------------------------------
# Small change to the times in the edgelist
edgelist <- data.frame(
	time = c(1, 2, 3, 4, 5, 5, 5, 6, 7, 8),
	actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
	actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
)

reh <- remify::remify(edgelist, model = "tie", riskset = "active")

# Selection of effects that have unique underlying cpp functions
effects <- ~ outdegreeSender() + inertia() + reciprocity() +
	itp() + psABBA() + psABAY() + recencyContinue() + rrankSend() 

# Method = "pt"
pt_stats <- remstats(reh, tie_effects = effects, method = "pt")
riskset <- attr(pt_stats, "riskset")

# Baseline
expect_equal(pt_stats[, , "baseline"], matrix(1, nrow = NROW(unique(edgelist$time)), ncol = nrow(riskset)))

# outdegreeSender
outdegreeSender <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(1, 0, 0, 0, 0, 0, 0),
	c(1, 1, 1, 1, 0, 0, 0),
	c(2, 1, 1, 1, 0, 0, 0),
	c(2, 2, 2, 2, 0, 0, 0),
	c(2, 3, 3, 3, 1, 1, 1),
	c(2, 4, 4, 4, 1, 1, 1),
	c(2, 5, 5, 5, 1, 1, 1)
)
expect_equal(pt_stats[, , "outdegreeSender"], outdegreeSender)

# inertia
inertia <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(1, 0, 0, 0, 0, 0, 0),
	c(1, 1, 0, 0, 0, 0, 0),
	c(2, 1, 0, 0, 0, 0, 0),
	c(2, 1, 1, 0, 0, 0, 0),
		c(2, 2, 1, 0, 1, 0, 1),
	c(2, 2, 2, 0, 1, 0, 1),
	c(2, 2, 2, 1, 1, 0, 1)
)
expect_equal(pt_stats[, , "inertia"], inertia)

# reciprocity
reciprocity <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0),
		c(0, 0, 1, 0, 1, 0, 0),
	c(0, 0, 1, 0, 2, 0, 0),
	c(0, 0, 1, 0, 2, 0, 0)
)
expect_equal(pt_stats[, , "reciprocity"], reciprocity)

# itp
itp <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0),
	c(1, 1, 0, 1, 2, 0, 0),
	c(1, 1, 0, 1, 2, 0, 0),
	c(1, 1, 0, 1, 3, 0, 1)
)
expect_equal(pt_stats[, , "itp"], itp)


# psABBA
psABBA <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0),
	c(0, 0, 1, 0, 0, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0)
)
expect_equal(pt_stats[, , "psABBA"], psABBA)

# psABAY
psABAY <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 1, 1, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 1, 0, 1, 0, 0, 0),
	c(0, 0, 1, 1, 0, 1, 0),
	c(0, 1, 0, 1, 0, 0, 0),
	c(0, 1, 1, 0, 0, 0, 0)
)
expect_equal(pt_stats[, , "psABAY"], psABAY)

# recencyContinue  
recencyContinue <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(1/2, 0.0, 0.0, 0.0, 0.0, 0, 0.0),
	c(1/3, 1/2, 0.0, 0.0, 0.0, 0, 0.0),
	c(1/2, 1/3, 0.0, 0.0, 0.0, 0, 0.0),
	c(1/3, 1/4, 1/2, 0.0, 0.0, 0, 0.0),
	c(1/4, 1/2, 1/3, 0.0, 1/2, 0, 1/2),
	c(1/5, 1/3, 1/2, 0.0, 1/3, 0, 1/3),
	c(1/6, 1/4, 1/3, 1/2, 1/4, 0, 1/4)
)
expect_equal(pt_stats[, , "recencyContinue"], recencyContinue)

# rrankSend
rrankSend <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(1, 0.0, 0.0, 0.0, 0, 0, 0),
	c(1, 1.0, 0.0, 0.0, 0, 0, 0),
	c(1, 1.0, 0.0, 0.0, 0, 0, 0),
	c(1, 1/2, 1.0, 0.0, 0, 0, 0),
	c(1, 1.0, 1/2, 0.0, 1, 0, 1),
	c(1, 1/2, 1.0, 0.0, 1, 0, 1),
	c(1, 1/3, 1/2, 1.0, 1, 0, 1)
)
expect_equal(pt_stats[, , "rrankSend"], rrankSend)

# Method = "pe"
pe_stats <- remstats(reh, tie_effects = effects, method = "pe")
riskset <- attr(pe_stats, "riskset")

# Baseline
expect_equal(pe_stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# outdegreeSender
outdegreeSender <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 1, 1, 1, 0, 0, 0),
  c(2, 1, 1, 1, 0, 0, 0),
  c(2, 2, 2, 2, 0, 0, 0),
  c(2, 2, 2, 2, 1, 0, 0),
  c(2, 2, 2, 2, 1, 1, 1),
  c(2, 3, 3, 3, 1, 1, 1),
  c(2, 4, 4, 4, 1, 1, 1),
  c(2, 5, 5, 5, 1, 1, 1)
)
expect_equal(pe_stats[, , "outdegreeSender"], outdegreeSender)

# inertia
inertia <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 0),
  c(2, 1, 0, 0, 0, 0, 0),
  c(2, 1, 1, 0, 0, 0, 0),
  c(2, 1, 1, 0, 1, 0, 0),
  c(2, 1, 1, 0, 1, 0, 1),
  c(2, 2, 1, 0, 1, 0, 1),
  c(2, 2, 2, 0, 1, 0, 1),
  c(2, 2, 2, 1, 1, 0, 1)
)
expect_equal(pe_stats[, , "inertia"], inertia)

# reciprocity
reciprocity <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 1, 0, 0),
  c(0, 0, 1, 0, 1, 0, 0),
  c(0, 0, 1, 0, 1, 0, 0),
  c(0, 0, 1, 0, 2, 0, 0),
  c(0, 0, 1, 0, 2, 0, 0)
)
expect_equal(pe_stats[, , "reciprocity"], reciprocity)

# itp
itp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(1, 1, 0, 0, 1, 0, 0),
  c(1, 1, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 2, 0, 0),
  c(1, 1, 0, 1, 2, 0, 0),
  c(1, 1, 0, 1, 3, 0, 1)
)
expect_equal(pe_stats[, , "itp"], itp)

# psABBA
psABBA <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0)
)
expect_equal(pe_stats[, , "psABBA"], psABBA)

# psABAY
psABAY <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 1, 1, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 1, 0, 1, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 1, 0),
	c(0, 0, 1, 1, 0, 0, 0),
	c(0, 1, 0, 1, 0, 0, 0),
	c(0, 1, 1, 0, 0, 0, 0)
)
expect_equal(pe_stats[, , "psABAY"], psABAY)

# recencyContinue  
recencyContinue <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1/2, 0.0, 0.0, 0.0, 0.0, 0, 0.0),
  c(1/3, 1/2, 0.0, 0.0, 0.0, 0, 0.0),
  c(1/2, 1/3, 0.0, 0.0, 0.0, 0, 0.0),
  c(1/3, 1/4, 1/2, 0.0, 0.0, 0, 0.0),
  c(1/3, 1/4, 1/2, 0.0, 1.0, 0, 0.0),
  c(1/3, 1/4, 1/2, 0.0, 1.0, 0, 1.0),
  c(1/4, 1/2, 1/3, 0.0, 1/2, 0, 1/2),
  c(1/5, 1/3, 1/2, 0.0, 1/3, 0, 1/3),
  c(1/6, 1/4, 1/3, 1/2, 1/4, 0, 1/4)
)
expect_equal(pe_stats[, , "recencyContinue"], recencyContinue)

# rrankSend
rrankSend <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0.0, 0.0, 0.0, 0, 0, 0),
  c(1, 1.0, 0.0, 0.0, 0, 0, 0),
  c(1, 1.0, 0.0, 0.0, 0, 0, 0),
  c(1, 1/2, 1.0, 0.0, 0, 0, 0),
  c(1, 1/2, 1.0, 0.0, 1, 0, 0),
  c(1, 1/2, 1.0, 0.0, 1, 0, 1),
  c(1, 1.0, 1/2, 0.0, 1, 0, 1),
  c(1, 1/2, 1.0, 0.0, 1, 0, 1),
  c(1, 1/3, 1/2, 1.0, 1, 0, 1)
)
expect_equal(pe_stats[, , "rrankSend"], rrankSend)