# Condition 3: Directed evens with types, tie-oriented model with active risk set

# Small edgelist
edgelist <- data.frame(
  time = 1:10,
  actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
  actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
)

event_types <- c(1, 1, 2, 2, 1, 2, 2, 1, 1, 1)

# Statistics
edgelist$type <- event_types
reh <- remify::remify(edgelist, model = "tie", riskset = "active")
effects <- ~
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
	rrankSend() + rrankReceive() +
	outdegreeSender(consider_type = FALSE) +
	outdegreeReceiver(consider_type = FALSE) +
	indegreeSender(consider_type = FALSE) +
	indegreeReceiver(consider_type = FALSE) +
	totaldegreeSender(consider_type = FALSE) +
	totaldegreeReceiver(consider_type = FALSE) +
	totaldegreeDyad(consider_type = FALSE) +
	inertia(consider_type = FALSE) + reciprocity(consider_type = FALSE) +
	isp(consider_type = FALSE) + itp(consider_type = FALSE) +
	osp(consider_type = FALSE) + otp(consider_type = FALSE) +
	isp(unique = TRUE, consider_type = FALSE) + 
	itp(unique = TRUE, consider_type = FALSE) +
	osp(unique = TRUE, consider_type = FALSE) + 
	otp(unique = TRUE, consider_type = FALSE) +    
	psABBA(consider_type = FALSE) + psABBY(consider_type = FALSE) +
	psABAB(consider_type = FALSE) + psABAY(consider_type = FALSE) +
	psABXA(consider_type = FALSE) + psABXB(consider_type = FALSE) +
	psABXY(consider_type = FALSE) +
	recencyContinue(consider_type = FALSE) +
	recencySendSender(consider_type = FALSE) +
	recencySendReceiver(consider_type = FALSE) +
	recencyReceiveSender(consider_type = FALSE) +
	recencyReceiveReceiver(consider_type = FALSE) +
	rrankSend(consider_type = FALSE) + rrankReceive(consider_type = FALSE)
stats <- remstats(reh, tie_effects = effects)
riskset <- attr(stats, "riskset")

# Baseline
expect_equal(stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# FEtype
FEtype <- cbind(matrix(0, nrow = nrow(edgelist), ncol = sum(riskset$type == 1)), 
	matrix(1, nrow = nrow(edgelist), ncol = sum(riskset$type == 2)))
expect_equal(stats[, , "FEtype_2"], FEtype)

# outdegreeSender.TypeAgg
outdegreeSender.TypeAgg <- rbind(
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
expect_equal(stats[, , "outdegreeSender.TypeAgg"], outdegreeSender.TypeAgg)

# outdegreeSender
outdegreeSender <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
  c(1, 1, 1, 1, 0, 0, 1, 0, 0, 0),
  c(1, 1, 1, 1, 0, 0, 1, 1, 1, 0),
  c(1, 1, 1, 1, 1, 0, 1, 1, 1, 0),
  c(1, 1, 1, 1, 1, 0, 1, 1, 1, 1),
  c(1, 1, 1, 1, 1, 0, 1, 2, 2, 1),
  c(1, 2, 2, 2, 1, 0, 1, 2, 2, 1),
  c(1, 3, 3, 3, 1, 0, 1, 2, 2, 1)
)
expect_equal(stats[, , "outdegreeSender"], outdegreeSender)

# outdegreeReceiver.TypeAgg
outdegreeReceiver.TypeAgg <- rbind(
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
expect_equal(stats[, , "outdegreeReceiver.TypeAgg"], outdegreeReceiver.TypeAgg)

# outdegreeReceiver
outdegreeReceiver <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 1, 0, 0, 0, 0),
  c(0, 1, 0, 0, 1, 1, 0, 0, 0, 0),
  c(0, 1, 0, 0, 1, 1, 0, 1, 0, 0),
  c(0, 1, 0, 0, 1, 1, 0, 1, 0, 0),
  c(1, 1, 1, 0, 1, 1, 0, 1, 0, 0),
  c(1, 1, 1, 0, 1, 1, 0, 1, 0, 0),
  c(1, 1, 1, 0, 1, 1, 0, 1, 0, 0),
  c(1, 1, 1, 0, 2, 1, 0, 1, 0, 0),
  c(1, 1, 1, 0, 3, 1, 0, 1, 0, 0)
)
expect_equal(stats[, , "outdegreeReceiver"], outdegreeReceiver)

# indegreeSender.TypeAgg
indegreeSender.TypeAgg <- rbind(
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
expect_equal(stats[, , "indegreeSender.TypeAgg"], indegreeSender.TypeAgg)

# indegreeSender
indegreeSender <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
  c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
  c(1, 1, 1, 1, 1, 0, 1, 0, 0, 0),
  c(1, 1, 1, 1, 2, 0, 1, 0, 0, 0),
  c(1, 1, 1, 1, 2, 1, 1, 0, 0, 0)
)
expect_equal(stats[, , "indegreeSender"], indegreeSender)

# indegreeReceiver.TypeAgg
indegreeReceiver.TypeAgg <- rbind(
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
expect_equal(stats[, , "indegreeReceiver.TypeAgg"], indegreeReceiver.TypeAgg)

# indegreeReceiver
indegreeReceiver <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(1, 1, 1, 0, 0, 1, 0, 0, 0, 0),
  c(1, 1, 1, 0, 0, 1, 1, 0, 1, 1),
  c(1, 1, 1, 0, 0, 1, 2, 0, 2, 2),
  c(1, 1, 1, 0, 1, 1, 2, 0, 2, 2),
  c(1, 1, 1, 0, 1, 1, 3, 0, 3, 3),
  c(1, 1, 1, 0, 1, 1, 3, 1, 3, 3),
  c(2, 1, 2, 0, 1, 1, 3, 1, 3, 3),
  c(2, 1, 2, 1, 1, 1, 3, 1, 3, 3)
)
expect_equal(stats[, , "indegreeReceiver"], indegreeReceiver)

# totaldegreeSender
totaldegreeSender <- indegreeSender + outdegreeSender
expect_equal(stats[, , "totaldegreeSender"], totaldegreeSender)

# totaldegreeSender.TypeAgg
totaldegreeSender.TypeAgg <- indegreeSender.TypeAgg + outdegreeSender.TypeAgg
expect_equal(stats[, , "totaldegreeSender.TypeAgg"], totaldegreeSender.TypeAgg)

# totaldegreeReceiver
totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
expect_equal(stats[, , "totaldegreeReceiver"], totaldegreeReceiver)

# totaldegreeReceiver.TypeAgg
totaldegreeReceiver.TypeAgg <- indegreeReceiver.TypeAgg + outdegreeReceiver.TypeAgg
expect_equal(stats[, , "totaldegreeReceiver.TypeAgg"], totaldegreeReceiver.TypeAgg)

# totaldegreeDyad.TypeAgg
totaldegreeDyad.TypeAgg <- totaldegreeSender.TypeAgg + totaldegreeReceiver.TypeAgg
expect_equal(stats[, , "totaldegreeDyad.TypeAgg"], totaldegreeDyad.TypeAgg)

# totaldegreeDyad
totaldegreeDyad <- totaldegreeSender + totaldegreeReceiver
expect_equal(stats[, , "totaldegreeDyad"], totaldegreeDyad)

# inertia.TypeAgg
inertia.TypeAgg <- rbind(
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
expect_equal(stats[, , "inertia.TypeAgg"], inertia.TypeAgg)

# inertia
inertia <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 1, 0, 1, 0),
  c(1, 1, 0, 0, 1, 0, 1, 0, 1, 0),
  c(1, 1, 0, 0, 1, 0, 1, 0, 1, 1),
  c(1, 1, 0, 0, 1, 0, 1, 1, 1, 1),
  c(1, 1, 1, 0, 1, 0, 1, 1, 1, 1),
  c(1, 1, 1, 1, 1, 0, 1, 1, 1, 1)
)
expect_equal(stats[, , "inertia"], inertia)

# reciprocity.TypeAgg
reciprocity.TypeAgg <- rbind(
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
expect_equal(stats[, , "reciprocity.TypeAgg"], reciprocity.TypeAgg)

# reciprocity
reciprocity <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 1, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "reciprocity"], reciprocity)

# itp.TypeAgg
itp.TypeAgg <- rbind(
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
expect_equal(stats[, , "itp.TypeAgg"], itp.TypeAgg)

# itp
itp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 1, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "itp"], itp)

# itp.unique.TypeAgg
itp.unique.TypeAgg <- rbind(
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
expect_equal(stats[, , "itp.unique.TypeAgg"], itp.unique.TypeAgg)

# itp.unique
itp.unique <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 1, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "itp.unique"], itp.unique)

# otp.TypeAgg
otp.TypeAgg <- rbind(
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
expect_equal(stats[, , "otp.TypeAgg"], otp.TypeAgg)

# otp
otp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0)
)
expect_equal(stats[, , "otp"], otp)

# otp.unique.TypeAgg
otp.unique.TypeAgg <- rbind(
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
expect_equal(stats[, , "otp.unique.TypeAgg"], otp.unique.TypeAgg)

# otp.unique
otp.unique <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0)
)
expect_equal(stats[, , "otp.unique"], otp.unique)

# isp.TypeAgg
isp.TypeAgg <- rbind(
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
expect_equal(stats[, , "isp.TypeAgg"], isp.TypeAgg)

# isp
isp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 0, 1, 1, 0, 0, 0)
)
expect_equal(stats[, , "isp"], isp)

# isp.unique.TypeAgg
isp.unique.TypeAgg <- rbind(
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
expect_equal(stats[, , "isp.unique.TypeAgg"], isp.unique.TypeAgg)

# isp.unique
isp.unique <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 0, 0, 0, 0, 1, 1, 0, 0, 0)
)
expect_equal(stats[, , "isp.unique"], isp.unique)

# osp.TypeAgg
osp.TypeAgg <- rbind(
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
expect_equal(stats[, , "osp.TypeAgg"], osp.TypeAgg)

# osp
osp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0)
)
expect_equal(stats[, , "osp"], osp)

# osp.unique.TypeAgg
osp.unique.TypeAgg <- rbind(
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
expect_equal(stats[, , "osp.unique.TypeAgg"], osp.unique.TypeAgg)

# osp.unique
osp.unique <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0)
)
expect_equal(stats[, , "osp.unique"], osp.unique)

# psABBA.TypeAgg
psABBA.TypeAgg <- rbind(
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
expect_equal(stats[, , "psABBA.TypeAgg"], psABBA.TypeAgg)

# psABBA
psABBA <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABBA"], psABBA)

# psABBY.TypeAgg
psABBY.TypeAgg <- rbind(
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
expect_equal(stats[, , "psABBY.TypeAgg"], psABBY.TypeAgg)

# psABBY
psABBY <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 1, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABBY"], psABBY)

# psABAB.TypeAgg
psABAB.TypeAgg <- rbind(
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
expect_equal(stats[, , "psABAB.TypeAgg"], psABAB.TypeAgg)

# psABAB
psABAB <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABAB"], psABAB)

# psABXB.TypeAgg
psABXB.TypeAgg <- rbind(
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
expect_equal(stats[, , "psABXB.TypeAgg"], psABXB.TypeAgg)

# psABXB
psABXB <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1),
	c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1),
	c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
	c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABXB"], psABXB)

# psABXY.TypeAgg
psABXY.TypeAgg <- rbind(
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
expect_equal(stats[, , "psABXY.TypeAgg"], psABXY.TypeAgg)

# psABXY
psABXY <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABXY"], psABXY)

# psABXA.TypeAgg
psABXA.TypeAgg <- rbind(
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
expect_equal(stats[, , "psABXA.TypeAgg"], psABXA.TypeAgg)

# psABXA
psABXA <- rbind(
	matrix(0.0, ncol = nrow(riskset)),
	c(0, 1, 0, 0, 0, 1, 0, 0, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABXA"], psABXA)

# psABAY.TypeAgg
psABAY.TypeAgg <- rbind(
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
expect_equal(stats[, , "psABAY.TypeAgg"], psABAY.TypeAgg)

# psABAY
psABAY <- rbind(
	matrix(0.0, ncol = nrow(riskset)),
	c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
	c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
	c(0, 1, 0, 1, 0, 0, 0, 0, 0, 0),
	c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABAY"], psABAY)

# recencyContinue.TypeAgg
recencyContinue.TypeAgg <- rbind(
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
expect_equal(stats[, , "recencyContinue.TypeAgg"], recencyContinue.TypeAgg)

# recencyContinue
recencyContinue <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(1/2, 0.0, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0),
  c(1/3, 1/2, 0.0, 0.0, 0.0, 0, 0.0, 0.0, 0.0, 0.0),
  c(1/4, 1/3, 0.0, 0.0, 0.0, 0, 1/2, 0.0, 0.0, 0.0),
  c(1/5, 1/4, 0.0, 0.0, 0.0, 0, 1/3, 0.0, 1/2, 0.0),
  c(1/6, 1/5, 0.0, 0.0, 1/2, 0, 1/4, 0.0, 1/3, 0.0),
  c(1/7, 1/6, 0.0, 0.0, 1/3, 0, 1/5, 0.0, 1/4, 1/2),
  c(1/8, 1/7, 0.0, 0.0, 1/4, 0, 1/6, 1/2, 1/5, 1/3),
  c(1/9, 1/8, 1/2, 0.0, 1/5, 0, 1/7, 1/3, 1/6, 1/4),
  c(1/10, 1/9, 1/3, 1/2, 1/6, 0, 1/8, 1/4, 1/7, 1/5)
)
expect_equal(stats[, , "recencyContinue"], recencyContinue)

# recencySendSender.TypeAgg
recencySendSender.TypeAgg <- rbind(
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
expect_equal(stats[, , "recencySendSender.TypeAgg"], recencySendSender.TypeAgg)

# recencySendSender
recencySendSender <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(1/2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
  c(1/3, 1/2, 1/2, 1/2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
  c(1/4, 1/3, 1/3, 1/3, 0.0, 0.0, 1/2, 0.0, 0.0, 0.0),
  c(1/5, 1/4, 1/4, 1/4, 0.0, 0.0, 1/3, 1/2, 1/2, 0.0),
  c(1/6, 1/5, 1/5, 1/5, 1/2, 0.0, 1/4, 1/3, 1/3, 0.0),
  c(1/7, 1/6, 1/6, 1/6, 1/3, 0.0, 1/5, 1/4, 1/4, 1/2),
  c(1/8, 1/7, 1/7, 1/7, 1/4, 0.0, 1/6, 1/2, 1/2, 1/3),
  c(1/9, 1/2, 1/2, 1/2, 1/5, 0.0, 1/7, 1/3, 1/3, 1/4),
  c(1/10, 1/2, 1/2, 1/2, 1/6, 0.0, 1/8, 1/4, 1/4, 1/5)
)
expect_equal(stats[, , "recencySendSender"], recencySendSender)

# recencySendReceiver.TypeAgg
recencySendReceiver.TypeAgg <- rbind(
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
expect_equal(stats[, , "recencySendReceiver.TypeAgg"], recencySendReceiver.TypeAgg)

# recencySendReceiver
recencySendReceiver <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(0.0, 1/2, 0.0, 0.0, 0.0, 1/2, 0.0, 0.0, 0.0, 0.0),
  c(0.0, 1/3, 0.0, 0.0, 1/2, 1/3, 0.0, 0.0, 0.0, 0.0),
  c(0.0, 1/4, 0.0, 0.0, 1/3, 1/4, 0.0, 1/2, 0.0, 0.0),
  c(0.0, 1/5, 0.0, 0.0, 1/4, 1/5, 0.0, 1/3, 0.0, 0.0),
  c(1/2, 1/6, 1/2, 0.0, 1/5, 1/6, 0.0, 1/4, 0.0, 0.0),
  c(1/3, 1/7, 1/3, 0.0, 1/6, 1/7, 0.0, 1/5, 0.0, 0.0),
  c(1/4, 1/8, 1/4, 0.0, 1/7, 1/8, 0.0, 1/6, 0.0, 0.0),
  c(1/5, 1/9, 1/5, 0.0, 1/2, 1/9, 0.0, 1/7, 0.0, 0.0),
  c(1/6, 1/10, 1/6, 0.0, 1/2, 1/10, 0.0, 1/8, 0.0, 0.0)
)
expect_equal(stats[, , "recencySendReceiver"], recencySendReceiver)

# recencyReceiveSender.TypeAgg
recencyReceiveSender.TypeAgg <- rbind(
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
expect_equal(stats[, , "recencyReceiveSender.TypeAgg"], recencyReceiveSender.TypeAgg)

# recencyReceiveSender
recencyReceiveSender <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(0.0, 0.0, 0.0, 0.0, 1/2, 0.0, 0.0, 0.0, 0.0, 0.0),
  c(1/2, 0.0, 0.0, 0.0, 1/3, 0.0, 0.0, 0.0, 0.0, 0.0),
  c(1/3, 0.0, 0.0, 0.0, 1/4, 0.0, 0.0, 0.0, 0.0, 0.0),
  c(1/4, 0.0, 0.0, 0.0, 1/5, 0.0, 0.0, 0.0, 0.0, 0.0),
  c(1/5, 1/2, 1/2, 1/2, 1/6, 0.0, 0.0, 0.0, 0.0, 0.0),
  c(1/6, 1/3, 1/3, 1/3, 1/7, 0.0, 0.0, 0.0, 0.0, 0.0),
  c(1/7, 1/4, 1/4, 1/4, 1/8, 0.0, 1/2, 0.0, 0.0, 0.0),
  c(1/8, 1/5, 1/5, 1/5, 1/2, 0.0, 1/3, 0.0, 0.0, 0.0),
  c(1/9, 1/6, 1/6, 1/6, 1/3, 1/2, 1/4, 0.0, 0.0, 0.0)
)
expect_equal(stats[, , "recencyReceiveSender"], recencyReceiveSender)

# recencyReceiveReceiver.TypeAgg
recencyReceiveReceiver.TypeAgg <- rbind(
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
expect_equal(stats[, , "recencyReceiveReceiver.TypeAgg"], recencyReceiveReceiver.TypeAgg)

# recencyReceiveReceiver
recencyReceiveReceiver <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(1/2, 0.0, 1/2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
  c(1/3, 1/2, 1/3, 0.0, 0.0, 1/2, 0.0, 0.0, 0.0, 0.0),
  c(1/4, 1/3, 1/4, 0.0, 0.0, 1/3, 1/2, 0.0, 1/2, 1/2),
  c(1/5, 1/4, 1/5, 0.0, 0.0, 1/4, 1/2, 0.0, 1/2, 1/2),
  c(1/6, 1/5, 1/6, 0.0, 1/2, 1/5, 1/3, 0.0, 1/3, 1/3),
  c(1/7, 1/6, 1/7, 0.0, 1/3, 1/6, 1/2, 0.0, 1/2, 1/2),
  c(1/8, 1/7, 1/8, 0.0, 1/4, 1/7, 1/3, 1/2, 1/3, 1/3),
  c(1/2, 1/8, 1/2, 0.0, 1/5, 1/8, 1/4, 1/3, 1/4, 1/4),
  c(1/3, 1/9, 1/3, 1/2, 1/6, 1/9, 1/5, 1/4, 1/5, 1/5)
)
expect_equal(stats[, , "recencyReceiveReceiver"], recencyReceiveReceiver)

# rrankSend.TypeAgg
rrankSend.TypeAgg <- rbind(
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
expect_equal(stats[, , "rrankSend.TypeAgg"], rrankSend.TypeAgg)

# rrankSend
rrankSend <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(1, 0.0, 0.0, 0.0, 0, 0, 0, 0.0, 0.0, 0),
  c(1, 1.0, 0.0, 0.0, 0, 0, 0, 0.0, 0.0, 0),
  c(1, 1.0, 0.0, 0.0, 0, 0, 1, 0.0, 0.0, 0),
  c(1, 1.0, 0.0, 0.0, 0, 0, 1, 0.0, 1.0, 0),
  c(1, 1.0, 0.0, 0.0, 1, 0, 1, 0.0, 1.0, 0),
  c(1, 1.0, 0.0, 0.0, 1, 0, 1, 0.0, 1.0, 1),
  c(1, 1.0, 0.0, 0.0, 1, 0, 1, 1.0, 1/2, 1),
  c(1, 1/2, 1.0, 0.0, 1, 0, 1, 1.0, 1/2, 1),
  c(1, 1/3, 1/2, 1.0, 1, 0, 1, 1.0, 1/2, 1)
)
expect_equal(stats[, , "rrankSend"], rrankSend)

# rrankReceive.TypeAgg
rrankReceive.TypeAgg <- rbind(
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
expect_equal(stats[, , "rrankReceive.TypeAgg"], rrankReceive.TypeAgg)

# rrankReceive
rrankReceive <- rbind(
  matrix(0.0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 1, 0, 0, 0, 0, 0)
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
    osp(scaling = "std", unique = TRUE) + otp(scaling = "std", unique = TRUE) +
    outdegreeSender(consider_type = FALSE, scaling = "std") +
    outdegreeReceiver(consider_type = FALSE, scaling = "std") +
    indegreeSender(consider_type = FALSE, scaling = "std") +
    indegreeReceiver(consider_type = FALSE, scaling = "std") +
    totaldegreeSender(consider_type = FALSE, scaling = "std") +
    totaldegreeReceiver(consider_type = FALSE, scaling = "std") +
    totaldegreeDyad(consider_type = FALSE, scaling = "std") +
    inertia(consider_type = FALSE, scaling = "std") +
    reciprocity(consider_type = FALSE, scaling = "std") +
    isp(consider_type = FALSE, scaling = "std") +
    itp(consider_type = FALSE, scaling = "std") +
    osp(consider_type = FALSE, scaling = "std") +
    otp(consider_type = FALSE, scaling = "std") +
    isp(consider_type = FALSE, scaling = "std", unique = TRUE) + 
    itp(consider_type = FALSE, scaling = "std", unique = TRUE) + 
    osp(consider_type = FALSE, scaling = "std", unique = TRUE) +
    otp(consider_type = FALSE, scaling = "std", unique = TRUE) 
std_stats <- remstats(reh, tie_effects = std_effects)

sapply(2:dim(std_stats)[3], function(p) {
  stat_name <- dimnames(std_stats)[[3]][p]
  scaled_original <- t(apply(stats[, , stat_name], 1, scale))
  scaled_original[which(apply(stats[, , stat_name], 1, sd) == 0), ] <-
    rep(0, ncol(stats))
  expect_equal(std_stats[, , stat_name], scaled_original)
})

# test proportional scaling
prop_effects <- ~
  outdegreeSender(scaling = "prop") + outdegreeReceiver(scaling = "prop") +
    indegreeSender(scaling = "prop") + indegreeReceiver(scaling = "prop") +
    totaldegreeSender(scaling = "prop") +
    totaldegreeReceiver(scaling = "prop") +
    totaldegreeDyad(scaling = "prop") +
    inertia(scaling = "prop") + reciprocity(scaling = "prop") +
    outdegreeSender(consider_type = FALSE, scaling = "prop") +
    outdegreeReceiver(consider_type = FALSE, scaling = "prop") +
    indegreeSender(consider_type = FALSE, scaling = "prop") +
    indegreeReceiver(consider_type = FALSE, scaling = "prop") +
    totaldegreeSender(consider_type = FALSE, scaling = "prop") +
    totaldegreeReceiver(consider_type = FALSE, scaling = "prop") +
    totaldegreeDyad(consider_type = FALSE, scaling = "prop") +
    inertia(consider_type = FALSE, scaling = "prop") +
    reciprocity(consider_type = FALSE, scaling = "prop")
prop_stats <- remstats(reh, tie_effects = prop_effects)

sapply(c(2:5, 11:14), function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[, , stat_name] / (1:nrow(stats) - 1)
  scaled_original[1, ] <- 1 / 4
  expect_equal(prop_stats[, , stat_name], scaled_original)
}) # in- and out-degree of the sender and receiver

sapply(c(6:7, 15:16), function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[, , stat_name] / (2 * (1:nrow(stats) - 1))
  scaled_original[1, ] <- 1 / 4
  expect_equal(prop_stats[, , stat_name], scaled_original)
}) # total degree of the sender and receiver

# totaldegreeDyad
prop_totaldegreeDyad <- stats[,,"totaldegreeDyad"] / (2*(1:nrow(stats)-1))
prop_totaldegreeDyad[1,] <- prop_totaldegreeDyad[1,] <- 1/4
expect_equal(prop_stats[,,"totaldegreeDyad"], prop_totaldegreeDyad)

# totaldegreeDyad.type
prop_totaldegreeDyad.TypeAgg <- stats[,,"totaldegreeDyad.TypeAgg"] / (2*(1:nrow(stats)-1))
prop_totaldegreeDyad.TypeAgg[1,] <- prop_totaldegreeDyad.TypeAgg[1,] <- 1/4
expect_equal(prop_stats[,,"totaldegreeDyad.TypeAgg"], prop_totaldegreeDyad.TypeAgg)

# inertia
prop_inertia <- stats[, , "inertia"] / stats[, , "outdegreeSender"]
prop_inertia[stats[, , "outdegreeSender"] == 0] <- 1 / 3
expect_equal(prop_stats[, , "inertia"], prop_inertia)

# inertia.TypeAgg
prop_inertia.TypeAgg <- stats[, , "inertia.TypeAgg"] / stats[, , "outdegreeSender.TypeAgg"]
prop_inertia.TypeAgg[stats[, , "outdegreeSender.TypeAgg"] == 0] <- 1 / 3
expect_equal(prop_stats[, , "inertia.TypeAgg"], prop_inertia.TypeAgg)

# reciprocity
prop_reciprocity <- stats[, , "reciprocity"] / stats[, , "indegreeSender"]
prop_reciprocity[stats[, , "indegreeSender"] == 0] <- 1 / 3
expect_equal(prop_stats[, , "reciprocity"], prop_reciprocity)

# reciprocity.TypeAgg
prop_reciprocity.TypeAgg <- stats[, , "reciprocity.TypeAgg"] /
  stats[, , "indegreeSender.TypeAgg"]
prop_reciprocity.TypeAgg[stats[, , "indegreeSender.TypeAgg"] == 0] <- 1 / 3
expect_equal(prop_stats[, , "reciprocity.TypeAgg"], prop_reciprocity.TypeAgg)

# Test method -------------------------------------------------------------
# Small change to the times in the edgelist
edgelist <- data.frame(
	time = c(1, 2, 3, 4, 5, 5, 5, 6, 7, 8),
	actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
	actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
)

event_types <- c(1, 1, 2, 2, 1, 2, 2, 1, 1, 1)
edgelist$type <- event_types
reh <- remify::remify(edgelist, model = "tie", riskset = "active")

# Selection of effects that have unique underlying cpp functions
effects <- ~ FEtype() + inertia(consider_type = FALSE) + 
  itp(consider_type = FALSE) 

stats <- remstats(reh, tie_effects = effects, method = "pt")
riskset <- attr(stats, "riskset")

# FEtype
FEtype <- cbind(
  matrix(0, nrow = NROW(unique(edgelist$time)), ncol = sum(riskset$type == 1)), 
	matrix(1, nrow = NROW(unique(edgelist$time)), ncol = sum(riskset$type == 2)))
expect_equal(stats[, , "FEtype_2"], FEtype)

# inertia.TypeAgg
inertia.TypeAgg <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 1, 1, 0, 0),
  c(2, 1, 0, 0, 0, 0, 2, 1, 0, 0),
  c(2, 1, 1, 0, 0, 0, 2, 1, 1, 0),
  c(2, 2, 1, 0, 1, 0, 2, 2, 1, 1),
  c(2, 2, 2, 0, 1, 0, 2, 2, 2, 1),
  c(2, 2, 2, 1, 1, 0, 2, 2, 2, 1)
)
expect_equal(stats[, , "inertia.TypeAgg"], inertia.TypeAgg)

# itp.TypeAgg
itp.TypeAgg <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 1, 2, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 2, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 3, 0, 1, 1, 0, 1)
)
expect_equal(stats[, , "itp.TypeAgg"], itp.TypeAgg)
