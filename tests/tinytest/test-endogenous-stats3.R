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
  outdegreeSender() + outdegreeReceiver() +
    indegreeSender() + indegreeReceiver() +
    totaldegreeSender() + totaldegreeReceiver() +
    inertia() + reciprocity() +
    isp() + itp() + osp() + otp() +
    isp(unique = TRUE) + itp(unique = TRUE) + 
    osp(unique = TRUE) + otp(unique = TRUE) +
    psABBA() + psABBY() + psABAB() + psABBY() +
    psABXA() + psABXB() + psABXY() +
    recencyContinue() +
    recencySendSender() + recencySendReceiver() +
    recencyReceiveSender() + recencyReceiveReceiver() +
    rrankSend() + rrankReceive() +
    outdegreeSender(consider_type = TRUE) +
    outdegreeReceiver(consider_type = TRUE) +
    indegreeSender(consider_type = TRUE) +
    indegreeReceiver(consider_type = TRUE) +
    totaldegreeSender(consider_type = TRUE) +
    totaldegreeReceiver(consider_type = TRUE) +
    inertia(consider_type = TRUE) + reciprocity(consider_type = TRUE) +
    isp(consider_type = TRUE) + itp(consider_type = TRUE) +
    osp(consider_type = TRUE) + otp(consider_type = TRUE) +
    isp(unique = TRUE, consider_type = TRUE) + 
    itp(unique = TRUE, consider_type = TRUE) +
    osp(unique = TRUE, consider_type = TRUE) + 
    otp(unique = TRUE, consider_type = TRUE) +    
    psABBA(consider_type = TRUE) + psABBY(consider_type = TRUE) +
    psABAB(consider_type = TRUE) + psABBY(consider_type = TRUE) +
    psABXA(consider_type = TRUE) + psABXB(consider_type = TRUE) +
    psABXY(consider_type = TRUE) +
    recencyContinue(consider_type = TRUE) +
    recencySendSender(consider_type = TRUE) +
    recencySendReceiver(consider_type = TRUE) +
    recencyReceiveSender(consider_type = TRUE) +
    recencyReceiveReceiver(consider_type = TRUE) +
    rrankSend(consider_type = TRUE) + rrankReceive(consider_type = TRUE)
stats <- remstats(reh, tie_effects = effects)
riskset <- attr(stats, "riskset")

# Baseline
expect_equal(stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# outdegreeSender
outdegreeSender <- rbind(
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
expect_equal(stats[, , "outdegreeSender"], outdegreeSender)

# outdegreeSender.type
outdegreeSender.type <- rbind(
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
expect_equal(stats[, , "outdegreeSender.type"], outdegreeSender.type)

# outdegreeReceiver
outdegreeReceiver <- rbind(
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
expect_equal(stats[, , "outdegreeReceiver"], outdegreeReceiver)

# outdegreeReceiver.type
outdegreeReceiver.type <- rbind(
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
expect_equal(stats[, , "outdegreeReceiver.type"], outdegreeReceiver.type)

# indegreeSender
indegreeSender <- rbind(
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
expect_equal(stats[, , "indegreeSender"], indegreeSender)

# indegreeSender.type
indegreeSender.type <- rbind(
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
expect_equal(stats[, , "indegreeSender.type"], indegreeSender.type)

# indegreeReceiver
indegreeReceiver <- rbind(
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
expect_equal(stats[, , "indegreeReceiver"], indegreeReceiver)

# indegreeReceiver.type
indegreeReceiver.type <- rbind(
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
expect_equal(stats[, , "indegreeReceiver.type"], indegreeReceiver.type)

# totaldegreeSender
totaldegreeSender <- indegreeSender + outdegreeSender
expect_equal(stats[, , "totaldegreeSender"], totaldegreeSender)

# totaldegreeSender.type
totaldegreeSender.type <- indegreeSender.type + outdegreeSender.type
expect_equal(stats[, , "totaldegreeSender.type"], totaldegreeSender.type)

# totaldegreeReceiver
totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
expect_equal(stats[, , "totaldegreeReceiver"], totaldegreeReceiver)

# totaldegreeReceiver.type
totaldegreeReceiver.type <- indegreeReceiver.type + outdegreeReceiver.type
expect_equal(stats[, , "totaldegreeReceiver.type"], totaldegreeReceiver.type)

# inertia
inertia <- rbind(
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
expect_equal(stats[, , "inertia"], inertia)

# inertia.type
inertia.type <- rbind(
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
expect_equal(stats[, , "inertia.type"], inertia.type)

# reciprocity
reciprocity <- rbind(
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
expect_equal(stats[, , "reciprocity"], reciprocity)

# reciprocity.type
reciprocity.type <- rbind(
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
expect_equal(stats[, , "reciprocity.type"], reciprocity.type)

# itp
itp <- rbind(
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
expect_equal(stats[, , "itp"], itp)

# itp.type
itp.type <- rbind(
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
expect_equal(stats[, , "itp.type"], itp.type)

# itp.unique
itp.unique <- rbind(
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
expect_equal(stats[, , "itp.unique"], itp.unique)

# itp.unique.type
itp.unique.type <- rbind(
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
expect_equal(stats[, , "itp.unique.type"], itp.unique.type)

# otp
otp <- rbind(
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
expect_equal(stats[, , "otp"], otp)

# otp.type
otp.type <- rbind(
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
expect_equal(stats[, , "otp.type"], otp.type)

# otp.unique
otp.unique <- rbind(
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
expect_equal(stats[, , "otp.unique"], otp.unique)

# otp.unique.type
otp.unique.type <- rbind(
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
expect_equal(stats[, , "otp.unique.type"], otp.unique.type)

# isp
isp <- rbind(
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
expect_equal(stats[, , "isp"], isp)

# isp.type
isp.type <- rbind(
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
expect_equal(stats[, , "isp.type"], isp.type)

# isp.unique
isp.unique <- rbind(
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
expect_equal(stats[, , "isp.unique"], isp.unique)

# isp.unique.type
isp.unique.type <- rbind(
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
expect_equal(stats[, , "isp.unique.type"], isp.unique.type)

# osp
osp <- rbind(
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
expect_equal(stats[, , "osp"], osp)

# osp.type
osp.type <- rbind(
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
expect_equal(stats[, , "osp.type"], osp.type)

# osp.unique
osp.unique <- rbind(
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
expect_equal(stats[, , "osp.unique"], osp.unique)

# osp.unique.type
osp.unique.type <- rbind(
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
expect_equal(stats[, , "osp.unique.type"], osp.unique.type)

# psABBA
psABBA <- rbind(
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
expect_equal(stats[, , "psABBA"], psABBA)

# psABBA.type
psABBA.type <- rbind(
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
expect_equal(stats[, , "psABBA.type"], psABBA.type)

# psABBY
psABBY <- rbind(
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
expect_equal(stats[, , "psABBY"], psABBY)

# psABBY.type
psABBY.type <- rbind(
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
expect_equal(stats[, , "psABBY.type"], psABBY.type)

# psABAB
psABAB <- rbind(
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
expect_equal(stats[, , "psABAB"], psABAB)

# psABAB.type
psABAB.type <- rbind(
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
expect_equal(stats[, , "psABAB.type"], psABAB.type)

# psABXB
psABXB <- rbind(
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
expect_equal(stats[, , "psABXB"], psABXB)

# psABXB.type
psABXB.type <- rbind(
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
expect_equal(stats[, , "psABXB.type"], psABXB.type)

# psABXY
psABXY <- rbind(
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
expect_equal(stats[, , "psABXY"], psABXY)

# psABXY.type
psABXY.type <- rbind(
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
expect_equal(stats[, , "psABXY.type"], psABXY.type)

# recencyContinue
recencyContinue <- rbind(
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
expect_equal(stats[, , "recencyContinue"], recencyContinue)

# recencyContinue.type
recencyContinue.type <- rbind(
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
expect_equal(stats[, , "recencyContinue.type"], recencyContinue.type)

# recencySendSender
recencySendSender <- rbind(
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
expect_equal(stats[, , "recencySendSender"], recencySendSender)

# recencySendSender.type
recencySendSender.type <- rbind(
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
expect_equal(stats[, , "recencySendSender.type"], recencySendSender.type)

# recencySendReceiver
recencySendReceiver <- rbind(
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
expect_equal(stats[, , "recencySendReceiver"], recencySendReceiver)

# recencySendReceiver.type
recencySendReceiver.type <- rbind(
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
expect_equal(stats[, , "recencySendReceiver.type"], recencySendReceiver.type)

# recencyReceiveSender
recencyReceiveSender <- rbind(
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
expect_equal(stats[, , "recencyReceiveSender"], recencyReceiveSender)

# recencyReceiveSender.type
recencyReceiveSender.type <- rbind(
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
expect_equal(stats[, , "recencyReceiveSender.type"], recencyReceiveSender.type)

# recencyReceiveReceiver
recencyReceiveReceiver <- rbind(
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
expect_equal(stats[, , "recencyReceiveReceiver"], recencyReceiveReceiver)

# recencyReceiveReceiver.type
recencyReceiveReceiver.type <- rbind(
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
expect_equal(stats[, , "recencyReceiveReceiver.type"], recencyReceiveReceiver.type)

# rrankSend
rrankSend <- rbind(
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
expect_equal(stats[, , "rrankSend"], rrankSend)

# rrankSend.type
rrankSend.type <- rbind(
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
expect_equal(stats[, , "rrankSend.type"], rrankSend.type)

# rrankReceive
rrankReceive <- rbind(
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
expect_equal(stats[, , "rrankReceive"], rrankReceive)

# rrankReceive.type
rrankReceive.type <- rbind(
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
expect_equal(stats[, , "rrankReceive.type"], rrankReceive.type)

# test standardization
std_effects <- ~
  outdegreeSender(scaling = "std") + outdegreeReceiver(scaling = "std") +
    indegreeSender(scaling = "std") + indegreeReceiver(scaling = "std") +
    totaldegreeSender(scaling = "std") + totaldegreeReceiver(scaling = "std") +
    inertia(scaling = "std") + reciprocity(scaling = "std") +
    isp(scaling = "std") + itp(scaling = "std") +
    osp(scaling = "std") + otp(scaling = "std") +
    isp(scaling = "std", unique = TRUE) + itp(scaling = "std", unique = TRUE) + 
    osp(scaling = "std", unique = TRUE) + otp(scaling = "std", unique = TRUE) +
    outdegreeSender(consider_type = TRUE, scaling = "std") +
    outdegreeReceiver(consider_type = TRUE, scaling = "std") +
    indegreeSender(consider_type = TRUE, scaling = "std") +
    indegreeReceiver(consider_type = TRUE, scaling = "std") +
    totaldegreeSender(consider_type = TRUE, scaling = "std") +
    totaldegreeReceiver(consider_type = TRUE, scaling = "std") +
    inertia(consider_type = TRUE, scaling = "std") +
    reciprocity(consider_type = TRUE, scaling = "std") +
    isp(consider_type = TRUE, scaling = "std") +
    itp(consider_type = TRUE, scaling = "std") +
    osp(consider_type = TRUE, scaling = "std") +
    otp(consider_type = TRUE, scaling = "std") +
    isp(consider_type = TRUE, scaling = "std", unique = TRUE) + 
    itp(consider_type = TRUE, scaling = "std", unique = TRUE) + 
    osp(consider_type = TRUE, scaling = "std", unique = TRUE) +
    otp(consider_type = TRUE, scaling = "std", unique = TRUE) 
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
    inertia(scaling = "prop") + reciprocity(scaling = "prop") +
    outdegreeSender(consider_type = TRUE, scaling = "prop") +
    outdegreeReceiver(consider_type = TRUE, scaling = "prop") +
    indegreeSender(consider_type = TRUE, scaling = "prop") +
    indegreeReceiver(consider_type = TRUE, scaling = "prop") +
    totaldegreeSender(consider_type = TRUE, scaling = "prop") +
    totaldegreeReceiver(consider_type = TRUE, scaling = "prop") +
    inertia(consider_type = TRUE, scaling = "prop") +
    reciprocity(consider_type = TRUE, scaling = "prop")
prop_stats <- remstats(reh, tie_effects = prop_effects)

sapply(c(2:5, 10:13), function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[, , stat_name] / (1:nrow(stats) - 1)
  scaled_original[1, ] <- 1 / 4
  expect_equal(prop_stats[, , stat_name], scaled_original)
}) # in- and out-degree of the sender and receiver

sapply(c(6:7, 14:15), function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[, , stat_name] / (2 * (1:nrow(stats) - 1))
  scaled_original[1, ] <- 1 / 4
  expect_equal(prop_stats[, , stat_name], scaled_original)
}) # total degree of the sender and receiver

# inertia
prop_inertia <- stats[, , "inertia"] / stats[, , "outdegreeSender"]
prop_inertia[stats[, , "outdegreeSender"] == 0] <- 1 / 3
expect_equal(prop_stats[, , "inertia"], prop_inertia)

# inertia.type
prop_inertia.type <- stats[, , "inertia.type"] / stats[, , "outdegreeSender.type"]
prop_inertia.type[stats[, , "outdegreeSender.type"] == 0] <- 1 / 3
expect_equal(prop_stats[, , "inertia.type"], prop_inertia.type)

# reciprocity
prop_reciprocity <- stats[, , "reciprocity"] / stats[, , "indegreeSender"]
prop_reciprocity[stats[, , "indegreeSender"] == 0] <- 1 / 3
expect_equal(prop_stats[, , "reciprocity"], prop_reciprocity)

# reciprocity.type
prop_reciprocity.type <- stats[, , "reciprocity.type"] /
  stats[, , "indegreeSender.type"]
prop_reciprocity.type[stats[, , "indegreeSender.type"] == 0] <- 1 / 3
expect_equal(prop_stats[, , "reciprocity.type"], prop_reciprocity.type)
