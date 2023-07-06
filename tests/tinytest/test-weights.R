# Small edgelist
edgelist <- data.frame(
  time = 1:5,
  actor1 = c(1, 1, 2, 2, 3),
  actor2 = c(2, 3, 1, 3, 2)
)

# Event weights
weights <- c(0.15, 0.25, 0.35, 0.45, 0.55)
edgelist$weight <- weights

# Test for tie-oriented model (weights are used in "adjmat", only need to test for one statistic)
reh <- remify::remify(edgelist, model = "tie")
effects <- ~ inertia()
stats <- remstats(reh, tie_effects = effects)
riskset <- attr(stats, "riskset")

inertia <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0.15, 0, 0, 0, 0, 0),
  c(0.15, 0.25, 0, 0, 0, 0),
  c(0.15, 0.25, 0.35, 0, 0, 0),
  c(0.15, 0.25, 0.35, 0.45, 0, 0)
)
expect_equal(stats[, , "inertia"], inertia)

# Actor-oriented model
reh <- remify::remify(edgelist, model = "actor")
sender_effects <- ~
  indegreeSender() + outdegreeSender() + totaldegreeSender() 
receiver_effects <- ~
  indegreeReceiver() + outdegreeReceiver() + totaldegreeReceiver() +
    inertia() + reciprocity() +
    isp() + itp() + osp() + otp() 
stats <- remstats(reh,
  sender_effects = sender_effects,
  receiver_effects = receiver_effects
)
sender_stats <- stats$sender_stats
receiver_stats <- stats$receiver_stats
actors <- attr(reh, "dictionary")$actors

# outdegreeSender
outdegreeSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0.15, 0, 0),
  c(0.40, 0, 0),
  c(0.40, 0.35, 0),
  c(0.40, 0.80, 0)
)
expect_equal(sender_stats[, , "outdegreeSender"], outdegreeSender)

# indegreeSender
indegreeSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0.15, 0),
  c(0, 0.15, 0.25),
  c(0.35, 0.15, 0.25),
  c(0.35, 0.15, 0.70)
)
expect_equal(sender_stats[, , "indegreeSender"], indegreeSender)

# totaldegreeSender
totaldegreeSender <- indegreeSender + outdegreeSender
expect_equal(sender_stats[, , "totaldegreeSender"], totaldegreeSender)

# outdegreeReceiver
outdegreeReceiver <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0.15, 0, 0),
  c(0.40, 0, 0),
  c(0.40, 0.35, 0),
  c(0.40, 0.80, 0)
)
expect_equal(receiver_stats[, , "outdegreeReceiver"], outdegreeReceiver)

# indegreeReceiver
indegreeReceiver <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0.15, 0),
  c(0, 0.15, 0.25),
  c(0.35, 0.15, 0.25),
  c(0.35, 0.15, 0.70)
)
expect_equal(receiver_stats[, , "indegreeReceiver"], indegreeReceiver)

# totaldegreeReceiver
totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
expect_equal(receiver_stats[, , "totaldegreeReceiver"], totaldegreeReceiver)

# inertia
inertia <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0.15, 0),
  c(0, 0, 0),
  c(0.35, 0, 0),
  c(0, 0, 0)
)
expect_equal(receiver_stats[, , "inertia"], inertia)

# reciprocity
reciprocity <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0),
  c(0.15, 0, 0),
  c(0.15, 0, 0),
  c(0.25, 0.45, 0)
)
expect_equal(receiver_stats[, , "reciprocity"], reciprocity)

# itp
itp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0),
  c(0, 0, 0),
  c(0, 0, 0),
  c(0.15, 0.25, 0)
)
expect_equal(receiver_stats[, , "itp"], itp)

# otp
otp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0),
  c(0, 0, 0),
  c(0, 0, 0.25),
  c(0, 0, 0)
)
expect_equal(receiver_stats[, , "otp"], otp)

# isp
isp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0),
  c(0, 0, 0.15),
  c(0, 0, 0.15),
  c(0.35, 0.15, 0)
)
expect_equal(receiver_stats[, , "isp"], isp)

# osp
osp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0),
  c(0, 0, 0),
  c(0, 0, 0),
  c(0, 0, 0)
)
expect_equal(receiver_stats[, , "osp"], osp)
