# tie-oriented model 
# --------------------------------------------------------------------
# Small edgelist
edgelist <- data.frame(
  time = 1:10,
  actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
  actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
)

reh <- remify::remify(edgelist, model = "tie", riskset = "active")
effects <- ~ inertia()

stats_window <- remstats(
  reh = reh, tie_effects = effects, memory = "window",
  memory_value = 5
)
riskset <- attr(stats_window, "riskset")

inertia_window <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 0),
  c(2, 1, 0, 0, 0, 0, 0),
  c(2, 1, 1, 0, 0, 0, 0),
  c(2, 1, 1, 0, 1, 0, 0),
  c(1, 1, 1, 0, 1, 0, 1),
  c(1, 1, 1, 0, 1, 0, 1),
  c(0, 1, 2, 0, 1, 0, 1),
  c(0, 1, 1, 1, 1, 0, 1)
)
expect_equal(stats_window[, , "inertia"], inertia_window)

stats_interval <- remstats(
  reh = reh, tie_effects = effects,
  memory = "interval", memory_value = c(2, 5)
)

intertia_interval <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 0),
  c(2, 1, 0, 0, 0, 0, 0),
  c(1, 1, 1, 0, 0, 0, 0),
  c(1, 0, 1, 0, 1, 0, 0),
  c(0, 0, 1, 0, 1, 0, 1),
  c(0, 1, 0, 0, 1, 0, 1)
)
expect_equal(stats_interval[, , "inertia"], intertia_interval)

stats_decay <- remstats(
  reh = reh, tie_effects = effects,
  memory = "decay", memory_value = 5
)

f <- function(time, time_event) {
  exp(-(time - time_event) * (log(2) / 5)) * (log(2) / 5)
}

inertia_decay <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(f(1, 1), 0, 0, 0, 0, 0, 0),
  c(f(2, 1), f(2, 2), 0, 0, 0, 0, 0),
  c(f(3, 1) + f(3, 3), f(3, 2), 0, 0, 0, 0, 0),
  c(f(4, 1) + f(4, 3), f(4, 2), f(4, 4), 0, 0, 0, 0),
  c(f(5, 1) + f(5, 3), f(5, 2), f(5, 4), 0, f(5, 5), 0, 0),
  c(f(6, 1) + f(6, 3), f(6, 2), f(6, 4), 0, f(6, 5), 0, f(6, 6)),
  c(f(7, 1) + f(7, 3), f(7, 2) + f(7, 7), f(7, 4), 0, f(7, 5), 0, f(7, 6)),
  c(f(8, 1) + f(8, 3), f(8, 2) + f(8, 7), f(8, 4) + f(8, 8), 0, f(8, 5), 0, f(8, 6)),
  c(f(9, 1) + f(9, 3), f(9, 2) + f(9, 7), f(9, 4) + f(9, 8), f(9, 9), f(9, 5), 0, f(9, 6))
)

expect_equal(stats_decay[, , "inertia"], inertia_decay)

# actor-oriented model 
# --------------------------------------------------------------------
# Small edgelist
edgelist <- data.frame(
  time = 1:10,
  actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
  actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
)

reh <- remify::remify(edgelist, model = "actor")
sender_effects <- ~
  indegreeSender() + outdegreeSender() + totaldegreeSender() +
    recencySendSender() + recencyReceiveSender()
receiver_effects <- ~
  indegreeReceiver() + outdegreeReceiver() + totaldegreeReceiver() +
    inertia() + reciprocity() +
    isp() + itp() + osp() + otp() +
    isp(unique = TRUE) + itp(unique = TRUE) + 
    osp(unique = TRUE) + otp(unique = TRUE) +
    recencyContinue() + recencySendReceiver() + recencyReceiveReceiver() +
    rrankSend() + rrankReceive()
stats_window <- remstats(reh, sender_effects = sender_effects,
  receiver_effects = receiver_effects, memory = "window", memory_value = 5
)
sender_statsW <- stats_window$sender_stats
receiver_statsW <- stats_window$receiver_stats
actors <- attr(reh, "dictionary")$actors

# outdegreeSender
outdegreeSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(1, 0, 0, 0),
  c(1, 1, 0, 0),
  c(2, 1, 0, 0),
  c(2, 2, 0, 0),
  c(2, 2, 1, 0),
  c(1, 2, 1, 1),
  c(1, 2, 1, 1),
  c(0, 3, 1, 1),
  c(0, 3, 1, 1)
)
expect_equal(sender_statsW[, , "outdegreeSender"], outdegreeSender)

# indegreeSender
indegreeSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 1, 0),
  c(1, 0, 1, 0),
  c(1, 0, 2, 0),
  c(1, 0, 3, 0),
  c(1, 1, 3, 0),
  c(1, 1, 3, 0),
  c(1, 1, 3, 0),
  c(1, 1, 3, 0),
  c(1, 1, 2, 1)
)
expect_equal(sender_statsW[, , "indegreeSender"], indegreeSender)

# totaldegreeSender
totaldegreeSender <- indegreeSender + outdegreeSender
expect_equal(sender_statsW[, , "totaldegreeSender"], totaldegreeSender)

# outdegreeReceiver
outdegreeReceiver <- outdegreeSender
expect_equal(receiver_statsW[, , "outdegreeReceiver"], outdegreeReceiver)

# indegreeReceiver
indegreeReceiver <- indegreeSender
expect_equal(receiver_statsW[, , "indegreeReceiver"], indegreeReceiver)

# totaldegreeReceiver
totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
expect_equal(receiver_statsW[, , "totaldegreeReceiver"], totaldegreeReceiver)

# inertia
inertia <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(1, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 1, 0),
  c(1, 0, 1, 0),
  c(1, 0, 2, 0),
  c(0, 0, 1, 0)
)
expect_equal(receiver_statsW[, , "inertia"], inertia)

# reciprocity
reciprocity <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 1, 0, 0),
  c(0, 0, 0, 0),
  c(2, 1, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 1, 0, 0)
)
expect_equal(receiver_statsW[, , "reciprocity"], reciprocity)

# itp
itp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 1, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 0, 1),
  c(1, 0, 0, 1),
  c(0, 0, 0, 1),
  c(0, 0, 1, 0)
)
expect_equal(receiver_statsW[, , "itp"], itp)

# interval memory
stats_interval <- remstats(reh, sender_effects = sender_effects,
  receiver_effects = receiver_effects, memory = "interval", 
  memory_value = c(2, 5)
)
sender_statsI <- stats_interval$sender_stats
receiver_statsI <- stats_interval$receiver_stats
actors <- attr(reh, "dictionary")$actors

# outdegreeSender
outdegreeSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 0, 0),
  c(1, 1, 0, 0),
  c(2, 1, 0, 0),
  c(1, 2, 0, 0),
  c(1, 1, 1, 0),
  c(0, 1, 1, 1),
  c(0, 1, 1, 1)
)
expect_equal(sender_statsI[, , "outdegreeSender"], outdegreeSender)

# indegreeSender
indegreeSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(1, 0, 1, 0),
  c(1, 0, 2, 0),
  c(1, 0, 2, 0),
  c(0, 1, 2, 0),
  c(0, 1, 2, 0),
  c(1, 1, 1, 0)
)
expect_equal(sender_statsI[, , "indegreeSender"], indegreeSender)

# totaldegreeSender
totaldegreeSender <- indegreeSender + outdegreeSender
expect_equal(sender_statsI[, , "totaldegreeSender"], totaldegreeSender)

# outdegreeReceiver
outdegreeReceiver <- outdegreeSender
expect_equal(receiver_statsI[, , "outdegreeReceiver"], outdegreeReceiver)

# indegreeReceiver
indegreeReceiver <- indegreeSender
expect_equal(receiver_statsI[, , "indegreeReceiver"], indegreeReceiver)

# totaldegreeReceiver
totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
expect_equal(receiver_statsI[, , "totaldegreeReceiver"], totaldegreeReceiver)

# inertia
inertia <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0)
)
expect_equal(receiver_statsI[, , "inertia"], inertia)

# reciprocity
reciprocity <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 0, 0, 0)
)
expect_equal(receiver_statsI[, , "reciprocity"], reciprocity)

# itp
itp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 1, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 0, 0),
  c(0, 0, 0, 1),
  c(0, 0, 0, 0)
)
expect_equal(receiver_statsI[, , "itp"], itp)

# decay memory
stats_decay <- remstats(reh, sender_effects = sender_effects,
  receiver_effects = receiver_effects, memory = "decay", 
  memory_value = 5
)
sender_statsD <- stats_decay$sender_stats
receiver_statsD <- stats_decay$receiver_stats
actors <- attr(reh, "dictionary")$actors

# outdegreeSender
outdegreeSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(f(1, 1), 0, 0, 0),
  c(f(2, 1), f(2, 2), 0, 0),
  c(f(3, 1) + f(3, 3), f(3, 2), 0, 0),
  c(f(4, 1) + f(4, 3), f(4, 2) + f(4, 4), 0, 0),
  c(f(5, 1) + f(5, 3), f(5, 2) + f(5, 4), f(5, 5), 0),
  c(f(6, 1) + f(6, 3), f(6, 2) + f(6, 4), f(6, 5), f(6, 6)),
  c(f(7, 1) + f(7, 3), f(7, 2) + f(7, 4) + f(7, 7), f(7, 5), f(7, 6)),
  c(f(8, 1) + f(8, 3), f(8, 2) + f(8, 4) + f(8, 7) + f(8, 8), f(8, 5), f(8, 6)),
  c(f(9, 1) + f(9, 3), f(9, 2) + f(9, 4) + f(9, 7) + f(9, 8) + f(9, 9), f(9, 5), f(9, 6))
)
expect_equal(sender_statsD[, , "outdegreeSender"], outdegreeSender)

# indegreeSender
indegreeSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, f(1, 1), 0),
  c(f(2, 2), 0, f(2, 1), 0),
  c(f(3, 2), 0, f(3, 1) + f(3, 3), 0),
  c(f(4, 2), 0, f(4, 1) + f(4, 3) + f(4, 4), 0),
  c(f(5, 2), f(5, 5), f(5, 1) + f(5, 3) + f(5, 4), 0),
  c(f(6, 2), f(6, 5), f(6, 1) + f(6, 3) + f(6, 4) + f(6, 6), 0),
  c(f(7, 2) + f(7, 7), f(7, 5), f(7, 1) + f(7, 3) + f(7, 4) + f(7, 6), 0),
  c(f(8, 2) + f(8, 7), f(8, 5), f(8, 1) + f(8, 3) + f(8, 4) + f(8, 6) + f(8, 8), 0),
  c(f(9, 2) + f(9, 7), f(9, 5), f(9, 1) + f(9, 3) + f(9, 4) + f(9, 6) + f(9, 8), f(9, 9))
)
expect_equal(sender_statsD[, , "indegreeSender"], indegreeSender)

# totaldegreeSender
totaldegreeSender <- indegreeSender + outdegreeSender
expect_equal(sender_statsD[, , "totaldegreeSender"], totaldegreeSender)

# outdegreeReceiver
outdegreeReceiver <- outdegreeSender
expect_equal(receiver_statsD[, , "outdegreeReceiver"], outdegreeReceiver)

# indegreeReceiver
indegreeReceiver <- indegreeSender
expect_equal(receiver_statsD[, , "indegreeReceiver"], indegreeReceiver)

# totaldegreeReceiver
totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
expect_equal(receiver_statsD[, , "totaldegreeReceiver"], totaldegreeReceiver)

# inertia
inertia <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, f(2, 1), 0),
  c(f(3, 2), 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(f(6, 2), 0, f(6, 4), 0),
  c(f(7, 2) + f(7, 7), 0, f(7, 4), 0),
  c(f(8, 2) + f(8, 7), 0, f(8, 4) + f(8, 8), 0),
  c(0, 0, f(9, 6), 0)
)
expect_equal(receiver_statsD[, , "inertia"], inertia)

# reciprocity
reciprocity <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, f(2, 2), 0, 0),
  c(0, 0, 0, 0),
  c(f(4, 1) + f(4, 3), f(4, 4), 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, f(6, 5), 0),
  c(0, 0, f(7, 5), 0),
  c(0, 0, f(8, 5), 0),
  c(0, f(9, 9), 0, 0)
)
expect_equal(receiver_statsD[, , "reciprocity"], reciprocity)

# itp
itp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, f(4, 2), 0, 0),
  c(0, 0, 0, 0),
  c(f(6, 5), 0, 0, f(6, 5)),
  c(f(7, 5), 0, 0, f(7, 5)),
  c(f(8, 5), 0, 0, f(8, 5)),
  c(0, 0, f(9, 5), 0)
)
expect_equal(receiver_statsD[, , "itp"], itp)
