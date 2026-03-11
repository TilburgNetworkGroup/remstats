# tie-oriented model 
# --------------------------------------------------------------------
# Small edgelist
edgelist <- data.frame(
  time = 1:10,
  actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
  actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
)

reh <- remify::remify2(edgelist, model = "tie", riskset = "active")
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
  exp(-(time - time_event) * (log(2) / 5)) #* (log(2) / 5)
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

# TODO(actor-model): # actor-oriented model 
# TODO(actor-model): # --------------------------------------------------------------------
# TODO(actor-model): # Small edgelist
# TODO(actor-model): edgelist <- data.frame(
# TODO(actor-model):   time = 1:10,
# TODO(actor-model):   actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
# TODO(actor-model):   actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): reh <- remify::remify(edgelist, model = "actor")
# TODO(actor-model): sender_effects <- ~
# TODO(actor-model):   indegreeSender() + outdegreeSender() + totaldegreeSender() +
# TODO(actor-model):     recencySendSender() + recencyReceiveSender()
# TODO(actor-model): receiver_effects <- ~
# TODO(actor-model):   indegreeReceiver() + outdegreeReceiver() + totaldegreeReceiver() +
# TODO(actor-model):     inertia() + reciprocity() +
# TODO(actor-model):     isp() + itp() + osp() + otp() +
# TODO(actor-model):     isp(unique = TRUE) + itp(unique = TRUE) + 
# TODO(actor-model):     osp(unique = TRUE) + otp(unique = TRUE) +
# TODO(actor-model):     recencyContinue() + recencySendReceiver() + recencyReceiveReceiver() +
# TODO(actor-model):     rrankSend() + rrankReceive()
# TODO(actor-model): stats_window <- remstats(reh, sender_effects = sender_effects,
# TODO(actor-model):   receiver_effects = receiver_effects, memory = "window", memory_value = 5
# TODO(actor-model): )
# TODO(actor-model): sender_statsW <- stats_window$sender_stats
# TODO(actor-model): receiver_statsW <- stats_window$receiver_stats
# TODO(actor-model): actors <- attr(reh, "dictionary")$actors
# TODO(actor-model): 
# TODO(actor-model): # outdegreeSender
# TODO(actor-model): outdegreeSender <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(1, 0, 0, 0),
# TODO(actor-model):   c(1, 1, 0, 0),
# TODO(actor-model):   c(2, 1, 0, 0),
# TODO(actor-model):   c(2, 2, 0, 0),
# TODO(actor-model):   c(2, 2, 1, 0),
# TODO(actor-model):   c(1, 2, 1, 1),
# TODO(actor-model):   c(1, 2, 1, 1),
# TODO(actor-model):   c(0, 3, 1, 1),
# TODO(actor-model):   c(0, 3, 1, 1)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(sender_statsW[, , "outdegreeSender"], outdegreeSender)
# TODO(actor-model): 
# TODO(actor-model): # indegreeSender
# TODO(actor-model): indegreeSender <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, 1, 0),
# TODO(actor-model):   c(1, 0, 1, 0),
# TODO(actor-model):   c(1, 0, 2, 0),
# TODO(actor-model):   c(1, 0, 3, 0),
# TODO(actor-model):   c(1, 1, 3, 0),
# TODO(actor-model):   c(1, 1, 3, 0),
# TODO(actor-model):   c(1, 1, 3, 0),
# TODO(actor-model):   c(1, 1, 3, 0),
# TODO(actor-model):   c(1, 1, 2, 1)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(sender_statsW[, , "indegreeSender"], indegreeSender)
# TODO(actor-model): 
# TODO(actor-model): # totaldegreeSender
# TODO(actor-model): totaldegreeSender <- indegreeSender + outdegreeSender
# TODO(actor-model): expect_equal(sender_statsW[, , "totaldegreeSender"], totaldegreeSender)
# TODO(actor-model): 
# TODO(actor-model): # outdegreeReceiver
# TODO(actor-model): outdegreeReceiver <- outdegreeSender
# TODO(actor-model): expect_equal(receiver_statsW[, , "outdegreeReceiver"], outdegreeReceiver)
# TODO(actor-model): 
# TODO(actor-model): # indegreeReceiver
# TODO(actor-model): indegreeReceiver <- indegreeSender
# TODO(actor-model): expect_equal(receiver_statsW[, , "indegreeReceiver"], indegreeReceiver)
# TODO(actor-model): 
# TODO(actor-model): # totaldegreeReceiver
# TODO(actor-model): totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
# TODO(actor-model): expect_equal(receiver_statsW[, , "totaldegreeReceiver"], totaldegreeReceiver)
# TODO(actor-model): 
# TODO(actor-model): # inertia
# TODO(actor-model): inertia <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 1, 0),
# TODO(actor-model):   c(1, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(1, 0, 1, 0),
# TODO(actor-model):   c(1, 0, 1, 0),
# TODO(actor-model):   c(1, 0, 2, 0),
# TODO(actor-model):   c(0, 0, 1, 0)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(receiver_statsW[, , "inertia"], inertia)
# TODO(actor-model): 
# TODO(actor-model): # reciprocity
# TODO(actor-model): reciprocity <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 1, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(2, 1, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 1, 0),
# TODO(actor-model):   c(0, 0, 1, 0),
# TODO(actor-model):   c(0, 0, 1, 0),
# TODO(actor-model):   c(0, 1, 0, 0)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(receiver_statsW[, , "reciprocity"], reciprocity)
# TODO(actor-model): 
# TODO(actor-model): # itp
# TODO(actor-model): itp <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 1, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(1, 0, 0, 1),
# TODO(actor-model):   c(1, 0, 0, 1),
# TODO(actor-model):   c(0, 0, 0, 1),
# TODO(actor-model):   c(0, 0, 1, 0)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(receiver_statsW[, , "itp"], itp)
# TODO(actor-model): 
# TODO(actor-model): # interval memory
# TODO(actor-model): stats_interval <- remstats(reh, sender_effects = sender_effects,
# TODO(actor-model):   receiver_effects = receiver_effects, memory = "interval", 
# TODO(actor-model):   memory_value = c(2, 5)
# TODO(actor-model): )
# TODO(actor-model): sender_statsI <- stats_interval$sender_stats
# TODO(actor-model): receiver_statsI <- stats_interval$receiver_stats
# TODO(actor-model): actors <- attr(reh, "dictionary")$actors
# TODO(actor-model): 
# TODO(actor-model): # outdegreeSender
# TODO(actor-model): outdegreeSender <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(1, 0, 0, 0),
# TODO(actor-model):   c(1, 1, 0, 0),
# TODO(actor-model):   c(2, 1, 0, 0),
# TODO(actor-model):   c(1, 2, 0, 0),
# TODO(actor-model):   c(1, 1, 1, 0),
# TODO(actor-model):   c(0, 1, 1, 1),
# TODO(actor-model):   c(0, 1, 1, 1)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(sender_statsI[, , "outdegreeSender"], outdegreeSender)
# TODO(actor-model): 
# TODO(actor-model): # indegreeSender
# TODO(actor-model): indegreeSender <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 1, 0),
# TODO(actor-model):   c(1, 0, 1, 0),
# TODO(actor-model):   c(1, 0, 2, 0),
# TODO(actor-model):   c(1, 0, 2, 0),
# TODO(actor-model):   c(0, 1, 2, 0),
# TODO(actor-model):   c(0, 1, 2, 0),
# TODO(actor-model):   c(1, 1, 1, 0)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(sender_statsI[, , "indegreeSender"], indegreeSender)
# TODO(actor-model): 
# TODO(actor-model): # totaldegreeSender
# TODO(actor-model): totaldegreeSender <- indegreeSender + outdegreeSender
# TODO(actor-model): expect_equal(sender_statsI[, , "totaldegreeSender"], totaldegreeSender)
# TODO(actor-model): 
# TODO(actor-model): # outdegreeReceiver
# TODO(actor-model): outdegreeReceiver <- outdegreeSender
# TODO(actor-model): expect_equal(receiver_statsI[, , "outdegreeReceiver"], outdegreeReceiver)
# TODO(actor-model): 
# TODO(actor-model): # indegreeReceiver
# TODO(actor-model): indegreeReceiver <- indegreeSender
# TODO(actor-model): expect_equal(receiver_statsI[, , "indegreeReceiver"], indegreeReceiver)
# TODO(actor-model): 
# TODO(actor-model): # totaldegreeReceiver
# TODO(actor-model): totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
# TODO(actor-model): expect_equal(receiver_statsI[, , "totaldegreeReceiver"], totaldegreeReceiver)
# TODO(actor-model): 
# TODO(actor-model): # inertia
# TODO(actor-model): inertia <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(1, 0, 1, 0),
# TODO(actor-model):   c(0, 0, 1, 0),
# TODO(actor-model):   c(0, 0, 1, 0),
# TODO(actor-model):   c(0, 0, 1, 0)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(receiver_statsI[, , "inertia"], inertia)
# TODO(actor-model): 
# TODO(actor-model): # reciprocity
# TODO(actor-model): reciprocity <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(1, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 1, 0),
# TODO(actor-model):   c(0, 0, 1, 0),
# TODO(actor-model):   c(0, 0, 0, 0)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(receiver_statsI[, , "reciprocity"], reciprocity)
# TODO(actor-model): 
# TODO(actor-model): # itp
# TODO(actor-model): itp <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 1, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(1, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 1),
# TODO(actor-model):   c(0, 0, 0, 0)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(receiver_statsI[, , "itp"], itp)
# TODO(actor-model): 
# TODO(actor-model): # decay memory
# TODO(actor-model): stats_decay <- remstats(reh, sender_effects = sender_effects,
# TODO(actor-model):   receiver_effects = receiver_effects, memory = "decay", 
# TODO(actor-model):   memory_value = 5
# TODO(actor-model): )
# TODO(actor-model): sender_statsD <- stats_decay$sender_stats
# TODO(actor-model): receiver_statsD <- stats_decay$receiver_stats
# TODO(actor-model): actors <- attr(reh, "dictionary")$actors
# TODO(actor-model): 
# TODO(actor-model): # outdegreeSender
# TODO(actor-model): outdegreeSender <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(f(1, 1), 0, 0, 0),
# TODO(actor-model):   c(f(2, 1), f(2, 2), 0, 0),
# TODO(actor-model):   c(f(3, 1) + f(3, 3), f(3, 2), 0, 0),
# TODO(actor-model):   c(f(4, 1) + f(4, 3), f(4, 2) + f(4, 4), 0, 0),
# TODO(actor-model):   c(f(5, 1) + f(5, 3), f(5, 2) + f(5, 4), f(5, 5), 0),
# TODO(actor-model):   c(f(6, 1) + f(6, 3), f(6, 2) + f(6, 4), f(6, 5), f(6, 6)),
# TODO(actor-model):   c(f(7, 1) + f(7, 3), f(7, 2) + f(7, 4) + f(7, 7), f(7, 5), f(7, 6)),
# TODO(actor-model):   c(f(8, 1) + f(8, 3), f(8, 2) + f(8, 4) + f(8, 7) + f(8, 8), f(8, 5), f(8, 6)),
# TODO(actor-model):   c(f(9, 1) + f(9, 3), f(9, 2) + f(9, 4) + f(9, 7) + f(9, 8) + f(9, 9), f(9, 5), f(9, 6))
# TODO(actor-model): )
# TODO(actor-model): expect_equal(sender_statsD[, , "outdegreeSender"], outdegreeSender)
# TODO(actor-model): 
# TODO(actor-model): # indegreeSender
# TODO(actor-model): indegreeSender <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, f(1, 1), 0),
# TODO(actor-model):   c(f(2, 2), 0, f(2, 1), 0),
# TODO(actor-model):   c(f(3, 2), 0, f(3, 1) + f(3, 3), 0),
# TODO(actor-model):   c(f(4, 2), 0, f(4, 1) + f(4, 3) + f(4, 4), 0),
# TODO(actor-model):   c(f(5, 2), f(5, 5), f(5, 1) + f(5, 3) + f(5, 4), 0),
# TODO(actor-model):   c(f(6, 2), f(6, 5), f(6, 1) + f(6, 3) + f(6, 4) + f(6, 6), 0),
# TODO(actor-model):   c(f(7, 2) + f(7, 7), f(7, 5), f(7, 1) + f(7, 3) + f(7, 4) + f(7, 6), 0),
# TODO(actor-model):   c(f(8, 2) + f(8, 7), f(8, 5), f(8, 1) + f(8, 3) + f(8, 4) + f(8, 6) + f(8, 8), 0),
# TODO(actor-model):   c(f(9, 2) + f(9, 7), f(9, 5), f(9, 1) + f(9, 3) + f(9, 4) + f(9, 6) + f(9, 8), f(9, 9))
# TODO(actor-model): )
# TODO(actor-model): expect_equal(sender_statsD[, , "indegreeSender"], indegreeSender)
# TODO(actor-model): 
# TODO(actor-model): # totaldegreeSender
# TODO(actor-model): totaldegreeSender <- indegreeSender + outdegreeSender
# TODO(actor-model): expect_equal(sender_statsD[, , "totaldegreeSender"], totaldegreeSender)
# TODO(actor-model): 
# TODO(actor-model): # outdegreeReceiver
# TODO(actor-model): outdegreeReceiver <- outdegreeSender
# TODO(actor-model): expect_equal(receiver_statsD[, , "outdegreeReceiver"], outdegreeReceiver)
# TODO(actor-model): 
# TODO(actor-model): # indegreeReceiver
# TODO(actor-model): indegreeReceiver <- indegreeSender
# TODO(actor-model): expect_equal(receiver_statsD[, , "indegreeReceiver"], indegreeReceiver)
# TODO(actor-model): 
# TODO(actor-model): # totaldegreeReceiver
# TODO(actor-model): totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
# TODO(actor-model): expect_equal(receiver_statsD[, , "totaldegreeReceiver"], totaldegreeReceiver)
# TODO(actor-model): 
# TODO(actor-model): # inertia
# TODO(actor-model): inertia <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, f(2, 1), 0),
# TODO(actor-model):   c(f(3, 2), 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(f(6, 2), 0, f(6, 4), 0),
# TODO(actor-model):   c(f(7, 2) + f(7, 7), 0, f(7, 4), 0),
# TODO(actor-model):   c(f(8, 2) + f(8, 7), 0, f(8, 4) + f(8, 8), 0),
# TODO(actor-model):   c(0, 0, f(9, 6), 0)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(receiver_statsD[, , "inertia"], inertia)
# TODO(actor-model): 
# TODO(actor-model): # reciprocity
# TODO(actor-model): reciprocity <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, f(2, 2), 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(f(4, 1) + f(4, 3), f(4, 4), 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, f(6, 5), 0),
# TODO(actor-model):   c(0, 0, f(7, 5), 0),
# TODO(actor-model):   c(0, 0, f(8, 5), 0),
# TODO(actor-model):   c(0, f(9, 9), 0, 0)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(receiver_statsD[, , "reciprocity"], reciprocity)
# TODO(actor-model): 
# TODO(actor-model): # itp
# TODO(actor-model): itp <- rbind(
# TODO(actor-model):   matrix(0, ncol = nrow(actors)),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(0, f(4, 2), 0, 0),
# TODO(actor-model):   c(0, 0, 0, 0),
# TODO(actor-model):   c(f(6, 5), 0, 0, f(6, 5)),
# TODO(actor-model):   c(f(7, 5), 0, 0, f(7, 5)),
# TODO(actor-model):   c(f(8, 5), 0, 0, f(8, 5)),
# TODO(actor-model):   c(0, 0, f(9, 5), 0)
# TODO(actor-model): )
# TODO(actor-model): expect_equal(receiver_statsD[, , "itp"], itp)
# TODO(actor-model): 
