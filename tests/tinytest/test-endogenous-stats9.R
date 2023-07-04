# Condition 9: Actor-oriented model

# Small edgelist
edgelist <- data.frame(
  time = 1:5,
  actor1 = c(1, 1, 2, 2, 3),
  actor2 = c(2, 3, 1, 3, 2)
)

# Statistics
reh <- remify::remify(edgelist, model = "actor")
sender_effects <- ~
  indegreeSender() + outdegreeSender() + totaldegreeSender() +
    recencySendSender() + recencyReceiveSender()
receiver_effects <- ~
  indegreeReceiver() + outdegreeReceiver() + totaldegreeReceiver() +
    inertia() + reciprocity() +
    isp() + itp() + osp() + otp() +
    recencyContinue() + recencySendReceiver() + recencyReceiveReceiver() +
    rrankSend() + rrankReceive()
stats <- remstats(reh,
  sender_effects = sender_effects,
  receiver_effects = receiver_effects
)
sender_stats <- stats$sender_stats
receiver_stats <- stats$receiver_stats
actors <- attr(reh, "dictionary")$actors

# baseline
expect_equal(sender_stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(actors)))

# outdegreeSender
outdegreeSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(1, 0, 0),
  c(2, 0, 0),
  c(2, 1, 0),
  c(2, 2, 0)
)
expect_equal(sender_stats[, , "outdegreeSender"], outdegreeSender)

# indegreeSender
indegreeSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 1, 0),
  c(0, 1, 1),
  c(1, 1, 1),
  c(1, 1, 2)
)
expect_equal(sender_stats[, , "indegreeSender"], indegreeSender)

# totaldegreeSender
totaldegreeSender <- indegreeSender + outdegreeSender
expect_equal(sender_stats[, , "totaldegreeSender"], totaldegreeSender)

# recencySendSender
recencySendSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(1 / 2, 0, 0),
  c(1 / 2, 0, 0),
  c(1 / 3, 1 / 2, 0),
  c(1 / 4, 1 / 2, 0)
)
expect_equal(sender_stats[, , "recencySendSender"], recencySendSender)

# recencyReceiveSender
recencyReceiveSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 1 / 2, 0),
  c(0, 1 / 3, 1 / 2),
  c(1 / 2, 1 / 4, 1 / 3),
  c(1 / 3, 1 / 5, 1 / 2)
)
expect_equal(sender_stats[, , "recencyReceiveSender"], recencyReceiveSender)

# outdegreeReceiver
outdegreeReceiver <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(1, 0, 0),
  c(2, 0, 0),
  c(2, 1, 0),
  c(2, 2, 0)
)
expect_equal(receiver_stats[, , "outdegreeReceiver"], outdegreeReceiver)

# indegreeReceiver
indegreeReceiver <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 1, 0),
  c(0, 1, 1),
  c(1, 1, 1),
  c(1, 1, 2)
)
expect_equal(receiver_stats[, , "indegreeReceiver"], indegreeReceiver)

# totaldegreeReceiver
totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
expect_equal(receiver_stats[, , "totaldegreeReceiver"], totaldegreeReceiver)

# itp
itp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0),
  c(0, 0, 0),
  c(0, 0, 0),
  c(1, 1, 0)
)
expect_equal(receiver_stats[, , "itp"], itp)

# otp
otp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0),
  c(0, 0, 0),
  c(0, 0, 1),
  c(0, 0, 0)
)
expect_equal(receiver_stats[, , "otp"], otp)

# isp
isp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0),
  c(0, 0, 1),
  c(0, 0, 1),
  c(1, 1, 0)
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

# recencyContinue
recencyContinue <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 1 / 2, 0),
  c(0, 0, 0),
  c(1 / 2, 0, 0),
  c(0, 0, 0)
)
expect_equal(receiver_stats[, , "recencyContinue"], recencyContinue)

# recencySendReceiver
recencySendReceiver <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(1 / 2, 0, 0),
  c(1 / 2, 0, 0),
  c(1 / 3, 1 / 2, 0),
  c(1 / 4, 1 / 2, 0)
)
expect_equal(receiver_stats[, , "recencySendReceiver"], recencySendReceiver)

# recencyReceiveReceiver
recencyReceiveReceiver <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 1 / 2, 0),
  c(0, 1 / 3, 1 / 2),
  c(1 / 2, 1 / 4, 1 / 3),
  c(1 / 3, 1 / 5, 1 / 2)
)
expect_equal(receiver_stats[, , "recencyReceiveReceiver"], recencyReceiveReceiver)

# rrankSend
rrankSend <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 1, 0),
  c(0, 0, 0),
  c(1, 0, 0),
  c(0, 0, 0)
)
expect_equal(receiver_stats[, , "rrankSend"], rrankSend)

# rrankReceive
rrankReceive <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0),
  c(1, 0, 0),
  c(1, 0, 0),
  c(1 / 2, 1, 0)
)
expect_equal(receiver_stats[, , "rrankReceive"], rrankReceive)
