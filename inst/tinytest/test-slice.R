# Test the 'start' and 'stop' arguments

# Small edgelist
edgelist <- data.frame(
  time = 1:5,
  actor1 = c(1, 1, 2, 2, 3),
  actor2 = c(2, 3, 1, 3, 2)
)

# Actor info
info <- data.frame(
  name = 1:3,
  time = rep(0, 3),
  x1 = c(10, 20, 30),
  x2 = c(0, 1, 1)
)

info2 <- data.frame(
  name = 1:3,
  time = rep(3, 3),
  x1 = c(100, 200, 300),
  x2 = c(1, 1, 0)
)

info <- rbind(info, info2)

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
    psABBA() + psABBY() + psABAB() + psABBY() +
    psABXA() + psABXB() + psABXY() +
    recencyContinue() +
    recencySendSender() + recencySendReceiver() +
    recencyReceiveSender() + recencyReceiveReceiver() +
    rrankSend() + rrankReceive() +
    send(variable = "x1") + receive(variable = "x1") + 
    average(variable = "x1") + difference(variable = "x1") + 
    maximum(variable = "x1") + minimum(variable = "x1") +
    same(variable = "x2") 
stats <- remstats(reh, tie_effects = effects, attr_actors = info)
slice_stats <- remstats(reh, tie_effects = effects, attr_actors = info, 
  start = 2, stop = 4)

# Test
expect_equal(stats[2:4,,], slice_stats[1:nrow(slice_stats),,])

# Statistics
reh <- remify::remify(edgelist, model = "actor")
sender_effects <- ~ send(variable = "x1") +
  indegreeSender() + outdegreeSender() + totaldegreeSender() +
  recencySendSender() + recencyReceiveSender()
receiver_effects <- ~ receive(variable = "x1") + 
  average(variable = "x1") + difference(variable = "x1") + 
  same(variable = "x2") + 
  indegreeReceiver() + outdegreeReceiver() + totaldegreeReceiver() +
  inertia() + reciprocity() +
  isp() + itp() + osp() + otp() +
  isp(unique = TRUE) + itp(unique = TRUE) + 
  osp(unique = TRUE) + otp(unique = TRUE) +
  recencyContinue() + recencySendReceiver() + recencyReceiveReceiver() +
  rrankSend() + rrankReceive()
stats <- remstats(reh = reh,
  sender_effects = sender_effects,
  receiver_effects = receiver_effects, 
  attr_actors = info
)
sender_stats <- stats$sender_stats
receiver_stats <- stats$receiver_stats
slice_stats <- remstats(reh = reh,
  sender_effects = sender_effects,
  receiver_effects = receiver_effects, 
  attr_actors = info, start = 2, stop = 4
)
slice_sender_stats <- slice_stats$sender_stats
slice_receiver_stats <- slice_stats$receiver_stats

# Test
expect_equal(sender_stats[2:4,,], 
  slice_sender_stats[1:nrow(slice_sender_stats),,])
expect_equal(receiver_stats[2:4,,], 
  slice_receiver_stats[1:nrow(slice_receiver_stats),,])