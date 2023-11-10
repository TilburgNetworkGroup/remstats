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
    psABBA() + psABBY() + psABAB() + psABBY() +
    psABXA() + psABXB() + psABXY() +
    recencyContinue() +
    recencySendSender() + recencySendReceiver() +
    recencyReceiveSender() + recencyReceiveReceiver() +
    rrankSend() + rrankReceive()
output <- capture.output(
  stats <- remstats(reh, tie_effects = effects, display_progress = TRUE)
)
expect_true(all(grepl("Calculating", output[-1])))

# Statistics
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
output <- capture.output(
  stats <- remstats(reh,
    sender_effects = sender_effects,
    receiver_effects = receiver_effects, display_progress = TRUE,
  )
)
expect_true(all(grepl("Computing", output)))
