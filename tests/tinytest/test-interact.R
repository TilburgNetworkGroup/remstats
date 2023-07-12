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

# Tie model
reh <- remify::remify(edgelist, model = "tie")
effects <- ~ send(variable = "x1"):inertia()
stats <- remstats(reh, tie_effects = effects, attr_data = info)

interaction <- stats[, , "send_x1"] * stats[, , "inertia"]
expect_equal(stats[, , "send_x1:inertia"], interaction)

# Actor model
reh <- remify::remify(edgelist, model = "actor")
sender_effects <- ~ send(variable = "x1"):outdegreeSender()
receiver_effects <- ~ receive(variable = "x1"):inertia()
stats <- remstats(reh, sender_effects = sender_effects, 
    receiver_effects = receiver_effects, attr_data = info)
sender_stats <- stats$sender_stats
receiver_stats <- stats$receiver_stats

sender_interaction <- sender_stats[, , "send_x1"] * 
  sender_stats[, , "outdegreeSender"]
expect_equal(sender_stats[, , "send_x1:outdegreeSender"], 
  sender_interaction)

receiver_interaction <- receiver_stats[, , "receive_x1"] * 
  receiver_stats[, , "inertia"]
expect_equal(receiver_stats[, , "receive_x1:inertia"], receiver_interaction)