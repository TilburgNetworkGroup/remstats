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
stats <- remstats(reh, tie_effects = effects, attr_actors = info)

expect_stdout(print(stats))
expect_stdout(summary(stats))

# Actor model
reh <- remify::remify(edgelist, model = "actor")
sender_effects <- ~ outdegreeSender()
receiver_effects <- ~ receive(variable = "x1"):inertia()
stats <- remstats(reh, sender_effects = sender_effects, 
    receiver_effects = receiver_effects, attr_actors = info)

expect_stdout(print(stats))
expect_stdout(summary(stats))

stats <- remstats(reh, sender_effects = sender_effects, attr_actors = info)
expect_stdout(print(stats))
expect_stdout(summary(stats))

stats <- remstats(reh, receiver_effects = receiver_effects, attr_actors = info)
expect_stdout(print(stats))
expect_stdout(summary(stats))