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
reh <- remify2(edgelist, model = "tie")
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

# TODO(actor-model): actor-oriented model weights test pending aomstats implementation
# reh <- remify2(edgelist, model = "actor")
# sender_effects <- ~
#   indegreeSender() + outdegreeSender() + totaldegreeSender()
# receiver_effects <- ~
#   indegreeReceiver() + outdegreeReceiver() + totaldegreeReceiver() +
#     inertia() + reciprocity() +
#     isp() + itp() + osp() + otp()
# stats <- remstats(reh,
#   sender_effects = sender_effects,
#   receiver_effects = receiver_effects
# )
# sender_stats <- stats$sender_stats
# receiver_stats <- stats$receiver_stats
# actors <- reh$meta$dictionary$actors
# ... (weight value checks for each statistic)
