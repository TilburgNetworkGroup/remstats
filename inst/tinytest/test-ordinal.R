# Create example remstats objects
edgelist <<- data.frame(
  time = c(1:4),
  actor1 = sample(1:3, 4, replace = T),
  actor2 = sample(4:6, 4, replace = T)
)

reh <- remify::remify(edgelist, model = "tie", actors = 1:6, ordinal = TRUE)
expect_silent(remstats(reh = reh, tie_effects = ~ inertia()))
