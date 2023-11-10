# Condition 1: Directed events, tie-oriented model with active risk set

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

# Tie info
X_wide <<- matrix(1:9, 3, 3)
diag(X_wide) <- 0

X_long <<- data.frame(
	actor1 = c(1, 1, 2, 2, 3, 3, 1),
	actor2 = c(2, 3, 1, 3, 1, 2, 2),
  time = c(rep(0, 6), 2),
	X_long = c(4, 7, 2, 8, 3, 6, 40)
)

# Event info
setting <<- c("a", "b", "b", "a", "a")

# UserStat
Y <<- matrix(1:25, nrow = 5, ncol = 5)

# Statistics
reh <- remify::remify(edgelist, model = "tie", riskset = "active")
effects <- ~ send(variable = "x1") + receive(variable = "x1") + 
  average(variable = "x1") + difference(variable = "x1") +
  maximum(variable = "x1") + minimum(variable = "x1") +
  same(variable = "x2") + 
  tie(variable = "X_wide", attr_dyads = X_wide) +
	tie(variable = "X_long", attr_dyads = X_long) +
  event(x = setting, variableName = "setting") +
  userStat(x = Y, variableName = "Y")
stats <- remstats(reh, tie_effects = effects, attr_actors = info)
riskset <- attr(stats, "riskset")

# Baseline
expect_equal(stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# send
send <- rbind(
  c(10, 10, 20, 20, 30),
  c(10, 10, 20, 20, 30),
  c(100, 100, 200, 200, 300),
  c(100, 100, 200, 200, 300),
  c(100, 100, 200, 200, 300)
)
expect_equal(stats[, , "send_x1"], send)

# receive
receive <- rbind(
  c(20, 30, 10, 30, 20),
  c(20, 30, 10, 30, 20),
  c(200, 300, 100, 300, 200),
  c(200, 300, 100, 300, 200),
  c(200, 300, 100, 300, 200)
)
expect_equal(stats[, , "receive_x1"], receive)

# average
average <- rbind(
  c(15, 20, 15, 25, 25),
  c(15, 20, 15, 25, 25),
  c(150, 200, 150, 250, 250),
  c(150, 200, 150, 250, 250),
  c(150, 200, 150, 250, 250)
)
expect_equal(stats[, , "average_x1"], average)

# difference
difference <- rbind(
  c(10, 20, 10, 10, 10),
  c(10, 20, 10, 10, 10),
  c(100, 200, 100, 100, 100),
  c(100, 200, 100, 100, 100),
  c(100, 200, 100, 100, 100)
)
expect_equal(stats[, , "difference_x1"], difference)

# maximum 
maximum <- rbind(
  c(20, 30, 20, 30, 30),
  c(20, 30, 20, 30, 30),
  c(200, 300, 200, 300, 300),
  c(200, 300, 200, 300, 300),
  c(200, 300, 200, 300, 300)
)
expect_equal(stats[, , "maximum_x1"], maximum)

# minimum
minimum <- rbind(
  c(10, 10, 10, 20, 20),
  c(10, 10, 10, 20, 20),
  c(100, 100, 100, 200, 200),
  c(100, 100, 100, 200, 200),
  c(100, 100, 100, 200, 200)
)
expect_equal(stats[, , "minimum_x1"], minimum)

# same
same <- rbind(
  c(0, 0, 0, 1, 1),
  c(0, 0, 0, 1, 1),
  c(1, 0, 1, 0, 0),
  c(1, 0, 1, 0, 0),
  c(1, 0, 1, 0, 0)
)
expect_equal(stats[, , "same_x2"], same)

# tie
tie <- rbind(
  c(4, 7, 2, 8, 6),
  c(4, 7, 2, 8, 6),
  c(4, 7, 2, 8, 6),
  c(4, 7, 2, 8, 6),
  c(4, 7, 2, 8, 6)
)
expect_equal(stats[, , "tie_X_wide"], tie)

tie_long <- rbind(
	c(4, 7, 2, 8, 6),
	c(40, 7, 2, 8, 6),
	c(40, 7, 2, 8, 6),
	c(40, 7, 2, 8, 6),
	c(40, 7, 2, 8, 6)
)
expect_equal(stats[, , "tie_X_long"], tie_long)

# event
event <- rbind(
  rep(0, 5),
  rep(1, 5),
  rep(1, 5),
  rep(0, 5),
  rep(0, 5)
)
expect_equal(stats[, , "event_setting"], event)

# userStat
expect_equal(stats[, , "userStat_Y"], Y)

# test attr_actors in variable functions
info3 <<- info[, 1:3]
info2 <<- info[, c(1, 2, 4)]

effects2 <- ~ send(variable = "x1", attr_actors = info3) + 
receive(variable = "x1", attr_actors = info3) + 
  average(variable = "x1", attr_actors = info3) + 
  difference(variable = "x1", attr_actors = info3) + 
  maximum(variable = "x1", attr_actors = info3) + 
  minimum(variable = "x1", attr_actors = info3) +
  same(variable = "x2", attr_actors = info2) 
stats2 <- remstats(reh, tie_effects = effects2)

expect_equal(stats2[, , "send_x1"], send)
expect_equal(stats2[, , "receive_x1"], receive)
expect_equal(stats2[, , "average_x1"], average)
expect_equal(stats2[, , "difference_x1"], difference)
expect_equal(stats2[, , "maximum_x1"], maximum)
expect_equal(stats2[, , "minimum_x1"], minimum)
expect_equal(stats2[, , "same_x2"], same)

# test difference absolute = FALSE
effects3 <- ~ difference(variable = "x1", absolute = FALSE)
stats3 <- remstats(reh, tie_effects = effects3, attr_actors = info)

difference <- rbind(
  c(-10, -20, 10, -10, 10),
  c(-10, -20, 10, -10, 10),
  c(-100, -200, 100, -100, 100),
  c(-100, -200, 100, -100, 100),
  c(-100, -200, 100, -100, 100)
)
expect_equal(stats3[, , "difference_x1"], difference)

# test attr_dyads in remstats
stats4 <- remstats(reh, tie_effects = ~ tie(variable = "X_wide"), 
  attr_dyads = X_wide)
expect_equal(stats4[, , "tie_X_wide"], tie)

stats5 <- remstats(reh, tie_effects = ~ tie(variable = "X_long"), 
  attr_dyads = X_long)
expect_equal(stats5[, , "tie_X_long"], tie_long)

# test standardization
std_effects <- ~
  send(variable = "x1", scaling = "std") + 
  receive(variable = "x1", scaling = "std") + 
  average(variable = "x1", scaling = "std") + 
  difference(variable = "x1", scaling = "std") + 
  maximum(variable = "x1", scaling = "std") + 
  minimum(variable = "x1", scaling = "std") +
  tie(variable = "X_wide", attr_dyads = X_wide, scaling = "std") +
  tie(variable = "X_long", attr_dyads = X_long, scaling = "std")
std_stats <- remstats(reh, tie_effects = std_effects, attr_actors = info)

sapply(2:dim(std_stats)[3], function(p) {
  stat_name <- dimnames(std_stats)[[3]][p]
  scaled_original <- t(apply(stats[,,stat_name], 1, scale))
  scaled_original[which(apply(stats[,,stat_name], 1, sd) == 0),] <- 
    rep(0, ncol(stats))
  expect_equal(std_stats[,,stat_name], scaled_original)
})
