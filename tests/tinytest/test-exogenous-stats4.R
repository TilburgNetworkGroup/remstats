# Condition 4: Undirected events with types, tie-oriented model with active riskset

# Small edgelist
edgelist <- data.frame(
  time = 1:5,
  actor1 = c(1, 1, 2, 2, 3),
  actor2 = c(2, 3, 1, 3, 2)
)

event_types <- c(1, 1, 2, 2, 1)

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
X <<- matrix(1:9, nrow = 3)
X <<- Matrix::forceSymmetric(X, "L")
X <<- as.matrix(X)
diag(X) <- 0

# Event info
setting <<- c("a", "b", "b", "a", "a")

# UserStat
Y <<- matrix(1:25, nrow = 5, ncol = 5)

# Statistics
edgelist$type <- event_types
reh <- remify::remify(edgelist, model = "tie", directed = FALSE, riskset = "active")
effects <- ~ average(variable = "x1") + difference(variable = "x1") + 
  maximum(variable = "x1") + minimum(variable = "x1") +
  same(variable = "x2") + tie(x = X, variableName = "X") +
  event(x = setting, variableName = "setting") +
  userStat(x = Y, variableName = "Y")
stats <- remstats(reh, tie_effects = effects, attr_data = info)
riskset <- attr(stats, "riskset")

# Baseline
expect_equal(stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# average
average <- rbind(
  c(15, 20, 25, 15, 25),
  c(15, 20, 25, 15, 25),
  c(150, 200, 250, 150, 250),
  c(150, 200, 250, 150, 250),
  c(150, 200, 250, 150, 250)
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
  c(20, 30, 30, 20, 30),
  c(20, 30, 30, 20, 30),
  c(200, 300, 300, 200, 300),
  c(200, 300, 300, 200, 300),
  c(200, 300, 300, 200, 300)
)
expect_equal(stats[, , "maximum_x1"], maximum)

# minimum
minimum <- rbind(
  c(10, 10, 20, 10, 20),
  c(10, 10, 20, 10, 20),
  c(100, 100, 200, 100, 200),
  c(100, 100, 200, 100, 200),
  c(100, 100, 200, 100, 200)
)
expect_equal(stats[, , "minimum_x1"], minimum)

# same
same <- rbind(
  c(0, 0, 1, 0, 1),
  c(0, 0, 1, 0, 1),
  c(1, 0, 0, 1, 0),
  c(1, 0, 0, 1, 0),
  c(1, 0, 0, 1, 0)
)
expect_equal(stats[, , "same_x2"], same)

# tie
tie <- rbind(
  c(2, 3, 6, 2, 6),
  c(2, 3, 6, 2, 6),
  c(2, 3, 6, 2, 6),
  c(2, 3, 6, 2, 6),
  c(2, 3, 6, 2, 6)
)
expect_equal(stats[, , "tie_X"], tie)

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

# test standardization
std_effects <- ~
  average(variable = "x1", scaling = "std") + 
  difference(variable = "x1", scaling = "std") + 
  maximum(variable = "x1", scaling = "std") + 
  minimum(variable = "x1", scaling = "std") + 
  tie(x = X, variableName = "X", scaling = "std")
std_stats <- remstats(reh, tie_effects = std_effects, attr_data = info)

sapply(2:dim(std_stats)[3], function(p) {
  stat_name <- dimnames(std_stats)[[3]][p]
  scaled_original <- t(apply(stats[,,stat_name], 1, scale))
  scaled_original[which(apply(stats[,,stat_name], 1, sd) == 0),] <- 
    rep(0, ncol(stats))
  expect_equal(std_stats[,,stat_name], scaled_original)
})