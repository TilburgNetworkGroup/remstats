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

# Statistics
reh <- remify::remify(edgelist, model = "tie", riskset = "active")
effects <-	~ send(variable = "x1") + receive(variable = "x1") + 
  average(variable = "x1") + difference(variable = "x1") +
  maximum(variable = "x1") + minimum(variable = "x1") +
  same(variable = "x2") 

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

# test standardization
std_effects <- ~
  send(variable = "x1", scaling = "std") + 
  receive(variable = "x1", scaling = "std") + 
  average(variable = "x1", scaling = "std") + 
  difference(variable = "x1", scaling = "std") + 
  maximum(variable = "x1", scaling = "std") + 
  minimum(variable = "x1", scaling = "std") 
std_stats <- remstats(reh, tie_effects = std_effects, attr_actors = info)

sapply(2:dim(std_stats)[3], function(p) {
  stat_name <- dimnames(std_stats)[[3]][p]
  scaled_original <- t(apply(stats[,,stat_name], 1, scale))
  scaled_original[which(apply(stats[,,stat_name], 1, sd) == 0),] <- 
    rep(0, ncol(stats))
  expect_equal(std_stats[,,stat_name], scaled_original)
})

# Test method -------------------------------------------------------------
# Small change to the times in the edgelist
edgelist <- data.frame(
  time = c(1, 2, 3, 3, 4),
  actor1 = c(1, 1, 2, 2, 3),
  actor2 = c(2, 3, 1, 3, 2)
)

reh <- remify::remify(edgelist, model = "tie", riskset = "active")

# Method = "pt"
# Selection of effects that have unique underlying cpp functions
effects <- ~ send(variable = "x1") +
  average(variable = "x1") 

pt_stats <- remstats(reh, tie_effects = effects, attr_actors = info, 
  method = "pt")
riskset <- attr(pt_stats, "riskset")

# send
send <- rbind(
  c(10, 10, 20, 20, 30),
  c(10, 10, 20, 20, 30),
  c(100, 100, 200, 200, 300),
  c(100, 100, 200, 200, 300)
)
expect_equal(pt_stats[, , "send_x1"], send)

# average
average <- rbind(
  c(15, 20, 15, 25, 25),
  c(15, 20, 15, 25, 25),
  c(150, 200, 150, 250, 250),
  c(150, 200, 150, 250, 250)
)
expect_equal(pt_stats[, , "average_x1"], average)

# Method = "pe"
# Selection of effects that have unique underlying cpp functions
effects <- ~ send(variable = "x1") +
  average(variable = "x1")  

pe_stats <- remstats(reh, tie_effects = effects, attr_actors = info, 
  method = "pe")
riskset <- attr(pt_stats, "riskset")

# send
send <- rbind(
  c(10, 10, 20, 20, 30),
  c(10, 10, 20, 20, 30),
  c(100, 100, 200, 200, 300),
  c(100, 100, 200, 200, 300),
  c(100, 100, 200, 200, 300)
)
expect_equal(pe_stats[, , "send_x1"], send)

# average
average <- rbind(
  c(15, 20, 15, 25, 25),
  c(15, 20, 15, 25, 25),
  c(150, 200, 150, 250, 250),
  c(150, 200, 150, 250, 250),
  c(150, 200, 150, 250, 250)
)
expect_equal(pe_stats[, , "average_x1"], average)