# Condition 9: Actor-oriented model

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
reh <- remify::remify(edgelist, model = "actor")
sender_effects <- ~ send(variable = "x1") 
receiver_effects <- ~ receive(variable = "x1") + 
  average(variable = "x1") + difference(variable = "x1") + 
  same(variable = "x2") 
stats <- remstats(reh = reh,
  sender_effects = sender_effects,
  receiver_effects = receiver_effects, 
  attr_actors = info
)
sender_stats <- stats$sender_stats
receiver_stats <- stats$receiver_stats
actors <- attr(reh, "dictionary")$actors

# baseline
expect_equal(sender_stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(actors)))

# send
send <- rbind(
  c(10, 20, 30),
  c(10, 20, 30),
  c(100, 200, 300),
  c(100, 200, 300),
  c(100, 200, 300)
)
expect_equal(sender_stats[, , "send_x1"], send)

# receive
receive <- rbind(
  c(10, 20, 30),
  c(10, 20, 30),
  c(100, 200, 300),
  c(100, 200, 300),
  c(100, 200, 300)
)
expect_equal(receiver_stats[, , "receive_x1"], receive)

# average
average <- rbind(
  c(10, 15, 20),
  c(10, 15, 20),
  c(150, 200, 250),
  c(150, 200, 250),
  c(200, 250, 300)
)
expect_equal(receiver_stats[, , "average_x1"], average)

# difference
difference <- rbind(
  c(0, 10, 20),
  c(0, 10, 20),
  c(100, 0, 100),
  c(100, 0, 100),
  c(200, 100, 0)
)
expect_equal(receiver_stats[, , "difference_x1"], difference)

# same 
same <- rbind(
  c(1, 0, 0),
  c(1, 0, 0),
  c(1, 1, 0),
  c(1, 1, 0),
  c(0, 0, 1)
)
expect_equal(receiver_stats[, , "same_x2"], same)

# test standardization
reh <- remify::remify(edgelist, model = "actor")
std_sender_effects <- ~ send(variable = "x1", scaling = "std")
std_receiver_effects <- ~ receive(variable = "x1", scaling = "std") + 
  average(variable = "x1", scaling = "std") + 
  difference(variable = "x1", scaling = "std") 
std_stats <- remstats(reh = reh,
  sender_effects = std_sender_effects,
  receiver_effects = std_receiver_effects, 
  attr_actors = info
)
std_sender_stats <- std_stats$sender_stats
std_receiver_stats <- std_stats$receiver_stats

sapply(2:dim(std_sender_stats)[3], function(p) {
  stat_name <- dimnames(std_sender_stats)[[3]][p]
  scaled_original <- t(apply(sender_stats[, , stat_name], 1, scale))
  scaled_original[which(apply(sender_stats[, , stat_name], 1, sd) == 0), ] <-
    rep(0, ncol(sender_stats))
  expect_equal(std_sender_stats[, , stat_name], scaled_original)
})

sapply(2:dim(std_receiver_stats)[3], function(p) {
  stat_name <- dimnames(std_receiver_stats)[[3]][p]
  scaled_original <- t(sapply(1:nrow(edgelist), function(m) {
    stat_row <- receiver_stats[m,, stat_name]
    row_mean <- mean(stat_row[-edgelist[m,2]])
    row_sd <- sd(stat_row[-edgelist[m,2]])
    if(row_sd == 0) {
      stat_row <- rep(0, ncol(receiver_stats))
    }  else {
      stat_row <- ((stat_row - row_mean) / row_sd)
      stat_row[edgelist[m,2]] <- 0
    }    
    stat_row
  }))
  expect_equal(std_receiver_stats[, , stat_name], scaled_original)
})

# Test method -------------------------------------------------------------
# Small change to the times in the edgelist
edgelist <- data.frame(
  time = c(1, 2, 3, 3, 4),
  actor1 = c(1, 1, 2, 2, 3),
  actor2 = c(2, 3, 1, 3, 2)
)

reh <- remify::remify(edgelist, model = "actor", riskset = "active")

# Method = "pt"
# Selection of effects that have unique underlying cpp functions
sender_effects <- ~ send(variable = "x1") 
receiver_effects <- ~ receive(variable = "x1") + 
  average(variable = "x1") 
pt_stats <- remstats(reh = reh,
  sender_effects = sender_effects,
  receiver_effects = receiver_effects, 
  attr_actors = info,
  method = "pt"
)
sender_stats <- pt_stats$sender_stats
receiver_stats <- pt_stats$receiver_stats
actors <- attr(reh, "dictionary")$actors

# send
send <- rbind(
  c(10, 20, 30),
  c(10, 20, 30),
  c(100, 200, 300),
  c(100, 200, 300)
)
expect_equal(sender_stats[, , "send_x1"], send)

# receive
receive <- rbind(
  c(10, 20, 30),
  c(10, 20, 30),
  c(100, 200, 300),
  c(100, 200, 300),
  c(100, 200, 300)
)
expect_equal(receiver_stats[, , "receive_x1"], receive)

# average
average <- rbind(
  c(10, 15, 20),
  c(10, 15, 20),
  c(150, 200, 250),
  c(150, 200, 250),
  c(200, 250, 300)
)
expect_equal(receiver_stats[, , "average_x1"], average)

# Method = "pe"

# Selection of effects that have unique underlying cpp functions
sender_effects <- ~ send(variable = "x1") 
receiver_effects <- ~ receive(variable = "x1") + 
  average(variable = "x1") 
pe_stats <- remstats(reh = reh,
  sender_effects = sender_effects,
  receiver_effects = receiver_effects, 
  attr_actors = info,
  method = "pe"
)
sender_stats <- pe_stats$sender_stats
receiver_stats <- pe_stats$receiver_stats
actors <- attr(reh, "dictionary")$actors

# send
send <- rbind(
  c(10, 20, 30),
  c(10, 20, 30),
  c(100, 200, 300),
	c(100, 200, 300),
  c(100, 200, 300)
)
expect_equal(sender_stats[, , "send_x1"], send)

# receive
receive <- rbind(
  c(10, 20, 30),
  c(10, 20, 30),
  c(100, 200, 300),
  c(100, 200, 300),
  c(100, 200, 300)
)
expect_equal(receiver_stats[, , "receive_x1"], receive)

# average
average <- rbind(
  c(10, 15, 20),
  c(10, 15, 20),
  c(150, 200, 250),
  c(150, 200, 250),
  c(200, 250, 300)
)
expect_equal(receiver_stats[, , "average_x1"], average)