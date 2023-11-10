# Condition 5: Actor-oriented model

# Small edgelist
edgelist <- data.frame(
  time = 1:10,
  actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
  actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
)

# Statistics
reh <- remify::remify(edgelist, model = "actor")
sender_effects <- ~
  indegreeSender() + outdegreeSender() + totaldegreeSender() +
    recencySendSender() + recencyReceiveSender() + psABA() + psABB() + psABX()
receiver_effects <- ~
  indegreeReceiver() + outdegreeReceiver() + totaldegreeReceiver() +
    inertia() + reciprocity() +
    isp() + itp() + osp() + otp() +
    isp(unique = TRUE) + itp(unique = TRUE) + 
    osp(unique = TRUE) + otp(unique = TRUE) +
    recencyContinue() + recencySendReceiver() + recencyReceiveReceiver() +
    rrankSend() + rrankReceive() + psABAB() + psABBA() + psABXA() + psABXB() +
    psABAY() + psABBY() + psABXY()
stats <- remstats(reh,
  sender_effects = sender_effects,
  receiver_effects = receiver_effects
)
sender_stats <- stats$sender_stats
receiver_stats <- stats$receiver_stats
actors <- attr(reh, "dictionary")$actors

# baseline
expect_equal(sender_stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(actors)))

# outdegreeSender
outdegreeSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(1, 0, 0, 0),
  c(1, 1, 0, 0),
  c(2, 1, 0, 0),
  c(2, 2, 0, 0),
  c(2, 2, 1, 0),
  c(2, 2, 1, 1),
  c(2, 3, 1, 1),
  c(2, 4, 1, 1),
  c(2, 5, 1, 1)
)
expect_equal(sender_stats[, , "outdegreeSender"], outdegreeSender)

# indegreeSender
indegreeSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 1, 0),
  c(1, 0, 1, 0),
  c(1, 0, 2, 0),
  c(1, 0, 3, 0),
  c(1, 1, 3, 0),
  c(1, 1, 4, 0),
  c(2, 1, 4, 0),
  c(2, 1, 5, 0),
  c(2, 1, 5, 1)
)
expect_equal(sender_stats[, , "indegreeSender"], indegreeSender)

# totaldegreeSender
totaldegreeSender <- indegreeSender + outdegreeSender
expect_equal(sender_stats[, , "totaldegreeSender"], totaldegreeSender)

# recencySendSender
recencySendSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(1/2, 0.0, 0.0, 0.0),
  c(1/3, 1/2, 0.0, 0.0),
  c(1/2, 1/3, 0.0, 0.0),
  c(1/3, 1/2, 0.0, 0.0),
  c(1/4, 1/3, 1/2, 0.0),
  c(1/5, 1/4, 1/3, 1/2),
  c(1/6, 1/2, 1/4, 1/3),
  c(1/7, 1/2, 1/5, 1/4),
  c(1/8, 1/2, 1/6, 1/5)
)
expect_equal(sender_stats[, , "recencySendSender"], recencySendSender)

# recencyReceiveSender
recencyReceiveSender <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0.0, 0.0, 1/2, 0.0),
  c(1/2, 0.0, 1/3, 0.0),
  c(1/3, 0.0, 1/2, 0.0),
  c(1/4, 0.0, 1/2, 0.0),
  c(1/5, 1/2, 1/3, 0.0),
  c(1/6, 1/3, 1/2, 0.0),
  c(1/2, 1/4, 1/3, 0.0),
  c(1/3, 1/5, 1/2, 0.0),
  c(1/4, 1/6, 1/3, 1/2)
)
expect_equal(sender_stats[, , "recencyReceiveSender"], recencyReceiveSender)

# psABA
psABA <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(1, 0, 0, 0),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0),
  c(0, 1, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(0, 1, 0, 0),
  c(0, 1, 0, 0)
)
expect_equal(sender_stats[, , "psABA"], psABA)

# psABB
psABB <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 1, 0),
  c(1, 0, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 1, 0, 0),
  c(0, 0, 1, 0),
  c(1, 0, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 0, 1)
)
expect_equal(sender_stats[, , "psABB"], psABB)

# psABX
psABX <- rbind(
  matrix(1, ncol = nrow(actors)),
  c(0, 1, 0, 1),
  c(0, 0, 1, 1),
  c(0, 1, 0, 1),
  c(1, 0, 0, 1),
  c(1, 0, 0, 1),
  c(1, 1, 0, 0),
  c(0, 0, 1, 1),
  c(1, 0, 0, 1),
  c(1, 0, 1, 0)  
)
expect_equal(sender_stats[, , "psABX"], psABX)

# outdegreeReceiver
outdegreeReceiver <- outdegreeSender
expect_equal(receiver_stats[, , "outdegreeReceiver"], outdegreeReceiver)

# indegreeReceiver
indegreeReceiver <- indegreeSender
expect_equal(receiver_stats[, , "indegreeReceiver"], indegreeReceiver)

# totaldegreeReceiver
totaldegreeReceiver <- indegreeReceiver + outdegreeReceiver
expect_equal(receiver_stats[, , "totaldegreeReceiver"], totaldegreeReceiver)

# inertia
inertia <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(1, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 1, 0),
  c(2, 0, 1, 0),
  c(2, 0, 2, 0),
  c(0, 0, 1, 0)
)
expect_equal(receiver_stats[, , "inertia"], inertia)

# reciprocity
reciprocity <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 1, 0, 0),
  c(0, 0, 0, 0),
  c(2, 1, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 1, 0, 0)
)
expect_equal(receiver_stats[, , "reciprocity"], reciprocity)

# itp
itp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 1, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 0, 1),
  c(1, 0, 0, 1),
  c(1, 0, 0, 1),
  c(0, 0, 1, 0)
)
expect_equal(receiver_stats[, , "itp"], itp)

# itp.unique
itp.unique <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 1, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 0, 1),
  c(1, 0, 0, 1),
  c(1, 0, 0, 1),
  c(0, 0, 1, 0)
)
expect_equal(receiver_stats[, , "itp.unique"], itp.unique)

# otp
otp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 2, 0),
  c(0, 0, 2, 0),
  c(0, 1, 0, 0)
)
expect_equal(receiver_stats[, , "otp"], otp)

# otp.unique
otp.unique <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 1, 0, 0)
)
expect_equal(receiver_stats[, , "otp.unique"], otp.unique)

# isp
isp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 1, 0)
)
expect_equal(receiver_stats[, , "isp"], isp)

# isp.unique
isp.unique <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 1, 0)
)
expect_equal(receiver_stats[, , "isp.unique"], isp.unique)

# osp
osp <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 0, 1),
  c(1, 0, 0, 1),
  c(2, 0, 0, 1),
  c(1, 1, 0, 0)
)
expect_equal(receiver_stats[, , "osp"], osp)

# osp.unique
osp.unique <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1, 0, 0, 1),
  c(1, 0, 0, 1),
  c(1, 0, 0, 1),
  c(1, 1, 0, 0)
)
expect_equal(receiver_stats[, , "osp.unique"], osp.unique)

# recencyContinue
recencyContinue <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0.0, 0.0, 0.0, 0.0),
  c(0.0, 0.0, 1/3, 0.0),
  c(1/3, 0.0, 0.0, 0.0),
  c(0.0, 0.0, 0.0, 0.0),
  c(0.0, 0.0, 0.0, 0.0),
  c(1/6, 0.0, 1/4, 0.0),
  c(1/2, 0.0, 1/5, 0.0),
  c(1/3, 0.0, 1/2, 0.0),
  c(0.0, 0.0, 1/5, 0.0)
)
expect_equal(receiver_stats[, , "recencyContinue"], recencyContinue)

# recencySendReceiver
recencySendReceiver <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(1/2, 0.0, 0.0, 0.0),
  c(1/3, 1/2, 0.0, 0.0),
  c(1/2, 1/3, 0.0, 0.0),
  c(1/3, 1/2, 0.0, 0.0),
  c(1/4, 1/3, 1/2, 0.0),
  c(1/5, 1/4, 1/3, 1/2),
  c(1/6, 1/2, 1/4, 1/3),
  c(1/7, 1/2, 1/5, 1/4),
  c(1/8, 1/2, 1/6, 1/5)
)
expect_equal(receiver_stats[, , "recencySendReceiver"], recencySendReceiver)

# recencyReceiveReceiver
recencyReceiveReceiver <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0.0, 0.0, 1/2, 0.0),
  c(1/2, 0.0, 1/3, 0.0),
  c(1/3, 0.0, 1/2, 0.0),
  c(1/4, 0.0, 1/2, 0.0),
  c(1/5, 1/2, 1/3, 0.0),
  c(1/6, 1/3, 1/2, 0.0),
  c(1/2, 1/4, 1/3, 0.0),
  c(1/3, 1/5, 1/2, 0.0),
  c(1/4, 1/6, 1/3, 1/2)
)
expect_equal(receiver_stats[, , "recencyReceiveReceiver"], recencyReceiveReceiver)

# rrankSend
rrankSend <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(1, 0, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0),
  c(1/2, 0, 1, 0),
  c(1, 0, 1/2, 0),
  c(1/2, 0, 1, 0),
  c(0, 0, 1, 0)
)
expect_equal(receiver_stats[, , "rrankSend"], rrankSend)

# rrankReceive
rrankReceive <- rbind(
  matrix(0, ncol = nrow(actors)),
  c(0, 0, 0, 0),
  c(0, 1, 0, 0),
  c(0, 0, 0, 0),
  c(1/2, 1, 0, 0),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 1, 0, 0)
)
expect_equal(receiver_stats[, , "rrankReceive"], rrankReceive)

# psABAB
psABAB <- rbind(
	matrix(0, ncol = nrow(actors)),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(1, 0, 0, 0),
	c(0, 0, 1, 0),
	c(0, 0, 0, 0)
)
expect_equal(receiver_stats[, , "psABAB"], psABAB)

# psABBA
psABBA <- rbind(
	matrix(0, ncol = nrow(actors)),
	c(0, 0, 0, 0),
	c(0, 1, 0, 0),
	c(0, 0, 0, 0),
	c(0, 1, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 1, 0, 0)
)
expect_equal(receiver_stats[, , "psABBA"], psABBA)

# psABXA
psABXA <- rbind(
	matrix(0, ncol = nrow(actors)),
	c(1, 0, 0, 0),
	c(0, 0, 0, 0),
	c(1, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 1, 0),
	c(0, 0, 0, 1),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0)
)
expect_equal(receiver_stats[, , "psABXA"], psABXA)

# psABXB
psABXB <- rbind(
	matrix(0, ncol = nrow(actors)),
	c(0, 0, 1, 0),
	c(0, 0, 0, 0),
	c(0, 0, 1, 0),
	c(0, 0, 0, 0),
	c(0, 1, 0, 0),
	c(0, 0, 1, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0)
)
expect_equal(receiver_stats[, , "psABXB"], psABXB)

# psABAY
psABAY <- rbind(
	matrix(0, ncol = nrow(actors)),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 1, 1),
	c(1, 0, 0, 1),
	c(0, 0, 0, 0)
)
expect_equal(receiver_stats[, , "psABAY"], psABAY)

# psABBY
psABBY <- rbind(
	matrix(0, ncol = nrow(actors)),
	c(0, 0, 0, 0),
	c(0, 0, 1, 1),
	c(0, 0, 0, 0),
	c(1, 0, 0, 1),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(1, 0, 1, 0)
)
expect_equal(receiver_stats[, , "psABBY"], psABBY)

# psABXY
psABXY <- rbind(
	matrix(1, ncol = nrow(actors)),
	c(0, 1, 0, 1),
	c(0, 0, 0, 0),
	c(0, 1, 0, 1),
	c(0, 0, 0, 0),
	c(1, 0, 0, 1),
	c(1, 1, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0),
	c(0, 0, 0, 0)
)
expect_equal(receiver_stats[, , "psABXY"], psABXY)

# test standardization
std_sender_effects <- ~
  indegreeSender(scaling = "std") + outdegreeSender(scaling = "std") + 
    totaldegreeSender(scaling = "std") 
std_receiver_effects <- ~
  indegreeReceiver(scaling = "std") + outdegreeReceiver(scaling = "std") + 
    totaldegreeReceiver(scaling = "std") +
    inertia(scaling = "std") + reciprocity(scaling = "std") +
    isp(scaling = "std") + itp(scaling = "std") + 
    osp(scaling = "std") + otp(scaling = "std") +
    isp(unique = TRUE, scaling = "std") + itp(unique = TRUE, scaling = "std") + 
    osp(unique = TRUE, scaling = "std") + otp(unique = TRUE, scaling = "std") 
std_stats <- remstats(reh,
  sender_effects = std_sender_effects,
  receiver_effects = std_receiver_effects
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

# test proportional scaling
prop_sender_effects <- ~
  indegreeSender(scaling = "prop") + outdegreeSender(scaling = "prop") + 
    totaldegreeSender(scaling = "prop") 
prop_receiver_effects <- ~
  indegreeReceiver(scaling = "prop") + outdegreeReceiver(scaling = "prop") + 
    totaldegreeReceiver(scaling = "prop") +
    inertia(scaling = "prop") + reciprocity(scaling = "prop") 
prop_stats <- remstats(reh,
  sender_effects = prop_sender_effects,
  receiver_effects = prop_receiver_effects
)
prop_sender_stats <- prop_stats$sender_stats
prop_receiver_stats <- prop_stats$receiver_stats

# indegreeSender
prop_indegreeSender <- sender_stats[,,"indegreeSender"] / 
  (1:nrow(sender_stats)-1)
prop_indegreeSender[1,] <- 1 / nrow(actors)
expect_equal(prop_sender_stats[,,"indegreeSender"], prop_indegreeSender)

# outdegreeSender
prop_outdegreeSender <- sender_stats[,,"outdegreeSender"] / 
  (1:nrow(sender_stats)-1)
prop_outdegreeSender[1,] <- 1 / nrow(actors)
expect_equal(prop_sender_stats[,,"outdegreeSender"], prop_outdegreeSender)

# totaldegreeSender
prop_totaldegreeSender <- sender_stats[,,"totaldegreeSender"] / 
  (2 * (1:nrow(sender_stats)-1))
prop_totaldegreeSender[1,] <- 1 / nrow(actors)
expect_equal(prop_sender_stats[,,"totaldegreeSender"], prop_totaldegreeSender)

# indegreeReceiver
prop_indegreeReceiver <- receiver_stats[,,"indegreeReceiver"] / 
  (1:nrow(receiver_stats)-1)
prop_indegreeReceiver[1,] <- 1 / nrow(actors)
expect_equal(prop_receiver_stats[,,"indegreeReceiver"], prop_indegreeReceiver)

# outdegreeReceiver
prop_outdegreeReceiver <- receiver_stats[,,"outdegreeReceiver"] / 
  (1:nrow(receiver_stats)-1)
prop_outdegreeReceiver[1,] <- 1 / nrow(actors)
expect_equal(prop_receiver_stats[,,"outdegreeReceiver"], prop_outdegreeReceiver)

# totaldegreeReceiver
prop_totaldegreeReceiver <- receiver_stats[,,"totaldegreeReceiver"] / 
  (2 * (1:nrow(sender_stats)-1))
prop_totaldegreeReceiver[1,] <- 1 / nrow(actors)
expect_equal(prop_receiver_stats[,,"totaldegreeReceiver"], 
  prop_totaldegreeReceiver)

# inertia
prop_inertia <- t(sapply(1:nrow(edgelist), function(m) {
  outdegree_thisSender <- sender_stats[m, edgelist[m, 2], "outdegreeSender"]
  if(outdegree_thisSender == 0) {
    scaled_original <- rep(1/(nrow(actors)-1), nrow(actors))
  } else {
    scaled_original <- receiver_stats[m,,"inertia"] / outdegree_thisSender
  }
  scaled_original[edgelist[m,2]] <- 0
  scaled_original  
}))
expect_equal(prop_receiver_stats[,,"inertia"], prop_inertia)

# reciprocity
prop_reciprocity <- t(sapply(1:nrow(edgelist), function(m) {
  indegree_thisSender <- sender_stats[m, edgelist[m, 2], "indegreeSender"]
  if(indegree_thisSender == 0) {
    scaled_original <- rep(1/(nrow(actors)-1), nrow(actors))
  } else {
    scaled_original <- receiver_stats[m,,"reciprocity"] / indegree_thisSender
  }
  scaled_original[edgelist[m,2]] <- 0
  scaled_original  
}))
expect_equal(prop_receiver_stats[,,"reciprocity"], prop_reciprocity)
