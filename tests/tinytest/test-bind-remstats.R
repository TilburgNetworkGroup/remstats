# Create example remstats objects
edgelist <- data.frame(
  time = c(1:4),
  actor1 = sample(1:3, 4, replace = T),
  actor2 = sample(4:6, 4, replace = T)
)

# --- Check for the tie-oriented model
reh <- remify::remify(edgelist, model = "tie", actors = 1:6)

# Create example remstats objects
rs1 <- remstats(reh = reh, 
  tie_effects = ~ inertia() + reciprocity() + otp())
rs2 <- remstats(reh = reh, tie_effects = ~ outdegreeSender() + reciprocity())
rs3 <- remstats(reh = reh, tie_effects = ~ reciprocity())

# Combine remstats objects
expect_warning(combined_stats <- bind_remstats(rs1, rs2, rs3))

# Check dimensions
expect_equal(dim(combined_stats), c(4, 30, 7))

# Check model attribute
expect_equal(attr(combined_stats, "model"), "tie")

# Check formula attribute
expect_equal(attr(combined_stats, "formula"), "Combined remstats object")

# Check riskset attribute
expect_equal(attr(combined_stats, "riskset"), attr(rs1, "riskset"))

# Check adjmat attribute
expect_equal(attr(combined_stats, "adjmat"), attr(rs1, "adjmat"))

# Check statistics
if (at_home()) {
  expected_stats <- abind::abind(rs1, rs2[, , -1], along = 3)
  expected_stats <- abind::abind(expected_stats, rs3[, , -1], along = 3)
  attributes(combined_stats) <- NULL
  attributes(expected_stats) <- NULL
  expect_true(identical(expected_stats, combined_stats))
}

# Check different remstats objects
reh_actor <- remify::remify(edgelist, model = "actor", actors = 1:6)
rs3 <- remstats(reh = reh, tie_effects = ~ inertia())
rs4 <- remstats(reh = reh_actor, receiver_effects = ~ otp())
expect_error(bind_remstats(rs3, rs4), "All objects should be of class")

# --- Check for the actor-oriented model
reh <- remify::remify(edgelist, model = "actor", actors = 1:6)

# Create example remstats objects
rs1 <- remstats(reh = reh, 
  receiver_effects = ~ inertia() + reciprocity() + otp())
rs2 <- remstats(reh = reh, 
  receiver_effects = ~ reciprocity() + itp())
rs3 <- remstats(reh = reh, 
  receiver_effects = ~ indegreeReceiver())

# Combine remstats objects
expect_silent(combined_stats <- bind_remstats(rs1, rs2, rs3))

# Check statistics
if (at_home()) {
  expected_stats <- abind::abind(rs1$receiver_stats, 
    rs2$receiver_stats, along = 3)
  expected_stats <- abind::abind(expected_stats, rs3$receiver_stats, along = 3)
  attributes(combined_stats$receiver_stats) <- NULL
  attributes(expected_stats) <- NULL
  expect_true(identical(expected_stats, combined_stats$receiver_stats))
}

# Create example remstats objects
rs4 <- remstats(reh = reh, 
  sender_effects = ~ outdegreeSender() + recencySendSender())
rs5 <- remstats(reh = reh, 
  sender_effects = ~ indegreeSender() + recencyReceiveSender())
rs6 <- remstats(reh = reh,
  sender_effects = ~ totaldegreeSender())

# Check statistics
if (at_home()) {
  expected_stats <- abind::abind(rs4$sender_stats, 
    rs5$sender_stats, along = 3)
  expected_stats <- abind::abind(expected_stats, rs6$sender_stats, along = 3)
  attributes(combined_stats$sender_stats) <- NULL
  attributes(expected_stats) <- NULL
  expect_true(identical(expected_stats, combined_stats$sender_stats))
}

# Combine remstats objects
expect_warning(combined_stats <- bind_remstats(rs4, rs5, rs6))

# Create example remstats objects
rs7 <- remstats(reh = reh, 
  receiver_effects = ~ inertia() + reciprocity() + otp(),
  sender_effects = ~ outdegreeSender() + recencySendSender())
rs8 <- remstats(reh = reh, 
  receiver_effects = ~ reciprocity() + itp(),
  sender_effects = ~ indegreeSender() + recencyReceiveSender())
rs9 <- remstats(reh = reh,
  receiver_effects = ~ indegreeReceiver(),
  sender_effects = ~ totaldegreeSender())

# Combine remstats objects
expect_warning(combined_stats <- bind_remstats(rs7, rs8, rs9))

# Check statistics
if (at_home()) {
  expected_stats <- abind::abind(rs7$sender_stats, 
    rs8$sender_stats[,,-1], along = 3)
  expected_stats <- abind::abind(expected_stats, 
    rs9$sender_stats[,,-1], along = 3)
  attributes(combined_stats$sender_stats) <- NULL
  attributes(expected_stats) <- NULL
  expect_true(identical(expected_stats, combined_stats$sender_stats))
  
  expected_stats <- abind::abind(rs7$receiver_stats, 
    rs8$receiver_stats, along = 3)
  expected_stats <- abind::abind(expected_stats, rs9$receiver_stats, along = 3)
  attributes(combined_stats$receiver_stats) <- NULL
  attributes(expected_stats) <- NULL
  expect_true(identical(expected_stats, combined_stats$receiver_stats))
}
