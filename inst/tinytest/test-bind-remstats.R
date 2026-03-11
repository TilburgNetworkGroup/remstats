# Create example remstats objects
edgelist <- data.frame(
  time = 1:4,
  actor1 = sample(1:3, 4, replace = T),
  actor2 = sample(4:6, 4, replace = T)
)

# --- Check for the tie-oriented model
reh <- remify::remify2(edgelist, model = "tie", actors = 1:6)

# Create example remstats objects
rs1 <- remstats(reh = reh, 
  tie_effects = ~ inertia() + reciprocity() + otp())
rs2 <- remstats(reh = reh, tie_effects = ~ outdegreeSender() + reciprocity())
rs3 <- remstats(reh = reh, tie_effects = ~ reciprocity())

# Combine remstats objects
expect_warning(combined_stats <- bind_remstats(rs1, rs2, rs3))

# Check dimensions
expect_equal(dim(combined_stats), c(4, 30, 5))

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
  expected_stats <- abind::abind(rs1, rs2[, , "outdegreeSender"], along = 3)
  attributes(combined_stats) <- NULL
  attributes(expected_stats) <- NULL
  expect_true(identical(expected_stats, combined_stats))
}

# Check different remstats objects
# TODO(actor-model): reh_actor <- remify::remify(edgelist, model = "actor", actors = 1:6)
# TODO(actor-model): rs3 <- remstats(reh = reh, tie_effects = ~ inertia())
# TODO(actor-model): rs4 <- remstats(reh = reh_actor, receiver_effects = ~ otp())
# TODO(actor-model): expect_error(bind_remstats(rs3, rs4), "All objects should be of class")

# TODO(actor-model): # --- Check for the actor-oriented model
# TODO(actor-model): reh <- remify::remify(edgelist, model = "actor", actors = 1:6)
# TODO(actor-model): 
# TODO(actor-model): # Create example remstats objects
# TODO(actor-model): rs1 <- remstats(reh = reh, 
# TODO(actor-model):   receiver_effects = ~ inertia() + reciprocity() + otp())
# TODO(actor-model): rs2 <- remstats(reh = reh, 
# TODO(actor-model):   receiver_effects = ~ reciprocity() + itp())
# TODO(actor-model): rs3 <- remstats(reh = reh, 
# TODO(actor-model):   receiver_effects = ~ indegreeReceiver())
# TODO(actor-model): 
# TODO(actor-model): # Combine remstats objects
# TODO(actor-model): expect_warning(combined_stats <- bind_remstats(rs1, rs2, rs3))
# TODO(actor-model): 
# TODO(actor-model): # Check statistics
# TODO(actor-model): if (at_home()) {
# TODO(actor-model):   expected_stats <- abind::abind(rs1$receiver_stats, 
# TODO(actor-model):     rs2$receiver_stats[,,"itp"], along = 3)
# TODO(actor-model):   expected_stats <- abind::abind(expected_stats, 
# TODO(actor-model):     rs3$receiver_stats[,,"indegreeReceiver"], along = 3)
# TODO(actor-model):   attributes(combined_stats$receiver_stats) <- NULL
# TODO(actor-model):   attributes(expected_stats) <- NULL
# TODO(actor-model):   expect_true(identical(expected_stats, combined_stats$receiver_stats))
# TODO(actor-model): }
# TODO(actor-model): 
# TODO(actor-model): # Create example remstats objects
# TODO(actor-model): rs4 <- remstats(reh = reh, 
# TODO(actor-model):   sender_effects = ~ outdegreeSender() + recencySendSender())
# TODO(actor-model): rs5 <- remstats(reh = reh, 
# TODO(actor-model):   sender_effects = ~ indegreeSender() + recencyReceiveSender())
# TODO(actor-model): rs6 <- remstats(reh = reh,
# TODO(actor-model):   sender_effects = ~ totaldegreeSender())
# TODO(actor-model): 
# TODO(actor-model): # Combine remstats objects
# TODO(actor-model): expect_warning(combined_stats <- bind_remstats(rs4, rs5, rs6))
# TODO(actor-model): 
# TODO(actor-model): # Check statistics
# TODO(actor-model): if (at_home()) {
# TODO(actor-model):   expected_stats <- abind::abind(rs4$sender_stats, 
# TODO(actor-model):     rs5$sender_stats[,,-1], along = 3)
# TODO(actor-model):   expected_stats <- abind::abind(expected_stats, rs6$sender_stats[,,-1], along = 3)
# TODO(actor-model):   attributes(combined_stats$sender_stats) <- NULL
# TODO(actor-model):   attributes(expected_stats) <- NULL
# TODO(actor-model):   expect_true(identical(expected_stats, combined_stats$sender_stats))
# TODO(actor-model): }
# TODO(actor-model): 
# TODO(actor-model): # Create example remstats objects
# TODO(actor-model): rs7 <- remstats(reh = reh, 
# TODO(actor-model):   receiver_effects = ~ inertia() + reciprocity() + otp(),
# TODO(actor-model):   sender_effects = ~ outdegreeSender() + recencySendSender())
# TODO(actor-model): rs8 <- remstats(reh = reh, 
# TODO(actor-model):   receiver_effects = ~ reciprocity() + itp(),
# TODO(actor-model):   sender_effects = ~ indegreeSender() + recencyReceiveSender())
# TODO(actor-model): rs9 <- remstats(reh = reh,
# TODO(actor-model):   receiver_effects = ~ indegreeReceiver(),
# TODO(actor-model):   sender_effects = ~ totaldegreeSender())
# TODO(actor-model): 
# TODO(actor-model): # Combine remstats objects
# TODO(actor-model): expect_warning(combined_stats <- bind_remstats(rs7, rs8, rs9))
# TODO(actor-model): 
# TODO(actor-model): # Check statistics
# TODO(actor-model): if (at_home()) {
# TODO(actor-model):   # Sender effects
# TODO(actor-model):   expected_stats <- abind::abind(rs7$sender_stats, 
# TODO(actor-model):     rs8$sender_stats[,,-1], along = 3)
# TODO(actor-model):   expected_stats <- abind::abind(expected_stats, 
# TODO(actor-model):     rs9$sender_stats[,,-1], along = 3)
# TODO(actor-model):   attributes(combined_stats$sender_stats) <- NULL
# TODO(actor-model):   attributes(expected_stats) <- NULL
# TODO(actor-model):   expect_true(identical(expected_stats, combined_stats$sender_stats))
# TODO(actor-model):   
# TODO(actor-model):   # Receiver effects
# TODO(actor-model):   expected_stats <- abind::abind(rs7$receiver_stats, 
# TODO(actor-model):     rs8$receiver_stats[,,"itp"], along = 3)
# TODO(actor-model):   expected_stats <- abind::abind(expected_stats, 
# TODO(actor-model):     rs9$receiver_stats[,,"indegreeReceiver"], along = 3)
# TODO(actor-model):   attributes(combined_stats$receiver_stats) <- NULL
# TODO(actor-model):   attributes(expected_stats) <- NULL
# TODO(actor-model):   expect_true(identical(expected_stats, combined_stats$receiver_stats))
# TODO(actor-model): }
