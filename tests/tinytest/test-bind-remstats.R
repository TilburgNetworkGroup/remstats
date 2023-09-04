# Create example remstats objects
# ----------------------------------
edgelist <- data.frame(
  time = c(1:4),
  actor1 = sample(1:3, 4, replace = T),
  actor2 = sample(4:6, 4, replace = T)
)

# Check for the tie-oriented model
reh <- remify::remify(edgelist, model = "tie", actors = 1:6)

# Create example remstats objects
rs1 <- remstats(reh = reh, tie_effects = ~ inertia())
rs2 <- remstats(reh = reh, tie_effects = ~ otp())

# Combine remstats objects
expect_warning(combined_stats <- bind_remstats(rs1, rs2))

# Check dimensions
expect_equal(dim(combined_stats), c(4, 30, 3))

# Check model attribute
expect_equal(attr(combined_stats, "model"), "tie")

# Check formula attribute
expect_equal(attr(combined_stats, "formula"), as.formula("~ inertia() + otp()"))

# Check riskset attribute
expect_equal(attr(combined_stats, "riskset"), attr(rs1, "riskset"))

# Check adjmat attribute
expect_equal(attr(combined_stats, "adjmat"), attr(rs1, "adjmat"))

# Check statistics
if (at_home()) {
  expected_stats <- abind::abind(rs1, rs2[, , 2], along = 3)
  attributes(combined_stats) <- NULL
  attributes(expected_stats) <- NULL
  expect_true(identical(expected_stats, combined_stats))
}

# Check different remstats objects
reh_actor <- remify::remify(edgelist, model = "actor", actors = 1:6)
rs3 <- remstats(reh = reh, tie_effects = ~ inertia())
rs4 <- remstats(reh = reh_actor, receiver_effects = ~ otp())
expect_error(bind_remstats(rs3, rs4), "All objects should be of class")

# Check for the actor-oriented model
# ----------------------------------
reh <- remify::remify(edgelist, model = "actor", actors = 1:6)

# Create example remstats objects
rs1 <- remstats(reh = reh, receiver_effects = ~ inertia())
rs2 <- remstats(reh = reh, receiver_effects = ~ otp())

# Combine remstats objects
expect_silent(combined_stats <- bind_remstats(rs1, rs2))

# Create example remstats objects
rs3 <- remstats(reh = reh, receiver_effects = ~ inertia())
rs4 <- remstats(reh = reh, sender_effects = ~ indegreeSender())

# Combine remstats objects
expect_silent(combined_stats <- bind_remstats(rs3, rs4))

# Create example remstats objects
rs5 <- remstats(reh = reh, sender_effects = ~ outdegreeSender())
rs6 <- remstats(reh = reh, sender_effects = ~ indegreeSender())

# Combine remstats objects
expect_silent(combined_stats <- bind_remstats(rs5, rs6))