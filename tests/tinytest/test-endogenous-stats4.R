# Condition 4: Undirected events with types, tie-oriented model with active riskset

# Small edgelist
edgelist <- data.frame(
  time = 1:5,
  actor1 = c(1, 1, 2, 2, 3),
  actor2 = c(2, 3, 1, 3, 2)
)

event_types <- c(1, 1, 2, 2, 1)

# Statistics
edgelist$type <- event_types
reh <- remify::remify(edgelist, model = "tie", directed = FALSE, 
  riskset = "active")
effects <- ~ inertia() + sp() + sp(unique = TRUE) + psABAB() + psABAY() +
  inertia(consider_type = TRUE) +
  sp(consider_type = TRUE) + sp(unique = TRUE, consider_type = TRUE) +
  psABAB(consider_type = TRUE) + psABAY(consider_type = TRUE)
stats <- remstats(reh, tie_effects = effects)
riskset <- attr(stats, "riskset")

# baseline
expect_equal(stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# inertia
inertia <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 1, 0),
  c(1, 1, 0, 1, 0),
  c(2, 1, 0, 2, 0),
  c(2, 1, 1, 2, 1)
)
expect_equal(stats[, , "inertia"], inertia)

# inertia.type
inertia.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0),
  c(1, 1, 0, 1, 0),
  c(1, 1, 0, 1, 1)
)
expect_equal(stats[, , "inertia.type"], inertia.type)

# sp
sp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 1),
  c(0, 0, 1, 0, 1),
  c(1, 1, 1, 1, 1)
)
expect_equal(stats[, , "sp"], sp)

# sp.type
sp.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0)
)
expect_equal(stats[, , "sp.type"], sp.type)

# sp.unique
sp.unique <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 1),
  c(0, 0, 1, 0, 1),
  c(1, 1, 1, 1, 1)
)
expect_equal(stats[, , "sp.unique"], sp.unique)

# sp.unique.type
sp.unique.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0)
)
expect_equal(stats[, , "sp.unique.type"], sp.unique.type)

# psABAB
psABAB <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 1, 0),
  c(0, 1, 0, 0, 0),
  c(1, 0, 0, 1, 0),
  c(0, 0, 1, 0, 1)
)
expect_equal(stats[, , "psABAB"], psABAB)

# psABAB.type
psABAB.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0),
  c(0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 1)
)
expect_equal(stats[, , "psABAB.type"], psABAB.type)

# psABAY
psABAY <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 1, 0, 1),
  c(1, 0, 1, 1, 1),
  c(0, 1, 1, 0, 1),
  c(1, 1, 0, 1, 0)
)
expect_equal(stats[, , "psABAY"], psABAY)

# psABAY.type
psABAY.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 1, 0, 0),
  c(1, 0, 1, 0, 0),
  c(0, 0, 0, 0, 1),
  c(0, 0, 0, 1, 0)
)
expect_equal(stats[, , "psABAY.type"], psABAY.type)

# test standardization
std_effects <- ~
  inertia(scaling = "std") + sp(scaling = "std") + 
  sp(scaling = "std", unique = TRUE) +
  inertia(consider_type = TRUE, scaling = "std") + 
  sp(consider_type = TRUE, scaling = "std") +
  sp(consider_type = TRUE, scaling = "std", unique = TRUE) 
std_stats <- remstats(reh, tie_effects = std_effects)

sapply(2:dim(std_stats)[3], function(p) {
  stat_name <- dimnames(std_stats)[[3]][p]
  scaled_original <- t(apply(stats[, , stat_name], 1, scale))
  scaled_original[which(apply(stats[, , stat_name], 1, sd) == 0), ] <-
    rep(0, ncol(stats))
  expect_equal(std_stats[, , stat_name], scaled_original)
})

# test proportional scaling
prop_effects <- ~ inertia(scaling = "prop") + 
  inertia(consider_type = TRUE, scaling = "std")
expect_error(remstats(reh, tie_effects = prop_effects),
  pattern = "not defined")
