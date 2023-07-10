# Condition 2: Undirected events, tie-oriented model with active risk set

# Small edgelist
edgelist <- data.frame(
  time = 1:10,
  actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
  actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
)

# Statistics
reh <- remify::remify(edgelist, model = "tie", directed = FALSE, 
  riskset = "active")
effects <- ~ inertia() + sp() + sp(unique = TRUE) + psABAB() + psABAY()
stats <- remstats(reh, tie_effects = effects)
riskset <- attr(stats, "riskset")

# baseline
expect_equal(stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# inertia
inertia <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0),
  c(1, 2, 0, 0, 0, 0),
  c(1, 2, 0, 1, 0, 0),
  c(1, 2, 0, 2, 0, 0),
  c(1, 2, 0, 2, 0, 1),
  c(2, 2, 0, 2, 0, 1),
  c(2, 2, 0, 3, 0, 1),
  c(2, 2, 0, 3, 1, 1)
)
expect_equal(stats[, , "inertia"], inertia)

# sp
sp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 1, 0, 0),
  c(1, 1, 0, 1, 0, 0),
  c(2, 1, 0, 1, 0, 0),
  c(2, 1, 1, 1, 1, 0),
  c(2, 2, 1, 2, 1, 0),
  c(2, 2, 1, 2, 1, 0),
  c(2, 2, 2, 3, 1, 1)
)
expect_equal(stats[, , "sp"], sp)

# sp.unique
sp.unique <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 1, 0, 0),
  c(1, 1, 0, 1, 0, 0),
  c(1, 1, 0, 1, 0, 0),
  c(1, 1, 1, 1, 1, 0),
  c(1, 1, 1, 1, 1, 0),
  c(1, 1, 1, 1, 1, 0),
  c(1, 1, 2, 2, 1, 1)
)
expect_equal(stats[, , "sp.unique"], sp.unique)

# psABAB
psABAB <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 1),
  c(1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 1, 0)
)
expect_equal(stats[, , "psABAB"], psABAB)

# psABAY
psABAY <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 1, 1, 0, 1),
  c(0, 1, 1, 1, 1, 0),
  c(1, 0, 1, 1, 0, 1),
  c(1, 1, 0, 0, 1, 1),
  c(1, 1, 0, 0, 1, 1),
  c(0, 1, 1, 1, 1, 0),
  c(0, 1, 1, 1, 1, 0),
  c(1, 1, 0, 0, 1, 1),
  c(1, 0, 1, 1, 0, 1)
)
expect_equal(stats[, , "psABAY"], psABAY)

# test standardization
std_effects <- ~
  inertia(scaling = "std") + sp(scaling = "std") + 
  sp(scaling = "std", unique = TRUE)
std_stats <- remstats(reh, tie_effects = std_effects)

sapply(2:dim(std_stats)[3], function(p) {
  stat_name <- dimnames(std_stats)[[3]][p]
  scaled_original <- t(apply(stats[, , stat_name], 1, scale))
  scaled_original[which(apply(stats[, , stat_name], 1, sd) == 0), ] <-
    rep(0, ncol(stats))
  expect_equal(std_stats[, , stat_name], scaled_original)
})

# test proportional scaling
prop_effects <- ~ inertia(scaling = "prop")
expect_error(remstats(reh, tie_effects = prop_effects),
  pattern = "not defined")