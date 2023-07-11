# Condition 4: Undirected events with types, tie-oriented model with active riskset

# Small edgelist
edgelist <- data.frame(
  time = 1:10,
  actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
  actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
)

event_types <- c(1, 1, 2, 2, 1, 2, 2, 1, 1, 1)

# Statistics
edgelist$type <- event_types
reh <- remify::remify(edgelist, model = "tie", directed = FALSE, 
  riskset = "active")
effects <- ~ degreeDiff() + degreeMin() + degreeMax() + 
  inertia() + sp() + sp(unique = TRUE) + psABAB() + psABAY() +
  inertia(consider_type = TRUE) + degreeDiff(consider_type = TRUE) + 
  degreeMin(consider_type = TRUE) + degreeMax(consider_type = TRUE) +
  sp(consider_type = TRUE) + sp(unique = TRUE, consider_type = TRUE) +
  psABAB(consider_type = TRUE) + psABAY(consider_type = TRUE)
stats <- remstats(reh, tie_effects = effects)
riskset <- attr(stats, "riskset")

# baseline
expect_equal(stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# degreeMin
degreeMin <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 0, 1, 0, 0),
  c(1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(1, 2, 0, 1, 0, 1, 2, 1, 0),
  c(2, 3, 0, 2, 0, 2, 3, 2, 0),
  c(3, 3, 0, 3, 0, 3, 3, 3, 0),
  c(3, 3, 1, 3, 1, 3, 3, 3, 1),
  c(4, 4, 1, 4, 1, 4, 4, 4, 1),
  c(4, 4, 1, 5, 1, 4, 4, 5, 1),
  c(4, 4, 2, 6, 2, 4, 4, 6, 2)
)
expect_equal(stats[, , "degreeMin"], degreeMin)

# degreeMin.type
degreeMin.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(1, 1, 0, 1, 0, 0, 0, 0, 0),
  c(1, 1, 0, 1, 0, 0, 1, 0, 0),
  c(1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(2, 2, 0, 2, 0, 1, 1, 1, 0),
  c(2, 2, 0, 2, 0, 1, 1, 1, 1),
  c(2, 2, 0, 2, 0, 2, 2, 2, 1),
  c(2, 2, 0, 3, 0, 2, 2, 2, 1),
  c(2, 2, 1, 3, 1, 2, 2, 2, 1)
)
expect_equal(stats[, , "degreeMin.type"], degreeMin.type)

# degreeMax
degreeMax <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 1, 1, 1, 0, 1, 1, 1, 1),
  c(2, 2, 2, 1, 1, 2, 2, 1, 1),
  c(3, 3, 3, 2, 1, 3, 3, 2, 2),
  c(3, 3, 3, 3, 2, 3, 3, 3, 3),
  c(3, 4, 3, 4, 3, 3, 4, 4, 4),
  c(3, 5, 3, 5, 3, 3, 5, 5, 5),
  c(4, 5, 4, 5, 4, 4, 5, 5, 5),
  c(5, 6, 4, 6, 5, 5, 6, 6, 6),
  c(6, 6, 4, 6, 6, 6, 6, 6, 6)
)
expect_equal(stats[, , "degreeMax"], degreeMax)

# degreeMax.type
degreeMax.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 1, 1, 1, 0, 0, 0, 0, 0),
  c(2, 2, 2, 1, 1, 0, 0, 0, 0),
  c(2, 2, 2, 1, 1, 1, 1, 1, 1),
  c(2, 2, 2, 1, 1, 1, 2, 2, 2),
  c(2, 2, 2, 2, 2, 1, 2, 2, 2),
  c(2, 2, 2, 2, 2, 1, 3, 3, 3),
  c(2, 2, 2, 2, 2, 2, 3, 3, 3),
  c(3, 3, 2, 3, 3, 2, 3, 3, 3),
  c(4, 3, 2, 4, 4, 2, 3, 3, 3)
)
expect_equal(stats[, , "degreeMax.type"], degreeMax.type)

# degreeDiff
degreeDiff <- degreeMax - degreeMin
expect_equal(stats[, , "degreeDiff"], degreeDiff)

# degreeDiff.type
degreeDiff.type <- degreeMax.type - degreeMin.type
expect_equal(stats[, , "degreeDiff.type"], degreeDiff.type)

# inertia
inertia <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 0, 1, 0, 0),
  c(1, 1, 0, 0, 0, 1, 1, 0, 0),
  c(1, 2, 0, 0, 0, 1, 2, 0, 0),
  c(1, 2, 0, 1, 0, 1, 2, 1, 0),
  c(1, 2, 0, 2, 0, 1, 2, 2, 0),
  c(1, 2, 0, 2, 0, 1, 2, 2, 1),
  c(2, 2, 0, 2, 0, 2, 2, 2, 1),
  c(2, 2, 0, 3, 0, 2, 2, 3, 1),
  c(2, 2, 0, 3, 1, 2, 2, 3, 1)
)
expect_equal(stats[, , "inertia"], inertia)

# inertia.type
inertia.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 1, 0, 0),
  c(1, 1, 0, 0, 0, 0, 1, 1, 0),
  c(1, 1, 0, 1, 0, 0, 1, 1, 0),
  c(1, 1, 0, 1, 0, 0, 1, 1, 1),
  c(1, 1, 0, 1, 0, 1, 1, 1, 1),
  c(1, 1, 0, 2, 0, 1, 1, 1, 1),
  c(1, 1, 0, 2, 1, 1, 1, 1, 1)
)
expect_equal(stats[, , "inertia.type"], inertia.type)

# sp
sp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0, 0, 1, 0),
  c(0, 0, 0, 1, 0, 0, 0, 1, 0),
  c(1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(2, 1, 0, 1, 0, 2, 1, 1, 0),
  c(2, 1, 1, 1, 1, 2, 1, 1, 0),
  c(2, 2, 1, 2, 1, 2, 2, 2, 0),
  c(2, 2, 1, 2, 1, 2, 2, 2, 0),
  c(2, 2, 2, 3, 1, 2, 2, 3, 1)
)
expect_equal(stats[, , "sp"], sp)

# sp.type
sp.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 1, 0, 0, 0),
  c(1, 1, 0, 1, 0, 1, 0, 0, 0),
  c(1, 1, 0, 1, 0, 1, 0, 0, 0),
  c(1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(1, 1, 1, 1, 0, 1, 1, 1, 0)
)
expect_equal(stats[, , "sp.type"], sp.type)

# sp.unique
sp.unique <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0, 0, 1, 0),
  c(0, 0, 0, 1, 0, 0, 0, 1, 0),
  c(1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(1, 1, 1, 1, 1, 1, 1, 1, 0),
  c(1, 1, 1, 1, 1, 1, 1, 1, 0),
  c(1, 1, 1, 1, 1, 1, 1, 1, 0),
  c(1, 1, 2, 2, 1, 1, 1, 2, 1)
)
expect_equal(stats[, , "sp.unique"], sp.unique)

# sp.unique.type
sp.unique.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 1, 0, 1, 0, 0, 0),
  c(1, 1, 0, 1, 0, 1, 0, 0, 0),
  c(1, 1, 0, 1, 0, 1, 0, 0, 0),
  c(1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(1, 1, 1, 1, 0, 1, 1, 1, 0)
)
expect_equal(stats[, , "sp.unique.type"], sp.unique.type)

# psABAB
psABAB <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 0, 1, 0, 0),
  c(1, 0, 0, 0, 0, 1, 0, 0, 0),
  c(0, 1, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 1, 0, 0, 0, 1, 0),
  c(0, 0, 0, 1, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 1),
  c(1, 0, 0, 0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABAB"], psABAB)

# psABAB.type
psABAB.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 1, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 1, 0),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0, 0, 1),
  c(0, 0, 0, 0, 0, 1, 0, 0, 0),
  c(0, 0, 0, 1, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 1, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABAB.type"], psABAB.type)

# psABAY
psABAY <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 1, 1, 0, 1, 0, 1, 1),
  c(0, 1, 1, 1, 1, 0, 1, 1, 0),
  c(1, 0, 1, 1, 0, 1, 0, 1, 1),
  c(1, 1, 0, 0, 1, 1, 1, 0, 1),
  c(1, 1, 0, 0, 1, 1, 1, 0, 1),
  c(0, 1, 1, 1, 1, 0, 1, 1, 0),
  c(0, 1, 1, 1, 1, 0, 1, 1, 0),
  c(1, 1, 0, 0, 1, 1, 1, 0, 1),
  c(1, 0, 1, 1, 0, 1, 0, 1, 1)
)
expect_equal(stats[, , "psABAY"], psABAY)

# psABAY.type
psABAY.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 1, 1, 0, 0, 0, 0, 0),
  c(0, 1, 1, 1, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 1, 0, 1, 1),
  c(0, 0, 0, 0, 0, 1, 1, 0, 1),
  c(1, 1, 0, 0, 1, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 1, 1, 0),
  c(0, 0, 0, 0, 0, 0, 1, 1, 0),
  c(1, 1, 0, 0, 1, 0, 0, 0, 0),
  c(1, 0, 1, 1, 0, 0, 0, 0, 0)
)
expect_equal(stats[, , "psABAY.type"], psABAY.type)

# test standardization
std_effects <- ~
  degreeMin(scaling = "std") + degreeMax(scaling = "std") +
  degreeDiff(scaling = "std") + inertia(scaling = "std") + 
  sp(scaling = "std") + sp(scaling = "std", unique = TRUE) +
  degreeMin(consider_type = TRUE, scaling = "std") + 
  degreeMax(consider_type = TRUE, scaling = "std") +
  degreeDiff(consider_type = TRUE, scaling = "std") +
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

prop_effects <- ~ degreeMin(consider_type = TRUE, scaling = "prop") + 
  degreeMax(consider_type = TRUE, scaling = "prop")
prop_stats <- remstats(reh, tie_effects = prop_effects)

sapply(2:dim(prop_stats)[3], function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[,,stat_name] / (1:nrow(stats)-1)
  scaled_original[1,] <- 1/4
  expect_equal(prop_stats[,,stat_name], scaled_original)
})