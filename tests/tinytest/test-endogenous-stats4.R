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
effects <- ~ FEtype() + 
  degreeDiff() + degreeMin() + degreeMax() + totaldegreeDyad() +
  inertia() + sp() + sp(unique = TRUE) + psABAB() + psABAY() +
  inertia(consider_type = TRUE) + degreeDiff(consider_type = TRUE) + 
  degreeMin(consider_type = TRUE) + degreeMax(consider_type = TRUE) +
  totaldegreeDyad(consider_type = TRUE) +
  sp(consider_type = TRUE) + sp(unique = TRUE, consider_type = TRUE) +
  psABAB(consider_type = TRUE) + psABAY(consider_type = TRUE)
stats <- remstats(reh, tie_effects = effects)
riskset <- attr(stats, "riskset")

# baseline
expect_equal(stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# FEtype
FEtype <- cbind(matrix(0, nrow = nrow(edgelist), ncol = sum(riskset$type == 1)), 
	matrix(1, nrow = nrow(edgelist), ncol = sum(riskset$type == 2)))
expect_equal(stats[, , "FEtype"], FEtype)

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

# totaldegreeDyad
totaldegreeDyad <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 2, 1, 1, 0, 1, 2, 1, 1),
  c(3, 3, 2, 2, 1, 3, 3, 2, 1),
  c(4, 5, 3, 3, 1, 4, 5, 3, 2),
  c(5, 6, 3, 5, 2, 5, 6, 5, 3),
  c(6, 7, 3, 7, 3, 6, 7, 7, 4),
  c(6, 8, 4, 8, 4, 6, 8, 8, 6),
  c(8, 9, 5, 9, 5, 8, 9, 9, 6),
  c(9, 10, 5, 11, 6, 9, 10, 11, 7),
  c(10, 10, 6, 12, 8, 10, 10, 12, 8)
)
expect_equal(stats[, , "totaldegreeDyad"], totaldegreeDyad)

# totaldegreeDyad.type
totaldegreeDyad.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 2, 1, 1, 0, 0, 0, 0, 0),
  c(3, 3, 2, 2, 1, 0, 0, 0, 0),
  c(3, 3, 2, 2, 1, 1, 2, 1, 1),
  c(3, 3, 2, 2, 1, 2, 3, 3, 2),
  c(4, 4, 2, 4, 2, 2, 3, 3, 2),
  c(4, 4, 2, 4, 2, 2, 4, 4, 4),
  c(4, 4, 2, 4, 2, 4, 5, 5, 4),
  c(5, 5, 2, 6, 3, 4, 5, 5, 4),
  c(6, 5, 3, 7, 5, 4, 5, 5, 4)
)
expect_equal(stats[, , "totaldegreeDyad.type"], totaldegreeDyad.type)

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
  degreeDiff(scaling = "std") + totaldegreeDyad(scaling = "std") +
  inertia(scaling = "std") + 
  sp(scaling = "std") + sp(scaling = "std", unique = TRUE) +
  degreeMin(consider_type = TRUE, scaling = "std") + 
  degreeMax(consider_type = TRUE, scaling = "std") +
  degreeDiff(consider_type = TRUE, scaling = "std") +
  totaldegreeDyad(consider_type = TRUE, scaling = "std") +
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

prop_effects <- ~ degreeMin(scaling = "prop") + 
  degreeMax(scaling = "prop") +
  totaldegreeDyad(scaling = "prop") +
  degreeMin(consider_type = TRUE, scaling = "prop") + 
  degreeMax(consider_type = TRUE, scaling = "prop") +
  totaldegreeDyad(consider_type = TRUE, scaling = "prop")
prop_stats <- remstats(reh, tie_effects = prop_effects)

sapply(c(2:3, 5:6), function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[,,stat_name] / (1:nrow(stats)-1)
  scaled_original[1,] <- 1/4
  expect_equal(prop_stats[,,stat_name], scaled_original)
})

# totaldegreeDyad
prop_totaldegreeDyad <- stats[,,"totaldegreeDyad"] / (2*(1:nrow(stats)-1))
prop_totaldegreeDyad[1,] <- prop_totaldegreeDyad[1,] <- 2/4
expect_equal(prop_stats[,,"totaldegreeDyad"], prop_totaldegreeDyad)

# totaldegreeDyad.type
prop_totaldegreeDyad.type <- stats[,,"totaldegreeDyad.type"] / (2*(1:nrow(stats)-1))
prop_totaldegreeDyad.type[1,] <- prop_totaldegreeDyad.type[1,] <- 2/4
expect_equal(prop_stats[,,"totaldegreeDyad.type"], prop_totaldegreeDyad.type)
