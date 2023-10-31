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
  inertia(consider_type = FALSE) + degreeDiff(consider_type = FALSE) + 
  degreeMin(consider_type = FALSE) + degreeMax(consider_type = FALSE) +
  totaldegreeDyad(consider_type = FALSE) +
  sp(consider_type = FALSE) + sp(unique = TRUE, consider_type = FALSE) +
  psABAB(consider_type = FALSE) + psABAY(consider_type = FALSE)
stats <- remstats(reh, tie_effects = effects)
riskset <- attr(stats, "riskset")

# baseline
expect_equal(stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# FEtype
FEtype <- cbind(matrix(0, nrow = nrow(edgelist), ncol = sum(riskset$type == 1)), 
	matrix(1, nrow = nrow(edgelist), ncol = sum(riskset$type == 2)))
expect_equal(stats[, , "FEtype_2"], FEtype)

# degreeMin.TypeAgg
degreeMin.TypeAgg <- rbind(
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
expect_equal(stats[, , "degreeMin.TypeAgg"], degreeMin.TypeAgg)

# degreeMin
degreeMin <- rbind(
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
expect_equal(stats[, , "degreeMin"], degreeMin)

# degreeMax.TypeAgg
degreeMax.TypeAgg <- rbind(
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
expect_equal(stats[, , "degreeMax.TypeAgg"], degreeMax.TypeAgg)

# degreeMax
degreeMax <- rbind(
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
expect_equal(stats[, , "degreeMax"], degreeMax)

# degreeDiff
degreeDiff.TypeAgg <- degreeMax.TypeAgg - degreeMin.TypeAgg
expect_equal(stats[, , "degreeDiff.TypeAgg"], degreeDiff.TypeAgg)

# degreeDiff
degreeDiff <- degreeMax - degreeMin
expect_equal(stats[, , "degreeDiff"], degreeDiff)

# totaldegreeDyad.TypeAgg
totaldegreeDyad.TypeAgg <- rbind(
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
expect_equal(stats[, , "totaldegreeDyad.TypeAgg"], totaldegreeDyad.TypeAgg)

# totaldegreeDyad
totaldegreeDyad <- rbind(
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
expect_equal(stats[, , "totaldegreeDyad"], totaldegreeDyad)

# inertia.TypeAgg
inertia.TypeAgg <- rbind(
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
expect_equal(stats[, , "inertia.TypeAgg"], inertia.TypeAgg)

# inertia
inertia <- rbind(
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
expect_equal(stats[, , "inertia"], inertia)

# sp.TypeAgg
sp.TypeAgg <- rbind(
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
expect_equal(stats[, , "sp.TypeAgg"], sp.TypeAgg)

# sp
sp <- rbind(
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
expect_equal(stats[, , "sp"], sp)

# sp.unique.TypeAgg
sp.unique.TypeAgg <- rbind(
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
expect_equal(stats[, , "sp.unique.TypeAgg"], sp.unique.TypeAgg)

# sp.unique
sp.unique <- rbind(
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
expect_equal(stats[, , "sp.unique"], sp.unique)

# psABAB.TypeAgg
psABAB.TypeAgg <- rbind(
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
expect_equal(stats[, , "psABAB.TypeAgg"], psABAB.TypeAgg)

# psABAB
psABAB <- rbind(
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
expect_equal(stats[, , "psABAB"], psABAB)

# psABAY.TypeAgg
psABAY.TypeAgg <- rbind(
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
expect_equal(stats[, , "psABAY.TypeAgg"], psABAY.TypeAgg)

# psABAY
psABAY <- rbind(
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
expect_equal(stats[, , "psABAY"], psABAY)

# test standardization
std_effects <- ~
  degreeMin(scaling = "std") + degreeMax(scaling = "std") +
  degreeDiff(scaling = "std") + totaldegreeDyad(scaling = "std") +
  inertia(scaling = "std") + 
  sp(scaling = "std") + sp(scaling = "std", unique = TRUE) +
  degreeMin(consider_type = FALSE, scaling = "std") + 
  degreeMax(consider_type = FALSE, scaling = "std") +
  degreeDiff(consider_type = FALSE, scaling = "std") +
  totaldegreeDyad(consider_type = FALSE, scaling = "std") +
  inertia(consider_type = FALSE, scaling = "std") + 
  sp(consider_type = FALSE, scaling = "std") +
  sp(consider_type = FALSE, scaling = "std", unique = TRUE) 
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
  inertia(consider_type = FALSE, scaling = "std")
expect_error(remstats(reh, tie_effects = prop_effects),
  pattern = "not defined")

prop_effects <- ~ degreeMin(scaling = "prop") + 
  degreeMax(scaling = "prop") +
  totaldegreeDyad(scaling = "prop") +
  degreeMin(consider_type = FALSE, scaling = "prop") + 
  degreeMax(consider_type = FALSE, scaling = "prop") +
  totaldegreeDyad(consider_type = FALSE, scaling = "prop")
prop_stats <- remstats(reh, tie_effects = prop_effects)

sapply(c(2:3, 5:6), function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[,,stat_name] / (1:nrow(stats)-1)
  scaled_original[1,] <- 1/4
  expect_equal(prop_stats[,,stat_name], scaled_original)
})

# totaldegreeDyad
prop_totaldegreeDyad <- stats[,,"totaldegreeDyad"] / (2*(1:nrow(stats)-1))
prop_totaldegreeDyad[1,] <- prop_totaldegreeDyad[1,] <- 1/4
expect_equal(prop_stats[,,"totaldegreeDyad"], prop_totaldegreeDyad)

# totaldegreeDyad
prop_totaldegreeDyad <- stats[,,"totaldegreeDyad"] / (2*(1:nrow(stats)-1))
prop_totaldegreeDyad[1,] <- prop_totaldegreeDyad[1,] <- 1/4
expect_equal(prop_stats[,,"totaldegreeDyad"], prop_totaldegreeDyad)
