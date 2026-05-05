# Condition 4: Undirected events with types, tie-oriented model with active riskset

library(tinytest)

edgelist <- data.frame(
  time = 1:10,
  actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
  actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1),
  type = c(1, 1, 2, 2, 1, 2, 2, 1, 1, 1)
)
reh <- remify(edgelist, model = "tie", directed = FALSE, riskset = "active",
											 extend_riskset_by_type = TRUE)

# ── "ignore" (default) ───────────────────────────────────────────────────────
effects_ig <- ~ FEtype() +
  degreeDiff() + degreeMin() + degreeMax() + totaldegreeDyad() +
  inertia() + sp() + sp(unique = TRUE) + psABAB() + psABAY()

stats <- remstats(reh, tie_effects = effects_ig, start = 1)
riskset <- attr(stats, "riskset")

# No type suffixes
expect_false(any(grepl("\\.1$|\\.2$|TypeAgg", dimnames(stats)[[3]])))

# baseline
expect_equal(stats[, , "baseline"],
  matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# FEtype
FEtype <- cbind(
  matrix(0, nrow = nrow(edgelist), ncol = sum(riskset$type == 1)),
  matrix(1, nrow = nrow(edgelist), ncol = sum(riskset$type == 2)))
expect_equal(stats[, , "FEtype_2"], FEtype)

# degreeMin (ignore = aggregate)
degreeMin.ig <- rbind(
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
expect_equal(stats[, , "degreeMin"], degreeMin.ig)

# degreeMax (ignore)
degreeMax.ig <- rbind(
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
expect_equal(stats[, , "degreeMax"], degreeMax.ig)

# degreeDiff (ignore)
expect_equal(stats[, , "degreeDiff"], degreeMax.ig - degreeMin.ig)

# totaldegreeDyad (ignore)
totaldegreeDyad.ig <- rbind(
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
expect_equal(stats[, , "totaldegreeDyad"], totaldegreeDyad.ig)

# inertia (ignore)
inertia.ig <- rbind(
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
expect_equal(stats[, , "inertia"], inertia.ig)

# sp (ignore)
sp.ig <- rbind(
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
expect_equal(stats[, , "sp"], sp.ig)

# sp.unique (ignore)
sp.unique.ig <- rbind(
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
expect_equal(stats[, , "sp.unique"], sp.unique.ig)

# psABAB (ignore)
psABAB.ig <- rbind(
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
expect_equal(stats[, , "psABAB"], psABAB.ig)

# psABAY (ignore)
psABAY.ig <- rbind(
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
expect_equal(stats[, , "psABAY"], psABAY.ig)

# ── "separate" ───────────────────────────────────────────────────────────────
stats_sep <- remstats(reh, tie_effects = ~
  inertia(consider_type = "separate") +
  degreeMin(consider_type = "separate"), start = 1)

expect_true(all(c("inertia.1","inertia.2",
                  "degreeMin.1","degreeMin.2") %in% dimnames(stats_sep)[[3]]))

# inertia.1 + inertia.2 = inertia (ignore)
expect_equal(stats_sep[,,"inertia.1"] + stats_sep[,,"inertia.2"],
  inertia.ig, info = "separate inertia sums to ignore")

# degreeMin.1: type-1-only degreeMin (non-additive — min is not linear)
degreeMin.1 <- rbind(
	matrix(0, ncol = nrow(riskset)),
	c(0, 1, 0, 0, 0, 0, 1, 0, 0),
	c(1, 1, 0, 1, 0, 1, 1, 1, 0),
	c(1, 1, 0, 1, 0, 1, 1, 1, 0),
	c(1, 1, 0, 1, 0, 1, 1, 1, 0),
	c(2, 2, 0, 2, 0, 2, 2, 2, 0),
	c(2, 2, 0, 2, 0, 2, 2, 2, 0),
	c(2, 2, 0, 2, 0, 2, 2, 2, 0),
	c(2, 2, 0, 3, 0, 2, 2, 3, 0),
	c(2, 2, 1, 3, 1, 2, 2, 3, 1)
)
expect_equal(stats_sep[,,"degreeMin.1"], degreeMin.1, info = "degreeMin.1 correct")

# inertia.1: type-1 events only, same value for both dyad types of same actor pair
inertia.1 <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 0, 1, 0, 0),
  c(1, 1, 0, 0, 0, 1, 1, 0, 0),
  c(1, 1, 0, 0, 0, 1, 1, 0, 0),
  c(1, 1, 0, 0, 0, 1, 1, 0, 0),
  c(1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(1, 1, 0, 1, 0, 1, 1, 1, 0),
  c(1, 1, 0, 2, 0, 1, 1, 2, 0),
  c(1, 1, 0, 2, 1, 1, 1, 2, 0)
)
expect_equal(stats_sep[,,"inertia.1"], inertia.1, info = "inertia.1 correct")

# ── Standardization ──────────────────────────────────────────────────────────
std_effects <- ~
  degreeMin(scaling = "std") + degreeMax(scaling = "std") +
  degreeDiff(scaling = "std") + totaldegreeDyad(scaling = "std") +
  inertia(scaling = "std") +
  sp(scaling = "std") + sp(scaling = "std", unique = TRUE)
std_stats <- remstats(reh, tie_effects = std_effects, start = 1)

sapply(2:dim(std_stats)[3], function(p) {
  stat_name <- dimnames(std_stats)[[3]][p]
  scaled_original <- t(apply(stats[, , stat_name], 1, scale))
  scaled_original[which(apply(stats[, , stat_name], 1, sd) == 0), ] <-
    rep(0, ncol(stats))
  expect_equal(std_stats[, , stat_name], scaled_original)
})

# ── Proportional scaling ─────────────────────────────────────────────────────
# inertia prop scaling not defined for undirected
prop_effects_err <- ~ inertia(scaling = "prop")
expect_error(remstats(reh, tie_effects = prop_effects_err, start = 1),
  pattern = "not defined")

prop_effects <- ~
  degreeMin(scaling = "prop") + degreeMax(scaling = "prop") +
  totaldegreeDyad(scaling = "prop")
prop_stats <- remstats(reh, tie_effects = prop_effects, start = 1)

# degreeMin, degreeMax: scaled by (m-1)
sapply(2:3, function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[,, stat_name] / (1:nrow(stats) - 1)
  scaled_original[1, ] <- 1/4
  expect_equal(prop_stats[,, stat_name], scaled_original)
})

# totaldegreeDyad: scaled by 2*(m-1)
prop_totaldegreeDyad <- stats[,,"totaldegreeDyad"] / (2*(1:nrow(stats)-1))
prop_totaldegreeDyad[1,] <- 1/4
expect_equal(prop_stats[,,"totaldegreeDyad"], prop_totaldegreeDyad)
