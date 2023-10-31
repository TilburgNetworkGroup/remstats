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
effects <- ~ degreeDiff() + degreeMin() + degreeMax() + totaldegreeDyad() +
  inertia() + sp() + sp(unique = TRUE) + psABAB() + psABAY()
stats <- remstats(reh, tie_effects = effects)
riskset <- attr(stats, "riskset")

# baseline
expect_equal(stats[, , "baseline"], matrix(1, nrow = nrow(edgelist), ncol = nrow(riskset)))

# degreeMin
degreeMin <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 0, 0, 0, 0),
  c(1, 1, 0, 1, 0, 0),
  c(1, 2, 0, 1, 0, 0),
  c(2, 3, 0, 2, 0, 0),
  c(3, 3, 0, 3, 0, 0),
  c(3, 3, 1, 3, 1, 1),
  c(4, 4, 1, 4, 1, 1),
  c(4, 4, 1, 5, 1, 1),
  c(4, 4, 2, 6, 2, 2)
)
expect_equal(stats[, , "degreeMin"], degreeMin)

# degreeMax
degreeMax <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 1, 1, 1, 0, 1),
  c(2, 2, 2, 1, 1, 1),
  c(3, 3, 3, 2, 1, 2),
  c(3, 3, 3, 3, 2, 3),
  c(3, 4, 3, 4, 3, 4),
  c(3, 5, 3, 5, 3, 5),
  c(4, 5, 4, 5, 4, 5),
  c(5, 6, 4, 6, 5, 6),
  c(6, 6, 4, 6, 6, 6)
)
expect_equal(stats[, , "degreeMax"], degreeMax)

# degreeDiff
degreeDiff <- degreeMax - degreeMin
expect_equal(stats[, , "degreeDiff"], degreeDiff)

# totaldegreeDyad
totaldegreeDyad <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 2, 1, 1, 0, 1),
  c(3, 3, 2, 2, 1, 1),
  c(4, 5, 3, 3, 1, 2),
  c(5, 6, 3, 5, 2, 3),
  c(6, 7, 3, 7, 3, 4),
  c(6, 8, 4, 8, 4, 6),
  c(8, 9, 5, 9, 5, 6),
  c(9, 10, 5, 11, 6, 7),
  c(10, 10, 6, 12, 8, 8)
)
expect_equal(stats[, , "totaldegreeDyad"], totaldegreeDyad)

# ccp
# duration <<- c(3, 2, 1, 1, 3, 2, 1, 1, 1, 1)
# ccp_stats <- remstats(reh, tie_effects = ~ ccp(duration = duration))
# ccp <- rbind(
#   matrix(0, ncol = nrow(riskset)),
#   c(0, 0, 0, 0, 0, 0),
#   c(0, 0, 0, 1, 0, 0),
#   c(0, 0, 0, 1, 0, 0),
#   c(0, 0, 0, 0, 0, 0),
#   c(0, 0, 0, 0, 0, 0),
#   c(0, 0, 0, 0, 1, 0),
#   c(0, 1, 0, 0, 1, 0),
#   c(0, 0, 0, 0, 0, 0),
#   c(0, 0, 0, 0, 0, 0)
# )
# expect_equal(ccp_stats[, , "ccp"], ccp)

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
  degreeMin(scaling = "std") + degreeMax(scaling = "std") +
  degreeDiff(scaling = "std") + totaldegreeDyad(scaling = "std") +
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

prop_effects <- ~ degreeMin(scaling = "prop") + degreeMax(scaling = "prop") + 
  totaldegreeDyad(scaling = "prop") 
prop_stats <- remstats(reh, tie_effects = prop_effects)

sapply(2:3, function(p) {
  stat_name <- dimnames(prop_stats)[[3]][p]
  scaled_original <- stats[,,stat_name] / (1:nrow(stats)-1)
  scaled_original[1,] <- 1/4
  expect_equal(prop_stats[,,stat_name], scaled_original)
}) # degreeMin and degreeMax

# totaldegreeDyad
prop_totaldegreeDyad <- stats[,,"totaldegreeDyad"] / (2*(1:nrow(stats)-1))
prop_totaldegreeDyad[1,] <- prop_totaldegreeDyad[1,] <- 1/4
expect_equal(prop_stats[,,"totaldegreeDyad"], prop_totaldegreeDyad)