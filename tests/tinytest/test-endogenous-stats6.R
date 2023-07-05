# Condition 6: Undirected events, tie-oriented model with active risk set

# Small edgelist
edgelist <- data.frame(
  time = 1:5,
  actor1 = c(1, 1, 2, 2, 3),
  actor2 = c(2, 3, 1, 4, 2)
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
  c(1, 0, 0, 0),
  c(1, 1, 0, 0),
  c(2, 1, 0, 0),
  c(2, 1, 0, 1)
)
expect_equal(stats[, , "inertia"], inertia)

# sp
sp <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0)
)
expect_equal(stats[, , which(dimnames(stats)[[3]] == "sp")[1]], sp)

# spUnique
spUnique <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0),
  c(0, 0, 1, 0)
)
expect_equal(stats[, , which(dimnames(stats)[[3]] == "sp")[2]], spUnique)

# psABAB
psABAB <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0),
  c(0, 0, 0, 1)
)
expect_equal(stats[, , "psABAB"], psABAB)

# psABAY
psABAY <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 1, 1, 1),
  c(1, 0, 1, 0),
  c(0, 1, 1, 1),
  c(1, 0, 1, 0)
)
expect_equal(stats[, , "psABAY"], psABAY)