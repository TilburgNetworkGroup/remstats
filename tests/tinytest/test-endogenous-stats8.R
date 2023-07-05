# Condition 8: Undirected events with types, tie-oriented model with active riskset

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
expect_equal(stats[, , which(dimnames(stats)[[3]] == "sp")[1]], sp)

# sp.type
sp.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0)
)
expect_equal(stats[, , which(dimnames(stats)[[3]] == "sp.type")[1]], sp.type)

# spUnique
spUnique <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 1),
  c(0, 0, 1, 0, 1),
  c(1, 1, 1, 1, 1)
)
expect_equal(stats[, , which(dimnames(stats)[[3]] == "sp")[2]], spUnique)

# spUnique.type
spUnique.type <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0),
  c(0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0),
  c(0, 0, 1, 0, 0)
)
expect_equal(stats[, , which(dimnames(stats)[[3]] == "sp.type")[2]], 
  spUnique.type)

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
