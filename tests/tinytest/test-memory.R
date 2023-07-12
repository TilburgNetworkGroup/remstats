# Small edgelist
edgelist <- data.frame(
  time = 1:10,
  actor1 = c(1, 2, 1, 2, 3, 4, 2, 2, 2, 4),
  actor2 = c(3, 1, 3, 3, 2, 3, 1, 3, 4, 1)
)

reh <- remify::remify(edgelist, model = "tie", riskset = "active")
effects <- ~ inertia()

stats_window <- remstats(
  reh = reh, tie_effects = effects, memory = "window",
  memory_value = 5
)
riskset <- attr(stats_window, "riskset")

inertia_window <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 0),
  c(2, 1, 0, 0, 0, 0, 0),
  c(2, 1, 1, 0, 0, 0, 0),
  c(2, 1, 1, 0, 1, 0, 0),
  c(1, 1, 1, 0, 1, 0, 1),
  c(1, 1, 1, 0, 1, 0, 1),
  c(0, 1, 2, 0, 1, 0, 1),
  c(0, 1, 1, 1, 1, 0, 1)
)
expect_equal(stats_window[, , "inertia"], inertia_window)

stats_interval <- remstats(
  reh = reh, tie_effects = effects,
  memory = "interval", memory_value = c(2, 5)
)

intertia_interval <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(0, 0, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 0),
  c(1, 0, 0, 0, 0, 0, 0),
  c(1, 1, 0, 0, 0, 0, 0),
  c(2, 1, 0, 0, 0, 0, 0),
  c(1, 1, 1, 0, 0, 0, 0),
  c(1, 0, 1, 0, 1, 0, 0),
  c(0, 0, 1, 0, 1, 0, 1),
  c(0, 1, 0, 0, 1, 0, 1)
)
expect_equal(stats_interval[, , "inertia"], intertia_interval)

stats_decay <- remstats(
  reh = reh, tie_effects = effects,
  memory = "decay", memory_value = 5
)

df <- function(time, time_event) {
  exp(-(time - time_event) * (log(2) / 5)) * (log(2) / 5)
}

inertia_decay <- rbind(
  matrix(0, ncol = nrow(riskset)),
  c(df(2, 1), 0, 0, 0, 0, 0, 0),
  c(df(3, 1), df(3, 2), 0, 0, 0, 0, 0),
  c(df(4, 1) + df(4, 3), df(4, 2), 0, 0, 0, 0, 0),
  c(df(5, 1) + df(5, 3), df(5, 2), df(5, 4), 0, 0, 0, 0),
  c(df(6, 1) + df(6, 3), df(6, 2), df(6, 4), 0, df(6, 5), 0, 0),
  c(df(7, 1) + df(7, 3), df(7, 2), df(7, 4), 0, df(7, 5), 0, df(7, 6)),
  c(df(8, 1) + df(8, 3), df(8, 2) + df(8, 7), df(8, 4), 0, df(8, 5), 0, df(8, 6)),
  c(df(9, 1) + df(9, 3), df(9, 2) + df(9, 7), df(9, 4) + df(9, 8), 0, df(9, 5), 0, df(9, 6)),
  c(df(10, 1) + df(10, 3), df(10, 2) + df(10, 7), df(10, 4) + df(10, 8), df(10, 9), df(10, 5), 0, df(10, 6))
)

expect_equal(stats_decay[, , "inertia"], inertia_decay)