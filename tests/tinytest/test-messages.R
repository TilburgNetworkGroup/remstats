# from effects.R
expect_warning(
    indegreeSender(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    outdegreeSender(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    totaldegreeSender(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    indegreeReceiver(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    outdegreeReceiver(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    totaldegreeReceiver(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    degreeDiff(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    degreeMax(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    degreeMin(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    totaldegreeDyad(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    inertia(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    reciprocity(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    otp(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    itp(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    osp(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    isp(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    sp(scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    send(variable = "test", scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    receive(variable = "test", scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    average(variable = "test", scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    difference(variable = "test", scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    maximum(variable = "test", scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    minimum(variable = "test", scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    tie(x = 1:3, scaling = "as.is"),
    pattern = "use 'scaling' is 'none'"
)

expect_warning(
    spUnique(),
    pattern = "deprecated"
)

x <<- c(1, 1, NA)
expect_error(
    event(x), 
    pattern = "missing values"
)

Y <<- matrix(1:15, nrow = 5, ncol = 3)
Y[1,1] <- NA
expect_warning(
    userStat(Y), 
    pattern = "missing values"
)

# from remstats.R
edgelist <- data.frame(
  time = 1:5,
  actor1 = c(1, 1, 2, 2, 3),
  actor2 = c(2, 3, 1, 3, 2)
)

info <- data.frame(
  name = 1:3,
  time = rep(0, 3),
  x1 = c(10, 20, 30),
  x2 = c(0, 1, 1)
)

reh <- remify::remify(edgelist, model = "tie")

expect_warning(
    remstats(reh = reh, tie_effects = ~ 1, attributes = info),
    pattern = "use 'attr_data'"
)

colnames(info)[1] <- "id"

expect_warning(
    remstats(reh = reh, tie_effects = ~ 1, attributes = info),
    pattern = "use 'name'"
)

expect_warning(
    remstats(edgelist = reh, tie_effects = ~ 1),
    pattern = "use 'reh'"
)