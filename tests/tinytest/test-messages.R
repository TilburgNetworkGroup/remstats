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

colnames(info)[1] <- "name"

expect_warning(
    remstats(edgelist = reh, tie_effects = ~ 1),
    pattern = "use 'reh'"
)

expect_error(
    remstats(reh = edgelist, sender_effects = ~ 1),
    pattern = "object of class remify"
)

# from attr_data in tomstats
reh <- remify::remify(edgelist, model = "tie")

expect_error(
    remstats(reh = reh, tie_effects = ~ send(variable = "x3"), 
        attr_data = info),
    pattern = "'x3' not in attr_data"
)

expect_error(
    remstats(reh = reh, tie_effects = ~ send(variable = "x1"), 
        attr_data = info[,-2]),
    pattern = "time variable is missing"
)

info$time[1] <- NA

expect_error(
    remstats(reh = reh, tie_effects = ~ send(variable = "x1"), 
        attr_data = info),
    pattern = "missing values"
)

info$time[1] <- 0
info$x1[1] <- NA

expect_warning(
    remstats(reh = reh, tie_effects = ~ send(variable = "x1"), 
        attr_data = info),
    pattern = "Missing values"
)

info$x1[1] <- 10

expect_error(
    remstats(reh = reh, tie_effects = ~ send(variable = "x1"), 
        attr_data = info[1:2,]),
    pattern = "Missing actors"
)

info2 <- rbind(info, c(4, 0, 40, 1))

expect_warning(
    remstats(reh = reh, tie_effects = ~ send(variable = "x1"), 
        attr_data = info2),
    pattern = "not in the risk set"
)

# from userStat in tomstats
Y <<- matrix(1:15, nrow = 5, ncol = 3)

expect_error(
    remstats(reh = reh, tie_effects = ~ userStat(Y[1:4, ])),
    pattern = "number of events"
)

expect_error(
    remstats(reh = reh, tie_effects = ~ userStat(Y[1:4, ])),
    pattern = "number of events"
)

# from aomstats.R
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

reh <- remify::remify(edgelist, model = "actor")

expect_warning(
    aomstats(reh = reh, sender_effects = ~ 1, attributes = info),
    pattern = "use 'attr_data'"
)

colnames(info)[1] <- "id"

expect_warning(
    aomstats(reh = reh, sender_effects = ~ 1, attributes = info),
    pattern = "use 'name'"
)

colnames(info)[1] <- "name"

expect_warning(
    aomstats(edgelist = reh, sender_effects = ~ 1),
    pattern = "use 'reh'"
)

expect_error(
    aomstats(reh = edgelist, sender_effects = ~ 1),
    pattern = "object of class remify"
)

reh <- remify::remify(edgelist, model = "tie")

expect_error(
    aomstats(reh = reh, sender_effects = ~ 1),
    pattern = "model argument"
)

reh <- remify::remify(edgelist, model = "actor")

expect_error(
    aomstats(reh = reh, sender_effects = ~ 1, start = 0),
    pattern = "1 or a larger"
)

expect_error(
    aomstats(reh = reh, sender_effects = ~ 1, start = 5, stop = 3),
    pattern = "cannot be smaller"
)

expect_error(
    remstats(reh = reh, sender_effects = ~ outdegreeReceiver()),
    pattern = "not defined"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ outdegreeSender()),
    pattern = "not defined"
)

# from attr_data in aomstats
reh <- remify::remify(edgelist, model = "actor")

expect_error(
    remstats(reh = reh, sender_effects = ~ send(variable = "x3"), 
        attr_data = info),
    pattern = "'x3' not in attr_data"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ receive(variable = "x3"), 
        attr_data = info),
    pattern = "'x3' not in attr_data"
)

expect_error(
    remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
        attr_data = info[,-2]),
    pattern = "time variable is missing"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
        attr_data = info[,-2]),
    pattern = "time variable is missing"
)

info$time[1] <- NA

expect_error(
    remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
        attr_data = info),
    pattern = "missing values"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
        attr_data = info),
    pattern = "missing values"
)

info$time[1] <- 0
info$x1[1] <- NA

expect_warning(
    remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
        attr_data = info),
    pattern = "Missing values"
)

expect_warning(
    remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
        attr_data = info),
    pattern = "Missing values"
)

info$x1[1] <- 10

expect_error(
    remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
        attr_data = info[1:2,]),
    pattern = "Missing actors"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
        attr_data = info[1:2,]),
    pattern = "Missing actors"
)

info2 <- rbind(info, c(4, 0, 40, 1))

expect_warning(
    remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
        attr_data = info2),
    pattern = "not in the risk set"
)

expect_warning(
    remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
        attr_data = info2),
    pattern = "not in the risk set"
)

# from userStat aomstats
Y <<- matrix(1:15, nrow = 5, ncol = 3)

expect_error(
    remstats(reh = reh, sender_effects = ~ userStat(Y[1:4, ])),
    pattern = "number of events"
)

expect_error(
    remstats(reh = reh, sender_effects = ~ userStat(Y[, 1:2])),
    pattern = "number of actors"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ userStat(Y[1:4, ])),
    pattern = "number of events"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ userStat(Y[, 1:2])),
    pattern = "number of actors"
)