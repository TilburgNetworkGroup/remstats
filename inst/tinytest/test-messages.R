# effects.R ----------------------------------------------------------
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
    tie(variable = "test", scaling = "as.is"),
    pattern = "deprecated"
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

# remstats.R ----------------------------------------------------------
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
    pattern = "Use 'attr_actors'"
)

colnames(info)[1] <- "id"

expect_warning(
    remstats(reh = reh, tie_effects = ~ 1, attributes = info),
    pattern = "use 'name'"
)

colnames(info)[1] <- "name"

expect_warning(
    remstats(edgelist = reh, tie_effects = ~ 1),
    pattern = "Use 'reh'"
)

expect_error(
    remstats(reh = edgelist, sender_effects = ~ 1),
    pattern = "object of class remify"
)

# tomstats.R ----------------------------------------------------------
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
    tomstats(reh = reh, effects = ~ 1, attributes = info),
    pattern = "Use 'attr_actors'"
)

colnames(info)[1] <- "id"

expect_warning(
    tomstats(reh = reh, effects = ~ 1, attributes = info),
    pattern = "use 'name'"
)

colnames(info)[1] <- "name"

expect_warning(
    tomstats(edgelist = reh, effects = ~ 1),
    pattern = "Use 'reh'"
)

expect_error(
    tomstats(reh = edgelist, effects = ~ 1),
    pattern = "object of class remify"
)

reh <- remify::remify(edgelist, model = "actor")

expect_error(
    tomstats(reh = reh, effects = ~ 1),
    pattern = "model argument"
)


reh <- remify::remify(edgelist, model = "tie")

expect_error(
    remstats(reh = reh, tie_effects = ~ 1, start = 0),
    pattern = "1 or a larger"
)

expect_error(
    remstats(reh = reh, tie_effects = ~ 1, start = 5, stop = 3),
    pattern = "cannot be smaller"
)

expect_error(
    remstats(reh = reh, tie_effects = ~ sp()),
    pattern = "directed events"
)

reh <- remify::remify(edgelist, model = "tie", directed = FALSE)

expect_error(
    remstats(reh = reh, tie_effects = ~ outdegreeReceiver()),
    pattern = "undirected events"
)

# process_covariate -----------------------------------------------------
reh <- remify::remify(edgelist, model = "tie")

expect_error(
    remstats(reh = reh, tie_effects = ~ send(variable = "x3"), 
        attr_actors = info),
    pattern = "'x3' not in attr_actors"
)

info$time[1] <- NA

expect_error(
    remstats(reh = reh, tie_effects = ~ send(variable = "x1"), 
        attr_actors = info),
    pattern = "Missing"
)

info$time[1] <- 0
info$x1[1] <- NA

expect_warning(
    remstats(reh = reh, tie_effects = ~ send(variable = "x1"), 
        attr_actors = info),
    pattern = "Missing values"
)

info$x1[1] <- 10

expect_error(
    remstats(reh = reh, tie_effects = ~ send(variable = "x1"), 
        attr_actors = info[1:2,]),
    pattern = "Missing actors"
)

info2 <- rbind(info, c(4, 0, 40, 1))

expect_warning(
    remstats(reh = reh, tie_effects = ~ send(variable = "x1"), 
        attr_actors = info2),
    pattern = "not in the risk set"
)

# Event info
setting <<- c("a", "b", "b", "a")

expect_error(
    remstats(reh = reh, tie_effects = ~ event(x = setting)),
    pattern = "number of events"
)

# userStat
Y <<- matrix(1:15, nrow = 5, ncol = 3)

expect_error(
    remstats(reh = reh, tie_effects = ~ userStat(Y[1:4, ])),
    pattern = "number of events"
)

expect_error(
    remstats(reh = reh, tie_effects = ~ userStat(Y[, 1:2])),
    pattern = "number of dyads"
)

# prep_exo ---------------------------------------------------------------
expect_error(
    send(variable = "x1", attr_actors = attributes),
    pattern = "not in attr_actors"
)

expect_error(
    send(variable = 1, attr_actors = info),
    pattern = "should be a string"
)

expect_error(
    send(variable = "x3", attr_actors = info),
    pattern = "'x3' not in attr_actors"
)

info$time[1] <- NA

expect_error(
    send(variable = "x1", attr_actors = info),
    pattern = "Missing"
)

info$time[1] <- 0
info$x1[1] <- NA

expect_warning(
    send(variable = "x1", attr_actors = info),
    pattern = "Missing values"
)

info$x1[1] <- 10

# parse_tie ---------------------------------------------------------------
X <<- matrix(1:9, 3, 3)
diag(X) <- 0

expect_error(
    remstats(reh = reh, tie_effects = ~ tie(x = X[1:2, ]),
    pattern = "number of actors")
)

expect_error(
    remstats(reh = reh, tie_effects = ~ tie(x = X[, 1:2]),
    pattern = "number of actors")
)

reh <- remify::remify(edgelist, model = "tie", directed = FALSE)

expect_error(
    remstats(reh = reh, tie_effects = ~ tie(x = X),
    pattern = "symmetric")
)

reh <- remify::remify(edgelist, model = "tie")

X[1,1] <- NA

expect_error(
    remstats(reh = reh, tie_effects = ~ tie(x = X),
    pattern = "missing values")
)

# validate_memory -------------------------------------------------------
reh <- remify::remify(edgelist, model = "tie")

expect_error(
    remstats(reh = reh, tie_effects = ~ inertia(), memory = "window"),
    pattern = "memory_value"
)

expect_error(
    remstats(reh = reh, tie_effects = ~ inertia(), memory = "decay"),
    pattern = "memory_value"
)

expect_error(
    remstats(reh = reh, tie_effects = ~ inertia(), memory = "interval", 
        memory_value = 1),
    pattern = "memory_value"
)

expect_error(
    remstats(reh = reh, tie_effects = ~ inertia(), memory = "interval", 
        memory_value = c(5,2)),
    pattern = "memory_value"
)

reh <- remify::remify(edgelist, model = "actor")

expect_error(
    remstats(reh = reh, receiver_effects = ~ inertia(), memory = "window"),
    pattern = "memory_value"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ inertia(), memory = "decay"),
    pattern = "memory_value"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ inertia(), memory = "interval", 
        memory_value = 1),
    pattern = "memory_value"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ inertia(), memory = "interval", 
        memory_value = c(5,2)),
    pattern = "memory_value"
)

# aomstats.R ------------------------------------------------
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
    pattern = "Use 'attr_actors'"
)

colnames(info)[1] <- "id"

expect_warning(
	aomstats(reh = reh, sender_effects = ~ 1 + send(variable = "x1"), attr_actors = info),
	pattern = "Use 'name'"
)

colnames(info)[1] <- "name"

expect_warning(
    aomstats(edgelist = reh, sender_effects = ~ 1),
    pattern = "Use 'reh'"
)

expect_error(
    aomstats(reh = edgelist, sender_effects = ~ 1),
    pattern = "remify object"
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

reh <- remify::remify(edgelist, model = "actor")

expect_error(
    remstats(reh = reh, sender_effects = ~ send(variable = "x3"), 
        attr_actors = info),
    pattern = "'x3' not in attr_actors"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ receive(variable = "x3"), 
        attr_actors = info),
    pattern = "'x3' not in attr_actors"
)

info$time[1] <- NA

expect_error(
    remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
        attr_actors = info),
    pattern = "Missing"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
        attr_actors = info),
    pattern = "Missing"
)

info$time[1] <- 0
info$x1[1] <- NA

expect_warning(
    remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
        attr_actors = info),
    pattern = "Missing values"
)

expect_warning(
    remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
        attr_actors = info),
    pattern = "Missing values"
)

info$x1[1] <- 10

expect_error(
    remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
        attr_actors = info[1:2,]),
    pattern = "Missing actors"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
        attr_actors = info[1:2,]),
    pattern = "Missing actors"
)

info2 <- rbind(info, c(4, 0, 40, 1))

expect_warning(
    remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
        attr_actors = info2),
    pattern = "not in the risk set"
)

expect_warning(
    remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
        attr_actors = info2),
    pattern = "not in the risk set"
)

Y <<- matrix(1:15, nrow = 5, ncol = 3)

expect_error(
    remstats(reh = reh, sender_effects = ~ userStat(Y[, 1:2])),
    pattern = "number of actors"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ userStat(Y[, 1:2])),
    pattern = "number of actors"
)

# check_formula ------------------------------------------------------------
reh <- remify::remify(edgelist, model = "actor")

expect_error(
    remstats(reh = reh, sender_effects = ~ outdegreeSender),
    pattern = "functions"
)

expect_error(
    remstats(reh = reh, receiver_effects = ~ inertia),
    pattern = "functions"
)

reh <- remify::remify(edgelist, model = "tie")

expect_error(
    remstats(reh = reh, tie_effects = ~ inertia),
    pattern = "functions"
)
