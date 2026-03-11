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

x <- c(1, 1, NA)
expect_error(
    event(x), 
    pattern = "missing values"
)

Y <- matrix(1:15, nrow = 5, ncol = 3)
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

reh <- remify2(edgelist, model = "tie")

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

reh <- remify2(edgelist, model = "tie")

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

# TODO(actor-model): reh <- remify::remify(edgelist, model = "actor")
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     tomstats(reh = reh, effects = ~ 1),
# TODO(actor-model):     pattern = "model argument"
# TODO(actor-model): )
# TODO(actor-model): 

reh <- remify2(edgelist, model = "tie")

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

reh <- remify2(edgelist, model = "tie", directed = FALSE)

expect_error(
    remstats(reh = reh, tie_effects = ~ outdegreeReceiver()),
    pattern = "undirected events"
)

# process_covariate -----------------------------------------------------
reh <- remify2(edgelist, model = "tie")

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
X <- matrix(1:9, 3, 3)
diag(X) <- 0

expect_error(
    remstats(reh = reh, tie_effects = ~ tie(x = X[1:2, ]),
    pattern = "number of actors")
)

expect_error(
    remstats(reh = reh, tie_effects = ~ tie(x = X[, 1:2]),
    pattern = "number of actors")
)

reh <- remify2(edgelist, model = "tie", directed = FALSE)

expect_error(
    remstats(reh = reh, tie_effects = ~ tie(x = X),
    pattern = "symmetric")
)

reh <- remify2(edgelist, model = "tie")

X[1,1] <- NA

expect_error(
    remstats(reh = reh, tie_effects = ~ tie(x = X),
    pattern = "missing values")
)

# validate_memory -------------------------------------------------------
reh <- remify2(edgelist, model = "tie")

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

# TODO(actor-model): reh <- remify::remify(edgelist, model = "actor")
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, receiver_effects = ~ inertia(), memory = "window"),
# TODO(actor-model):     pattern = "memory_value"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, receiver_effects = ~ inertia(), memory = "decay"),
# TODO(actor-model):     pattern = "memory_value"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, receiver_effects = ~ inertia(), memory = "interval", 
# TODO(actor-model):         memory_value = 1),
# TODO(actor-model):     pattern = "memory_value"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, receiver_effects = ~ inertia(), memory = "interval", 
# TODO(actor-model):         memory_value = c(5,2)),
# TODO(actor-model):     pattern = "memory_value"
# TODO(actor-model): )

# TODO(actor-model): # aomstats.R ------------------------------------------------
# TODO(actor-model): edgelist <- data.frame(
# TODO(actor-model):   time = 1:5,
# TODO(actor-model):   actor1 = c(1, 1, 2, 2, 3),
# TODO(actor-model):   actor2 = c(2, 3, 1, 3, 2)
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): info <- data.frame(
# TODO(actor-model):   name = 1:3,
# TODO(actor-model):   time = rep(0, 3),
# TODO(actor-model):   x1 = c(10, 20, 30),
# TODO(actor-model):   x2 = c(0, 1, 1)
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): reh <- remify::remify(edgelist, model = "actor")
# TODO(actor-model): 
# TODO(actor-model): expect_warning(
# TODO(actor-model):     aomstats(reh = reh, sender_effects = ~ 1, attributes = info),
# TODO(actor-model):     pattern = "Use 'attr_actors'"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): colnames(info)[1] <- "id"
# TODO(actor-model): 
# TODO(actor-model): expect_warning(
# TODO(actor-model): 	aomstats(reh = reh, sender_effects = ~ 1 + send(variable = "x1"), attr_actors = info),
# TODO(actor-model): 	pattern = "Use 'name'"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): colnames(info)[1] <- "name"
# TODO(actor-model): 
# TODO(actor-model): expect_warning(
# TODO(actor-model):     aomstats(edgelist = reh, sender_effects = ~ 1),
# TODO(actor-model):     pattern = "Use 'reh'"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     aomstats(reh = edgelist, sender_effects = ~ 1),
# TODO(actor-model):     pattern = "remify object"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): reh <- remify2(edgelist, model = "tie")
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     aomstats(reh = reh, sender_effects = ~ 1),
# TODO(actor-model):     pattern = "model argument"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): reh <- remify::remify(edgelist, model = "actor")
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     aomstats(reh = reh, sender_effects = ~ 1, start = 0),
# TODO(actor-model):     pattern = "1 or a larger"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     aomstats(reh = reh, sender_effects = ~ 1, start = 5, stop = 3),
# TODO(actor-model):     pattern = "cannot be smaller"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, sender_effects = ~ outdegreeReceiver()),
# TODO(actor-model):     pattern = "not defined"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, receiver_effects = ~ outdegreeSender()),
# TODO(actor-model):     pattern = "not defined"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): reh <- remify::remify(edgelist, model = "actor")
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, sender_effects = ~ send(variable = "x3"), 
# TODO(actor-model):         attr_actors = info),
# TODO(actor-model):     pattern = "'x3' not in attr_actors"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, receiver_effects = ~ receive(variable = "x3"), 
# TODO(actor-model):         attr_actors = info),
# TODO(actor-model):     pattern = "'x3' not in attr_actors"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): info$time[1] <- NA
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
# TODO(actor-model):         attr_actors = info),
# TODO(actor-model):     pattern = "Missing"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
# TODO(actor-model):         attr_actors = info),
# TODO(actor-model):     pattern = "Missing"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): info$time[1] <- 0
# TODO(actor-model): info$x1[1] <- NA
# TODO(actor-model): 
# TODO(actor-model): expect_warning(
# TODO(actor-model):     remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
# TODO(actor-model):         attr_actors = info),
# TODO(actor-model):     pattern = "Missing values"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_warning(
# TODO(actor-model):     remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
# TODO(actor-model):         attr_actors = info),
# TODO(actor-model):     pattern = "Missing values"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): info$x1[1] <- 10
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
# TODO(actor-model):         attr_actors = info[1:2,]),
# TODO(actor-model):     pattern = "Missing actors"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
# TODO(actor-model):         attr_actors = info[1:2,]),
# TODO(actor-model):     pattern = "Missing actors"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): info2 <- rbind(info, c(4, 0, 40, 1))
# TODO(actor-model): 
# TODO(actor-model): expect_warning(
# TODO(actor-model):     remstats(reh = reh, sender_effects = ~ send(variable = "x1"), 
# TODO(actor-model):         attr_actors = info2),
# TODO(actor-model):     pattern = "not in the risk set"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_warning(
# TODO(actor-model):     remstats(reh = reh, receiver_effects = ~ receive(variable = "x1"), 
# TODO(actor-model):         attr_actors = info2),
# TODO(actor-model):     pattern = "not in the risk set"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): # check_formula ------------------------------------------------------------
# TODO(actor-model): reh <- remify::remify(edgelist, model = "actor")
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, sender_effects = ~ outdegreeSender),
# TODO(actor-model):     pattern = "functions"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, receiver_effects = ~ inertia),
# TODO(actor-model):     pattern = "functions"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): reh <- remify2(edgelist, model = "tie")
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     remstats(reh = reh, tie_effects = ~ inertia),
# TODO(actor-model):     pattern = "functions"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): # plot.R ------------------------------------------------
# TODO(actor-model): 
# TODO(actor-model): # Load data
# TODO(actor-model): data(history)
# TODO(actor-model): 
# TODO(actor-model): # Prepare data for 'tomstats'
# TODO(actor-model): reh_tie <- remify2(edgelist = history[,1:3], model = "tie")
# TODO(actor-model): # Compute effects
# TODO(actor-model): stats_tie <- remstats(reh_tie, tie_effects = ~ inertia())
# TODO(actor-model): 
# TODO(actor-model): # Prepare data for 'aomstats'
# TODO(actor-model): reh_actor <- remify::remify(edgelist = history[,1:3], model = "actor")
# TODO(actor-model): # Compute effects
# TODO(actor-model): stats_actor <- remstats(reh_actor, sender_effects = ~ outdegreeSender(), receiver_effects = ~ indegreeReceiver())
# TODO(actor-model): 
# TODO(actor-model): 
# TODO(actor-model): # errors for 'boxplot.tomstats'
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     boxplot(x = stats_tie, effect = TRUE),
# TODO(actor-model):     pattern = "Expected"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     boxplot(x = stats_tie, effect = 3),
# TODO(actor-model):     pattern = "'effect'"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     boxplot(x = stats_tie, effect = "reciprocity"),
# TODO(actor-model):     pattern = "reciprocity"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): 
# TODO(actor-model): # errors for 'boxplot.aomstats'
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     boxplot(x = stats_actor, model = "sender", effect = TRUE),
# TODO(actor-model):     pattern = "Expected"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     boxplot(x = stats_actor, model = "sender", effect = 3),
# TODO(actor-model):     pattern = "'effect'"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     boxplot(x = stats_actor, model = "receiver", effect = 3),
# TODO(actor-model):     pattern = "'effect'"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     boxplot(x = stats_actor, model = "sender", effect = "outdegreeReceiver"),
# TODO(actor-model):     pattern = "outdegreeReceiver"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     boxplot(x = stats_actor, model = "receiver", effect = "outdegreeSender"),
# TODO(actor-model):     pattern = "outdegreeSender"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     boxplot(x = stats_actor, model = "receiver", effect = "indegreeReceiver", by = "actors"),
# TODO(actor-model):     pattern = "'by' = 'actors'"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): # errors for 'plot.aomstats'
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     plot(x = stats_actor, effect = TRUE),
# TODO(actor-model):     pattern = "Expected"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     plot(x = stats_actor, effect = "indegreeReceiver"),
# TODO(actor-model):     pattern = "indegreeReceiver"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     plot(x = stats_actor, effect = 3),
# TODO(actor-model):     pattern = "'effect'"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): # errors for 'plot.tomstats'
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     plot(x = stats_tie, effect = TRUE),
# TODO(actor-model):     pattern = "Expected"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     plot(x = stats_tie, effect = "reciprocity"),
# TODO(actor-model):     pattern = "reciprocity"
# TODO(actor-model): )
# TODO(actor-model): 
# TODO(actor-model): expect_error(
# TODO(actor-model):     plot(x = stats_tie, effect = 3),
# TODO(actor-model):     pattern = "'effect'"
# TODO(actor-model): )