# Load data
data(history)

# plots for 'tomstats' objects

# Prepare data
reh <- remify::remify(edgelist = history[,1:3], model = "tie")
# Compute effects
stats <- remstats(reh, tie_effects = ~ inertia())

# boxplot.tomstats

# Plot the 'inertia' distribution for 20 timepoints
expect_silent(boxplot(stats, effect = "inertia"))
expect_silent(boxplot(stats, effect = 1))
# Plot the 'inertia' distribution for 20 dyads
expect_silent(boxplot(stats, effect = "inertia", by = "dyads"))
# Plot the 'inertia' distribution for dyads 2:5
expect_silent(boxplot(stats, effect = "inertia", by = "dyads", subset = 2:5))

# plot.tomstats

# Plot the 'inertia' trajectories for 5 dyads
expect_silent(plot(stats, effect = "inertia"))
expect_silent(plot(stats, effect = 1))
# Plot the 'inertia' trajectory for a specific dyad
expect_silent(plot(stats, effect = "inertia", subset = 60))

# plots for 'aomstats' objects

# Prepare data
reh <- remify::remify(edgelist = history[,1:3], model = "actor")
# Compute effects
stats <- remstats(reh, sender_effects = ~ outdegreeSender(), receiver_effects = ~ outdegreeReceiver())

# boxplot.aomstats

# Plot the 'outdegreeSender' distribution for 20 timepoints
expect_silent(boxplot(stats, effect = "outdegreeSender", model = "sender"))
expect_silent(boxplot(stats, effect = 1, model = "sender"))
expect_silent(boxplot(x = stats, effect = 1, model = "receiver"))
# Plot the 'inertia' distribution for all 10 actors
expect_silent(boxplot(stats, effect = "outdegreeSender", model = "sender", by = "actors"))

# plot.aomstats

# Plot the 'outdegreeSender' trajectories 5 actors
expect_silent(plot(stats, effect = "outdegreeSender"))
expect_silent(plot(stats, effect = 1))
# Plot the 'outdegreeSender' trajectory for a specific actor
expect_silent(plot(stats, effect = "outdegreeSender", subset = 10))

