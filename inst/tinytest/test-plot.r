# Load data
data(history)

# plots for 'tomstats' objects

# Prepare data
reh <- remify2(edgelist = history[,1:3], model = "tie")
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

# TODO(actor-model): # plots for 'aomstats' objects
# TODO(actor-model): 
# TODO(actor-model): # Prepare data
# TODO(actor-model): reh <- remify::remify(edgelist = history[,1:3], model = "actor")
# TODO(actor-model): # Compute effects
# TODO(actor-model): stats <- remstats(reh, sender_effects = ~ outdegreeSender(), receiver_effects = ~ outdegreeReceiver())
# TODO(actor-model): 
# TODO(actor-model): # boxplot.aomstats
# TODO(actor-model): 
# TODO(actor-model): # Plot the 'outdegreeSender' distribution for 20 timepoints
# TODO(actor-model): expect_silent(boxplot(stats, effect = "outdegreeSender", model = "sender"))
# TODO(actor-model): expect_silent(boxplot(stats, effect = 1, model = "sender"))
# TODO(actor-model): expect_silent(boxplot(x = stats, effect = 1, model = "receiver"))
# TODO(actor-model): # Plot the 'inertia' distribution for all 10 actors
# TODO(actor-model): expect_silent(boxplot(stats, effect = "outdegreeSender", model = "sender", by = "actors"))
# TODO(actor-model): 
# TODO(actor-model): # plot.aomstats
# TODO(actor-model): 
# TODO(actor-model): # Plot the 'outdegreeSender' trajectories 5 actors
# TODO(actor-model): expect_silent(plot(stats, effect = "outdegreeSender"))
# TODO(actor-model): expect_silent(plot(stats, effect = 1))
# TODO(actor-model): # Plot the 'outdegreeSender' trajectory for a specific actor
# TODO(actor-model): expect_silent(plot(stats, effect = "outdegreeSender", subset = 10))
# TODO(actor-model): 
