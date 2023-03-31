library(remify)

test_that("rrankSend", {
	data(history)
	
	reh_tie <- remify::remify(history, model = "tie")
	reh_actor <- remify::remify(history, model = "actor")
	tomres <- tomstats(reh = reh_tie, effects = ~ rrankSend())
	aomres <- aomstats(reh = reh_actor, receiver_effects = ~ rrankSend())
	
	# The value of the recency statistic is between 0 and 1
	expect_true(all(tomres$statistics[,,2] >= 0) & all(tomres$statistics[,,2] <= 1))
	expect_true(all(aomres$receiver_stats >= 0) & all(aomres$receiver_stats <= 1))
	
	# Ranks are smaller than the maximum number of actors
	n <- length(unique(info$name))
	ranks <- 1/tomres$statistics[,,2]
	expect_true(max(tomres$statistics[,,2]) < n)
	ranks <- 1/aomres$receiver_stats
	expect_true(max(aomres$receiver_stats) < n)

	# Randomly select timepoint and check for most recent event
	edgelist <- reh_tie$edgelist
	riskset <- tomres$riskset
	rt <- sample(1:nrow(edgelist), 1)
	event <- attr(reh_tie, "dyad")[rt-1]
	stat <- tomres$statistics[,,2]
	expect_equal(stat[rt, event], 1)
})

test_that("rrankReceive", {
	data(history)
	
	reh_tie <- remify::remify(history, model = "tie")
	reh_actor <- remify::remify(history, model = "actor")
	tomres <- tomstats(reh = reh_tie, effects = ~ rrankReceive())
	aomres <- aomstats(reh = reh_actor, receiver_effects = ~ rrankReceive())
	
	# The value of the recency statistic is between 0 and 1
	expect_true(all(tomres$statistics[,,2] >= 0) & all(tomres$statistics[,,2] <= 1))
	expect_true(all(aomres$receiver_stats >= 0) & all(aomres$receiver_stats <= 1))
	
	# Ranks are smaller than the maximum number of actors
	n <- length(unique(info$name))
	ranks <- 1/tomres$statistics[,,2]
	expect_true(max(tomres$statistics[,,2]) < n)
	ranks <- 1/aomres$receiver_stats
	expect_true(max(aomres$receiver_stats) < n)
	
	# Randomly select timepoint and check for most recent event
	edgelist <- reh_tie$edgelist
	riskset <- tomres$riskset
	rt <- sample(1:nrow(edgelist), 1)
	event <- attr(reh_tie, "dyad")[rt-1]
	stat <- tomres$statistics[,,2]
	revent <- which(riskset$receiver == riskset$sender[event] &
			riskset$sender == riskset$receiver[event])
	expect_equal(stat[rt, revent], 1)
})
