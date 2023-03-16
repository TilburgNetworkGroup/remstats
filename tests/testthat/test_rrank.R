library(remify)
library(remstats)

test_that("rrankSend", {
	data(history)
	
	rehObject <- reh(edgelist = history, model = "tie")
	tomres <- tomstats(reh = rehObject, effects = ~ rrankSend())
	aomres <- aomstats(reh = rehObject, receiver_effects = ~ rrankSend())
	
	# The value of the recency statistic is between 0 and 1
	expect_true(all(tomres$statistics[,,2] >= 0) & all(tomres$statistics[,,2] <= 1))
	expect_true(all(aomres$statistics$receiver_stats >= 0) & all(aomres$statistics$receiver_stats <= 1))
	
	# Ranks are smaller than the maximum number of actors
	n <- length(unique(info$id))
	ranks <- 1/tomres$statistics[,,2]
	expect_true(max(tomres$statistics[,,2]) < n)
	ranks <- 1/aomres$statistics$receiver_stats
	expect_true(max(aomres$statistics$receiver_stats) < n)

	# Randomly select timepoint and check for most recent event
	edgelist <- tomres$reh
	riskset <- tomres$riskset
	rt <- sample(1:nrow(edgelist), 1)
	event <- edgelist[rt-1,]
	stat <- tomres$statistics[,,2]
	expect_equal(stat[rt, event[2] + 1], 1)
})

test_that("rrankReceive", {
	data(history)
	
	rehObject <- reh(edgelist = history, model = "tie")
	tomres <- tomstats(reh = rehObject, effects = ~ rrankReceive())
	aomres <- aomstats(reh = rehObject, receiver_effects = ~ rrankReceive())
	
	# The value of the recency statistic is between 0 and 1
	expect_true(all(tomres$statistics[,,2] >= 0) & all(tomres$statistics[,,2] <= 1))
	expect_true(all(aomres$statistics$receiver_stats >= 0) & all(aomres$statistics$receiver_stats <= 1))
	
	# Ranks are smaller than the maximum number of actors
	n <- length(unique(info$id))
	ranks <- 1/tomres$statistics[,,2]
	expect_true(max(tomres$statistics[,,2]) < n)
	ranks <- 1/aomres$statistics$receiver_stats
	expect_true(max(aomres$statistics$receiver_stats) < n)
	
	# Randomly select timepoint and check for most recent event
	edgelist <- tomres$reh
	riskset <- tomres$riskset
	rt <- sample(1:nrow(edgelist), 1)
	event <- edgelist[rt-1,]
	stat <- tomres$statistics[,,2]
	revent <- which(riskset$receiver == riskset$sender[event[2] + 1] &
			riskset$sender == riskset$receiver[event[2] + 1])
	expect_equal(stat[rt, revent], 1)
})
