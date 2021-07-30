library(remify)
library(remstats)

test_that("rrankSend", {
	data(history)
	
	effects <- ~ rrankSend()
	tomres <- tomstats(edgelist = history, effects = effects)
	aomres <- aomstats(edgelist = history, receiver_effects = effects)
	
	# The value of the recency statistic is between 0 and 1
	expect_true(all(tomres$statistics[,,2] >= 0) & all(tomres$statistics[,,2] <= 1))
	expect_true(all(aomres$statistics$receiver_stats >= 0) & all(aomres$statistics$receiver_stats <= 1))
	
	# Ranks are smaller than the maximum number of actors
	n <- length(unique(info$id))
	ranks <- 1/tomres$statistics[,,2]
	expect_true(max(tomres$statistics[,,2]) < n)
	ranks <- 1/aomres$statistics$receiver_stats
	expect_true(max(aomres$statistics$receiver_stats) < n)
})

test_that("rrankReceive", {
	data(history)
	
	effects <- ~ rrankReceive()
	tomres <- tomstats(edgelist = history, effects = effects)
	aomres <- aomstats(edgelist = history, receiver_effects = effects)
	
	# The value of the recency statistic is between 0 and 1
	expect_true(all(tomres$statistics[,,2] >= 0) & all(tomres$statistics[,,2] <= 1))
	expect_true(all(aomres$statistics$receiver_stats >= 0) & all(aomres$statistics$receiver_stats <= 1))
	
	# Ranks are smaller than the maximum number of actors
	n <- length(unique(info$id))
	ranks <- 1/tomres$statistics[,,2]
	expect_true(max(tomres$statistics[,,2]) < n)
	ranks <- 1/aomres$statistics$receiver_stats
	expect_true(max(aomres$statistics$receiver_stats) < n)
})
