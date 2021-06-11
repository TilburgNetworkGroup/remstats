library(remify)
library(remstats)

test_that("rrankSend", {
	data(history)
	
	effects <- ~ rrankSend()
	tomres <- tomstats(edgelist = history, effects = effects)
	aomres <- aomstats(edgelist = history, choiceEffects = effects)
	
	# The value of the recency statistic is between 0 and 1
	expect_true(all(tomres$statistics[,,2] >= 0) & all(tomres$statistics[,,2] <= 1))
	expect_true(all(aomres$statistics$choice >= 0) & all(aomres$statistics$choice <= 1))
	
	# Ranks are smaller than the maximum number of actors
	n <- length(unique(info$id))
	ranks <- 1/tomres$statistics[,,2]
	expect_true(max(tomres$statistics[,,2]) < n)
	ranks <- 1/aomres$statistics$choice
	expect_true(max(aomres$statistics$choice) < n)
})

test_that("rrankReceive", {
	data(history)
	
	effects <- ~ rrankReceive()
	tomres <- tomstats(edgelist = history, effects = effects)
	aomres <- aomstats(edgelist = history, choiceEffects = effects)
	
	# The value of the recency statistic is between 0 and 1
	expect_true(all(tomres$statistics[,,2] >= 0) & all(tomres$statistics[,,2] <= 1))
	expect_true(all(aomres$statistics$choice >= 0) & all(aomres$statistics$choice <= 1))
	
	# Ranks are smaller than the maximum number of actors
	n <- length(unique(info$id))
	ranks <- 1/tomres$statistics[,,2]
	expect_true(max(tomres$statistics[,,2]) < n)
	ranks <- 1/aomres$statistics$choice
	expect_true(max(aomres$statistics$choice) < n)
})
