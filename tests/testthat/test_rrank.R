library(remify)
library(remstats)

test_that("rrankSend", {
	data(history)
	
	rehObject <- reh(edgelist = history)
	tomres <- tomstats(edgelist = rehObject, effects = ~ rrankSend())
	aomres <- aomstats(edgelist = rehObject, receiver_effects = ~ rrankSend())
	
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
	edgelist <- tomres$edgelist
	riskset <- tomres$riskset
	rt <- sample(1:nrow(edgelist), 1)
	event <- edgelist[rt-1,]
	stat <- tomres$statistics[,,2]
	expect_equal(stat[rt, which(riskset[,1] == as.character(event[2]) & 
			riskset[,2] == as.character(event[3]))], 1)
})

test_that("rrankReceive", {
	data(history)
	
	rehObject <- reh(edgelist = history)
	tomres <- tomstats(edgelist = rehObject, effects = ~ rrankReceive())
	aomres <- aomstats(edgelist = rehObject, receiver_effects = ~ rrankReceive())
	
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
	edgelist <- tomres$edgelist
	riskset <- tomres$riskset
	rt <- sample(1:nrow(edgelist), 1)
	event <- edgelist[rt-1,]
	stat <- tomres$statistics[,,2]
	expect_equal(stat[rt, which(riskset[,1] == as.character(event[3]) & 
			riskset[,2] == as.character(event[2]))], 1)
})
