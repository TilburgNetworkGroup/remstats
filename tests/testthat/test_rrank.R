context("recency ranks")

library(remstats)

test_that("recency ranks", {
	# Specify the effects
	form <- ~ rrankSend() + rrankReceive()
	
	# Compute the statistics
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	
	# Tests
	# The value of the recency statistic is between 0 and 1
	expect_true(all(stats[,,"rrankSend"] >= 0) & 
		all(stats[,,"rrankSend"] <=1))
	expect_true(all(stats[,,"rrankReceive"] >= 0) & 
		all(stats[,,"rrankReceive"] <=1))
	# Ranks are smaller than the maximum number of actors
	n <- length(unique(info$id))
	ranks <- 1/stats[,,"rrankSend"]
	expect_true(max(ranks[ranks!=Inf]) < n)
	ranks <- 1/stats[,,"rrankReceive"]
	expect_true(max(ranks[ranks!=Inf]) < n)
})

test_that("recency ranks with type", {
	# Make sure type is a column in the history
	names(history)[4] <- "type"
	
	# Specify the effects
	form <- ~ rrankSend(with_type = TRUE) + rrankReceive(with_type = TRUE)
	
	# Compute the statistics
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	
	# Tests
	n <- length(unique(info$id))
	ranks <- 1/stats[,,"rrankSend"]
	expect_true(max(ranks[ranks!=Inf]) <= (n-1))
	ranks <- 1/stats[,,"rrankReceive"]
	expect_true(max(ranks[ranks!=Inf]) <= (n-1))
	expect_equal(ncol(stats[,,"rrankSend"]), n*(n-1)*2)
	expect_equal(ncol(stats[,,"rrankReceive"]), n*(n-1)*2)
})


