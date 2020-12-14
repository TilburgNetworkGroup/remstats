context("baseline stats")

library(remstats)

test_that("baseline statistic", {
	# Specify the effect and compute the statistics
	out <- remstats(~ baseline(), edgelist = history)
	stats <- out$statistics

	# Test
	expect_true(all(stats==1))
})

test_that("baselineType statistic", {
	# Specify the effect and compute the statistics
	names(history)[4] <- "type"
	out <- remstats(~ baseline(with_type = TRUE), edgelist = history)
	stats <- out$statistics
	riskset <- out$riskset
	
	expect_true(all(stats[,which(riskset[,3] != 1),]==0))
	expect_true(all(stats[,which(riskset[,3] == 1),]==1))
})
