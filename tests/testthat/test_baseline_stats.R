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
	out <- remstats(~ baseline(with_type = TRUE), edgelist = history, 
		with_type = TRUE)
	stats <- out$statistics
	riskset <- out$riskset
	
	expect_true(all(stats[,which(riskset[,3] != "work"),]==0))
	expect_true(all(stats[,which(riskset[,3] == "work"),]==1))
})
