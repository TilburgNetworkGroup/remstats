context("actor exogenous stats")

library(remstats)

test_that("covariates don't vary over time", {
	# Specify the effects
	x <- ~ send("extraversion", info[info$time == 0,]) + 
		receive("agreeableness", info[info$time == 0,])

	# Compute the statistics
	out <- remstats(x, edgelist = history)
	stats <- out$statistics

	# Tests
	expect_true(all(stats[,,"send_extraversion"] %in% 
		info$extraversion[info$time == 0]))
	expect_true(all(stats[,,"receive_agreeableness"] %in% 
		info$agreeableness[info$time == 0]))
})

test_that("covariates vary over time", {

	# Specify the effects
	x <- ~ send("extraversion", info) + receive("agreeableness", info)
	
	# Compute statistics
	out <- remstats(x, edgelist = history)
	stats <- out$statistics
	
	# Tests
	expect_true(all(stats[,,"send_extraversion"] %in% info$extraversion))
	expect_true(all(stats[,,"receive_agreeableness"] %in% info$agreeableness))
})