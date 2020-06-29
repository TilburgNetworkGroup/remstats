context("dyad exogenous stats")

library(remstats)

test_that("covariates don't vary over time", {
	# Specify the effects
	form <- ~ same("sex", info[info$time == 0,]) + 
		difference("extraversion", info[info$time == 0,]) +
		average("extraversion", info[info$time == 0,]) +
		minimum("agreeableness", info[info$time == 0,]) +
		maximum("agreeableness", info[info$time == 0,]) +
		equate("age", 0, info[info$time == 0,])
	
	# Compute the statistics
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	
	# Tests
	expect_true(all(stats[,,"same_sex"] %in% c(0,1)))
	temp <- expand.grid(info$extraversion[info$time==0], 
		info$extraversion[info$time==0])
	expect_true(all(stats[,,"difference_extraversion"] %in% 
		abs(temp[,1]-temp[,2])))
	expect_true(all(stats[,,"average_extraversion"] %in% apply(temp, 1, mean)))
	temp <- expand.grid(info$agreeableness[info$time==0], 
		info$agreeableness[info$time==0])
	expect_true(all(stats[,,"minimum_agreeableness"] %in% apply(temp, 1, min)))
	expect_true(all(stats[,,"maximum_agreeableness"] %in% apply(temp, 1, max)))
	expect_true(all(stats[,,"equate_age"] %in% c(0,1)))
})

test_that("covariates vary over time", {
	# Specify the effects
	form <- ~ same("sex", info) + difference("extraversion", info) +
		average("extraversion", info) + minimum("agreeableness", info) +
		maximum("agreeableness", info) + equate("age", 0, info)

	# Compute statistics
	out <- remstats(form, edgelist = history)
	stats <- out$statistics
	
	# Tests
	expect_true(all(stats[,,"same_sex"] %in% c(0,1)))
	temp <- expand.grid(info$extraversion, info$extraversion)
	expect_true(all(stats[,,"difference_extraversion"] %in% 
		abs(temp[,1]-temp[,2])))
	expect_true(all(stats[,,"average_extraversion"] %in% apply(temp, 1, mean)))
	temp <- expand.grid(info$agreeableness, info$agreeableness)
	expect_true(all(stats[,,"minimum_agreeableness"] %in% apply(temp, 1, min)))
	expect_true(all(stats[,,"maximum_agreeableness"] %in% apply(temp, 1, max)))
	expect_true(all(stats[,,"equate_age"] %in% c(0,1)))
})